/**
 * PyLog REPL - Web Worker (Pyodide Bootstrap)
 *
 * Runs Pyodide and PyLog in a Web Worker for UI isolation.
 * Handles initialization, query execution, and communication with main thread.
 */

// Worker state
let pyodide = null;
let pylogEngine = null;
let pylogEngineClass = null;  // Keep reference to Python Engine class
let pylogProgram = null;
let pylogPretty = null;
let parseQuery = null;
let standardLibraryClauses = [];  // Keep reference to standard library clauses
let versions = {};
let currentCommitSha = null;

/**
 * Get the current commit SHA, preferring manifest over GitHub API to reduce dependencies
 */
async function getCurrentCommitSha(existingManifest = null) {
    // If we already have a manifest with commit_sha, use that first
    if (existingManifest && existingManifest.commit_sha) {
        console.log(`Worker: Using commit SHA from existing manifest: ${existingManifest.commit_sha}`);
        return existingManifest.commit_sha;
    }

    try {
        console.log('Worker: Fetching current commit SHA from GitHub API...');

        const apiUrl = 'https://api.github.com/repos/xpqz/pylog/commits/main';
        const response = await fetch(apiUrl);

        if (!response.ok) {
            throw new Error(`GitHub API request failed: ${response.status} - ${response.statusText}`);
        }

        const data = await response.json();
        const sha = data.sha;

        console.log(`Worker: Current commit SHA from API: ${sha}`);
        return sha;

    } catch (error) {
        console.warn('Worker: Failed to fetch commit SHA from GitHub API:', error);
        console.warn('Worker: This may be due to rate limiting, ad blockers, or network issues');
        return null;
    }
}

/**
 * Fetch manifest with fallback to raw.githubusercontent.com
 */
async function fetchManifestWithFallback(commitRef) {
    const manifestUrls = [
        `https://cdn.jsdelivr.net/gh/xpqz/pylog@${commitRef}/mkdocs/docs/try/assets/manifest.json?_cb=${Date.now()}`,
        `https://raw.githubusercontent.com/xpqz/pylog/${commitRef}/mkdocs/docs/try/assets/manifest.json?_cb=${Date.now()}`
    ];

    for (let i = 0; i < manifestUrls.length; i++) {
        const url = manifestUrls[i];
        const source = i === 0 ? 'JsDelivr CDN' : 'GitHub Raw';

        try {
            console.log(`Worker: Trying manifest from ${source}: ${url}`);

            const response = await Promise.race([
                fetch(url),
                new Promise((_, reject) =>
                    setTimeout(() => reject(new Error('Manifest fetch timeout after 10 seconds')), 10000)
                )
            ]);

            if (!response.ok) {
                throw new Error(`${source} request failed: ${response.status} - ${response.statusText}`);
            }

            const manifest = await response.json();
            console.log(`Worker: ✅ Manifest loaded successfully from ${source}`);
            return manifest;

        } catch (error) {
            console.warn(`Worker: ⚠️ Failed to load manifest from ${source}:`, error);
            if (i === manifestUrls.length - 1) {
                throw new Error(`Failed to load manifest from all sources. Last error: ${error.message}`);
            }
        }
    }
}

/**
 * Extract error information for enhanced error reporting
 */
function extractErrorInfo(error, query) {
    try {
        // Convert error to string to analyze
        const errorString = String(error);

        // Check if this is a ReaderError by examining the error message
        if (errorString.includes('ReaderError')) {
            // Try to extract ReaderError information from Python error
            const readerErrorData = extractReaderErrorInfo(error, errorString, query);
            if (readerErrorData) {
                return readerErrorData;
            }
        }

        // Default error format
        return {
            message: errorString,
            errorType: 'GenericError'
        };
    } catch (extractError) {
        console.warn('Worker: Error while extracting error info:', extractError);
        return {
            message: String(error),
            errorType: 'GenericError'
        };
    }
}

/**
 * Extract ReaderError specific information
 */
function extractReaderErrorInfo(pyError, errorString, query) {
    try {
        // If we have access to the Python error object, try to extract attributes
        if (pyError && typeof pyError === 'object') {
            // Try to access Python error attributes
            let position = null;
            let token = null;
            let message = null;

            try {
                // Try to get attributes from Python error object
                if (pyError.position !== undefined) {
                    position = pyError.position;
                }
                if (pyError.token !== undefined) {
                    token = pyError.token;
                }
                if (pyError.message !== undefined) {
                    message = pyError.message;
                }
            } catch (attrError) {
                // Python attribute access failed, try string parsing
                console.log('Worker: Direct attribute access failed, trying string parsing');
            }

            // If we got position information, return structured error
            if (position !== null || message || token) {
                return {
                    message: message || extractMessageFromString(errorString),
                    errorType: 'ReaderError',
                    position: position,
                    token: token,
                    query: query
                };
            }
        }

        // Fallback: try to parse position from error string
        const positionMatch = errorString.match(/at position (\d+)/);
        if (positionMatch) {
            const position = parseInt(positionMatch[1]);
            const message = extractMessageFromString(errorString);

            return {
                message: message,
                errorType: 'ReaderError',
                position: position,
                token: null,
                query: query
            };
        }

        return null;
    } catch (parseError) {
        console.warn('Worker: Failed to parse ReaderError:', parseError);
        return null;
    }
}

/**
 * Extract error message from error string
 */
function extractMessageFromString(errorString) {
    // Remove "ReaderError at position X: " prefix if present
    const cleanMessage = errorString.replace(/^.*?ReaderError(?:\s+at\s+position\s+\d+)?\s*:\s*/, '');

    // Remove any remaining prefixes
    return cleanMessage.replace(/^(Prolog error: )?/, '');
}

/**
 * Initialize Pyodide and PyLog
 */
async function initializePyodide() {
    try {
        // Send progress updates to UI
        postMessage({ type: 'progress', step: 'loading-pyodide', message: 'Loading Pyodide from CDN...' });
        console.log('Worker: Loading Pyodide...');

        // Load Pyodide from CDN with timeout
        const loadPromise = new Promise(async (resolve, reject) => {
            try {
                importScripts('https://cdn.jsdelivr.net/pyodide/v0.24.1/full/pyodide.js');
                const pyodideInstance = await loadPyodide({
                    indexURL: 'https://cdn.jsdelivr.net/pyodide/v0.24.1/full/'
                });
                resolve(pyodideInstance);
            } catch (error) {
                reject(error);
            }
        });

        const timeoutPromise = new Promise((_, reject) => {
            setTimeout(() => reject(new Error('Pyodide loading timeout after 60 seconds')), 60000);
        });

        pyodide = await Promise.race([loadPromise, timeoutPromise]);

        postMessage({ type: 'progress', step: 'pyodide-loaded', message: 'Pyodide loaded successfully' });
        console.log('Worker: Pyodide loaded, version:', pyodide.version);
        versions.pyodide = pyodide.version;

        // Install micropip
        postMessage({ type: 'progress', step: 'loading-micropip', message: 'Loading package manager...' });
        await pyodide.loadPackage('micropip');
        const micropip = pyodide.pyimport('micropip');

        postMessage({ type: 'progress', step: 'loading-manifest', message: 'Loading PyLog wheel manifest...' });
        console.log('Worker: Loading wheel manifest with fallback strategy...');

        // Strategy: Try @main first to get current manifest, then use its commit_sha for subsequent loads
        let manifest;
        let commitRef = 'main';

        try {
            // First, try to load manifest from @main to get the latest commit_sha
            console.log('Worker: Attempting initial manifest load from @main to get current commit SHA');
            manifest = await fetchManifestWithFallback('main');

            // If manifest has commit_sha, use that for better cache busting
            if (manifest.commit_sha && manifest.commit_sha !== 'main') {
                console.log(`Worker: Found commit SHA in manifest: ${manifest.commit_sha}`);
                commitRef = manifest.commit_sha;
                currentCommitSha = manifest.commit_sha;

                // Reload manifest using the commit SHA for guaranteed freshness
                console.log(`Worker: Reloading manifest with commit SHA: ${commitRef}`);
                manifest = await fetchManifestWithFallback(commitRef);
            } else {
                // Fallback: Try to get commit SHA from GitHub API
                console.log('Worker: Manifest missing commit_sha, falling back to GitHub API');
                currentCommitSha = await getCurrentCommitSha();
                commitRef = currentCommitSha || 'main';

                if (currentCommitSha && currentCommitSha !== 'main') {
                    console.log(`Worker: Reloading manifest with API-provided commit SHA: ${commitRef}`);
                    manifest = await fetchManifestWithFallback(commitRef);
                }
            }
        } catch (error) {
            console.error('Worker: All manifest loading strategies failed:', error);
            throw new Error(`Failed to load wheel manifest: ${error.message}`);
        }

        console.log(`Worker: Using final commit reference: ${commitRef}`);
        postMessage({ type: 'progress', step: 'manifest-loaded', message: `Manifest v${manifest.version} loaded` });
        console.log('Worker: Manifest loaded:', manifest.version, 'from', manifest.generated_at);

        // Log commit information for debugging
        if (manifest.commit_sha) {
            console.log(`Worker: Manifest commit SHA: ${manifest.commit_sha}`);
            versions.commit_sha = manifest.commit_sha;
        }
        if (currentCommitSha) {
            console.log(`Worker: Current branch commit SHA: ${currentCommitSha}`);
            versions.current_commit_sha = currentCommitSha;
        }

        if (!manifest.pylog || !manifest.pylog.url) {
            throw new Error('PyLog wheel URL not found in manifest');
        }
        if (!manifest.lark || !manifest.lark.url) {
            throw new Error('Lark wheel URL not found in manifest');
        }

        console.log(`Worker: Found PyLog wheel: ${manifest.pylog.wheel}`);
        console.log(`Worker: PyLog URL: ${manifest.pylog.url}`);
        console.log(`Worker: Found Lark wheel: ${manifest.lark.wheel}`);
        console.log(`Worker: Lark URL: ${manifest.lark.url}`);

        // Install Lark with fallback strategy
        postMessage({ type: 'progress', step: 'installing-lark', message: 'Installing Lark parser from CDN...' });
        console.log(`Worker: Installing Lark wheel: ${manifest.lark.url}`);

        try {
            const larkInstallPromise = micropip.install(manifest.lark.url);
            const larkTimeoutPromise = new Promise((_, reject) => {
                setTimeout(() => reject(new Error('Lark installation timeout after 30 seconds')), 30000);
            });
            await Promise.race([larkInstallPromise, larkTimeoutPromise]);
            console.log('Worker: ✅ Lark installation completed successfully');
        } catch (error) {
            console.warn('Worker: ⚠️ Primary Lark install failed, trying fallback URL:', error);
            // Create fallback URL using raw.githubusercontent.com
            const fallbackLarkUrl = manifest.lark.url.replace(
                'cdn.jsdelivr.net/gh/xpqz/pylog@',
                'raw.githubusercontent.com/xpqz/pylog/'
            );
            console.log(`Worker: Trying Lark fallback: ${fallbackLarkUrl}`);

            const fallbackInstallPromise = micropip.install(fallbackLarkUrl);
            const fallbackTimeoutPromise = new Promise((_, reject) => {
                setTimeout(() => reject(new Error('Lark fallback installation timeout after 30 seconds')), 30000);
            });
            await Promise.race([fallbackInstallPromise, fallbackTimeoutPromise]);
            console.log('Worker: ✅ Lark installation completed successfully via fallback');
        }

        // Install PyLog with fallback strategy
        versions.pylog = manifest.pylog.version;

        postMessage({ type: 'progress', step: 'installing-pylog', message: `Installing PyLog v${manifest.pylog.version} from CDN...` });
        console.log(`Worker: Installing PyLog wheel: ${manifest.pylog.url}`);

        try {
            const pylogInstallPromise = micropip.install(manifest.pylog.url);
            const pylogTimeoutPromise = new Promise((_, reject) => {
                setTimeout(() => reject(new Error('PyLog installation timeout after 30 seconds')), 30000);
            });
            await Promise.race([pylogInstallPromise, pylogTimeoutPromise]);
            console.log('Worker: ✅ PyLog installation completed successfully');
        } catch (error) {
            console.warn('Worker: ⚠️ Primary PyLog install failed, trying fallback URL:', error);
            // Create fallback URL using raw.githubusercontent.com
            const fallbackPylogUrl = manifest.pylog.url.replace(
                'cdn.jsdelivr.net/gh/xpqz/pylog@',
                'raw.githubusercontent.com/xpqz/pylog/'
            );
            console.log(`Worker: Trying PyLog fallback: ${fallbackPylogUrl}`);

            const fallbackInstallPromise = micropip.install(fallbackPylogUrl);
            const fallbackTimeoutPromise = new Promise((_, reject) => {
                setTimeout(() => reject(new Error('PyLog fallback installation timeout after 30 seconds')), 30000);
            });
            await Promise.race([fallbackInstallPromise, fallbackTimeoutPromise]);
            console.log('Worker: ✅ PyLog installation completed successfully via fallback');
        }

        postMessage({ type: 'progress', step: 'importing-pylog', message: 'Importing PyLog modules...' });
        console.log('Worker: Importing PyLog modules...');

        // Try importing modules directly since __init__.py files are empty
        console.log('Worker: Attempting direct module imports...');

        try {
            const engineModule = pyodide.pyimport('prolog.engine.engine');
            console.log('Worker: Engine module keys:', Object.keys(engineModule));
            pylogEngineClass = engineModule.Engine;
            console.log('Worker: ✅ Engine class imported successfully');
        } catch (error) {
            console.error('Worker: ❌ Failed to import engine:', error);
            throw new Error(`Failed to import PyLog engine: ${error.message}`);
        }

        try {
            const clausesModule = pyodide.pyimport('prolog.ast.clauses');
            pylogProgram = clausesModule.Program;
            console.log('Worker: ✅ Program class imported successfully');
        } catch (error) {
            console.error('Worker: ❌ Failed to import clauses:', error);
            throw new Error(`Failed to import PyLog clauses: ${error.message}`);
        }

        try {
            const prettyModule = pyodide.pyimport('prolog.ast.pretty');
            pylogPretty = prettyModule.pretty;
            console.log('Worker: ✅ Pretty printer imported successfully');
        } catch (error) {
            console.error('Worker: ❌ Failed to import pretty:', error);
            throw new Error(`Failed to import PyLog pretty: ${error.message}`);
        }

        try {
            const parserModule = pyodide.pyimport('prolog.parser.parser');
            parseQuery = parserModule.parse_query;
            console.log('Worker: ✅ Parser imported successfully');
        } catch (error) {
            console.error('Worker: ❌ Failed to import parser:', error);
            throw new Error(`Failed to import PyLog parser: ${error.message}`);
        }

        postMessage({ type: 'progress', step: 'loading-stdlib', message: 'Loading standard library...' });
        console.log('Worker: Loading standard library...');

        // Load standard library using importlib.resources
        standardLibraryClauses = [];  // Reset global variable
        try {
            // Use Python's importlib.resources to read the stdlib file from the package
            const pythonCode = `
import importlib.resources as resources

# Read lists.pl from the package
try:
    # Python 3.9+ way
    files = resources.files('prolog.lib')
    lists_file = files / 'lists.pl'
    stdlib_content = lists_file.read_text()
except AttributeError:
    # Fallback for older Python
    with resources.open_text('prolog.lib', 'lists.pl') as f:
        stdlib_content = f.read()

stdlib_content
            `;

            const libContent = pyodide.runPython(pythonCode);
            console.log(`Worker: Standard library content length: ${libContent.length} characters`);

            // Import the parser to parse the standard library
            const parserPackage = pyodide.pyimport('prolog.parser.parser');

            // Parse the standard library
            standardLibraryClauses = parserPackage.parse_program(libContent);
            console.log(`Worker: ✅ Standard library loaded: ${standardLibraryClauses.length} clauses`);

        } catch (error) {
            console.error('Worker: ⚠️ Failed to load standard library:', error);
            console.error('Worker: Continuing with empty program...');
        }

        postMessage({ type: 'progress', step: 'initializing-engine', message: 'Creating Prolog engine...' });
        console.log('Worker: Creating engine with standard library...');

        // Create initial engine with standard library
        await resetEngine(null, standardLibraryClauses);

        postMessage({ type: 'progress', step: 'ready', message: 'PyLog REPL ready!' });
        postMessage({
            type: 'initialized',
            versions: versions
        });

    } catch (error) {
        console.error('Worker: Failed to initialize:', error);
        const errorMessage = error.message || 'Unknown initialization error';
        const detailedMessage = error.stack ? `${errorMessage}\n\nDetails: ${error.stack}` : errorMessage;

        postMessage({
            type: 'error',
            message: errorMessage,
            details: detailedMessage,
            step: 'initialization-failed'
        });
    }
}

/**
 * Reset the engine to fresh state with optional max_steps limit and standard library
 */
async function resetEngine(maxSteps = null, standardLibraryClauses = []) {
    try {
        console.log('Worker: Resetting engine...');

        // Create program with standard library clauses
        const program = pylogProgram(standardLibraryClauses);
        console.log(`Worker: Created program with ${standardLibraryClauses.length} clauses`);

        // Set max_steps at engine creation if provided
        // Engine(program, occurs_check=False, max_solutions=None, trace=False, max_steps=None, ...)
        if (maxSteps !== null) {
            pylogEngine = pylogEngineClass(program, false, null, false, maxSteps);
        } else {
            pylogEngine = pylogEngineClass(program);
        }

        console.log('Worker: Engine reset complete');

    } catch (error) {
        console.error('Worker: Engine reset failed:', error);
        throw error;
    }
}

/**
 * Execute a Prolog query with limits and batched results
 */
async function executeQuery(query, options = {}) {
    try {
        if (!pylogEngine || !parseQuery) {
            throw new Error('PyLog not initialized');
        }

        const maxSteps = options.maxSteps || 1000;
        const maxSolutions = options.maxSolutions || 10;

        console.log(`Worker: Executing query: ${query} (max_steps: ${maxSteps}, max_solutions: ${maxSolutions})`);

        // Reset engine with maxSteps limit if different from current
        await resetEngine(maxSteps, standardLibraryClauses);

        // Parse the query
        const goals = parseQuery(`?- ${query}.`);

        // Execute with max_solutions enforced by engine
        const results = [];

        try {
            const solutions = pylogEngine.run(goals, maxSolutions);

            for (const solution of solutions) {
                // Keep all solutions including empty ones (for "true" results)
                if (solution !== null && solution !== undefined) {
                    // Pretty print the solution with operators
                    const prettySolution = {};
                    for (const [key, value] of Object.entries(solution)) {
                        prettySolution[key] = pylogPretty(value, true); // operator_mode=True
                    }
                    results.push(prettySolution);
                }
            }
        } catch (pyError) {
            // Handle Prolog execution errors
            console.error('Worker: Prolog execution error:', pyError);
            const errorData = extractErrorInfo(pyError, query);
            postMessage({
                type: 'error',
                query: query,
                ...errorData
            });
            return;
        }

        // Send batched results (using 'solutions' to match UI expectation)
        postMessage({
            type: 'solutions',
            query: query,
            solutions: results,
            stepCount: pylogEngine._steps_taken || 0,  // Actual steps from engine
            solutionCount: results.length,
            limits: {
                maxSteps: maxSteps,
                maxSolutions: maxSolutions
            }
        });

    } catch (error) {
        console.error('Worker: Query execution failed:', error);
        const errorData = extractErrorInfo(error, query);
        postMessage({
            type: 'error',
            query: query,
            ...errorData
        });
    }
}

/**
 * Handle messages from main thread
 */
self.onmessage = async function(event) {
    const { type, data } = event.data;

    switch (type) {
        case 'init':
            await initializePyodide();
            break;

        case 'query':
            if (!data || !data.query) {
                postMessage({
                    type: 'error',
                    message: 'Query required for query command'
                });
                break;
            }
            await executeQuery(data.query, data.options || {});
            break;

        case 'reset':
            try {
                await resetEngine(null, standardLibraryClauses);
                postMessage({
                    type: 'reset',
                    message: 'Engine reset successful'
                });
            } catch (error) {
                postMessage({
                    type: 'error',
                    message: `Reset failed: ${error.message}`
                });
            }
            break;

        case 'stop':
            console.log('Worker: Stop requested');
            // Note: Actual termination would need more sophisticated handling
            // For now, we just acknowledge the stop request
            postMessage({ type: 'stopped' });
            break;

        default:
            console.warn('Worker: Unknown message type:', type);
            postMessage({
                type: 'error',
                message: `Unknown message type: ${type}`
            });
    }
};

/**
 * Handle worker errors
 */
self.onerror = function(error) {
    console.error('Worker: Unhandled error:', error);
    postMessage({
        type: 'error',
        message: error.message || 'Unknown worker error'
    });
};

// Log worker startup
console.log('Worker: PyLog REPL worker initialized');