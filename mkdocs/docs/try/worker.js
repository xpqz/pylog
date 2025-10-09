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
let versions = {};

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

        postMessage({ type: 'progress', step: 'loading-release', message: 'Loading PyLog release assets...' });
        console.log('Worker: Loading PyLog release from GitHub API...');

        // Fetch latest release from GitHub API with timeout
        const releaseApiUrl = 'https://api.github.com/repos/xpqz/pylog/releases/latest';
        const releasePromise = fetch(releaseApiUrl);
        const releaseTimeoutPromise = new Promise((_, reject) => {
            setTimeout(() => reject(new Error('Release API timeout after 10 seconds')), 10000);
        });

        const releaseResponse = await Promise.race([releasePromise, releaseTimeoutPromise]);
        if (!releaseResponse.ok) {
            throw new Error(`Failed to load release info: ${releaseResponse.status} - ${releaseResponse.statusText}`);
        }

        const release = await releaseResponse.json();
        postMessage({ type: 'progress', step: 'release-loaded', message: `Release ${release.tag_name} loaded` });
        console.log('Worker: Release loaded:', release.tag_name, release.assets.length, 'assets');

        // Find PyLog and Lark wheels in release assets
        const pylogAsset = release.assets.find(asset =>
            asset.name.includes('pylog') &&
            asset.name.includes('web') &&
            asset.name.endsWith('.whl')
        );
        const larkAsset = release.assets.find(asset =>
            asset.name.includes('lark') &&
            asset.name.endsWith('.whl')
        );

        if (!pylogAsset) {
            throw new Error('PyLog web wheel not found in release assets');
        }
        if (!larkAsset) {
            throw new Error('Lark wheel not found in release assets');
        }

        console.log(`Worker: Found PyLog wheel: ${pylogAsset.name}`);
        console.log(`Worker: Found Lark wheel: ${larkAsset.name}`);

        // Install Lark from GitHub release
        postMessage({ type: 'progress', step: 'installing-lark', message: 'Installing Lark parser from GitHub release...' });
        console.log(`Worker: Installing Lark wheel: ${larkAsset.browser_download_url}`);

        const larkInstallPromise = micropip.install(larkAsset.browser_download_url);
        const larkTimeoutPromise = new Promise((_, reject) => {
            setTimeout(() => reject(new Error('Lark installation timeout after 30 seconds')), 30000);
        });
        await Promise.race([larkInstallPromise, larkTimeoutPromise]);

        // Install PyLog from GitHub release
        const pylogVersion = release.tag_name.replace(/^v/, ''); // Remove 'v' prefix
        versions.pylog = pylogVersion;

        postMessage({ type: 'progress', step: 'installing-pylog', message: `Installing PyLog v${pylogVersion}...` });
        console.log(`Worker: Installing PyLog wheel: ${pylogAsset.browser_download_url} (version: ${pylogVersion})`);

        const pylogInstallPromise = micropip.install(pylogAsset.browser_download_url);
        const pylogTimeoutPromise = new Promise((_, reject) => {
            setTimeout(() => reject(new Error('PyLog installation timeout after 30 seconds')), 30000);
        });
        await Promise.race([pylogInstallPromise, pylogTimeoutPromise]);

        postMessage({ type: 'progress', step: 'importing-pylog', message: 'Importing PyLog modules...' });
        console.log('Worker: Importing PyLog modules...');
        const prolog = pyodide.pyimport('prolog');
        pylogEngineClass = prolog.engine.Engine;  // Keep reference to Engine class
        pylogProgram = prolog.ast.clauses.Program;  // Correct path: ast.clauses.Program
        pylogPretty = prolog.ast.pretty.pretty;
        parseQuery = prolog.parser.parser.parse_query;  // Correct path: parser.parser.parse_query

        postMessage({ type: 'progress', step: 'initializing-engine', message: 'Creating Prolog engine...' });
        console.log('Worker: PyLog successfully initialized');

        // Create initial engine with empty program
        await resetEngine();

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
 * Reset the engine to fresh state with optional max_steps limit
 */
async function resetEngine(maxSteps = null) {
    try {
        console.log('Worker: Resetting engine...');

        // Create fresh engine with empty program using Python Engine class
        const emptyProgram = pylogProgram([]);

        // Set max_steps at engine creation if provided
        // Engine(program, occurs_check=False, max_solutions=None, trace=False, max_steps=None, ...)
        if (maxSteps !== null) {
            pylogEngine = pylogEngineClass(emptyProgram, false, null, false, maxSteps);
        } else {
            pylogEngine = pylogEngineClass(emptyProgram);
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
        await resetEngine(maxSteps);

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
                await resetEngine();
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