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
 * Initialize Pyodide and PyLog
 */
async function initializePyodide() {
    try {
        console.log('Worker: Loading Pyodide...');

        // Load Pyodide from CDN
        importScripts('https://cdn.jsdelivr.net/pyodide/v0.24.1/full/pyodide.js');
        pyodide = await loadPyodide({
            indexURL: 'https://cdn.jsdelivr.net/pyodide/v0.24.1/full/'
        });

        console.log('Worker: Pyodide loaded, version:', pyodide.version);
        versions.pyodide = pyodide.version;

        // Install micropip
        await pyodide.loadPackage('micropip');
        const micropip = pyodide.pyimport('micropip');

        console.log('Worker: Installing lark dependency...');
        await micropip.install('lark>=1.1.0');

        console.log('Worker: Installing PyLog wheel...');
        // Look for the web wheel in the assets directory
        const wheelResponse = await fetch('./assets/');
        const wheelText = await wheelResponse.text();
        const wheelMatch = wheelText.match(/pylog-([^-]+)-py3-none-any\.whl/);

        if (!wheelMatch) {
            throw new Error('PyLog web wheel not found in assets directory');
        }

        const wheelName = wheelMatch[0];
        const version = wheelMatch[1];
        console.log(`Worker: Found PyLog wheel: ${wheelName}, version: ${version}`);
        versions.pylog = version;

        await micropip.install(`./assets/${wheelName}`);

        console.log('Worker: Importing PyLog modules...');
        const prolog = pyodide.pyimport('prolog');
        pylogEngineClass = prolog.engine.Engine;  // Keep reference to Engine class
        pylogProgram = prolog.engine.Program;
        pylogPretty = prolog.ast.pretty.pretty;
        parseQuery = prolog.parser.reader.parse_query;

        console.log('Worker: PyLog successfully initialized');

        // Create initial engine with empty program
        await resetEngine();

        postMessage({
            type: 'initialized',
            versions: versions
        });

    } catch (error) {
        console.error('Worker: Failed to initialize:', error);
        postMessage({ type: 'error', message: error.message });
    }
}

/**
 * Reset the engine to fresh state
 */
async function resetEngine() {
    try {
        console.log('Worker: Resetting engine...');

        // Create fresh engine with empty program using Python Engine class
        const emptyProgram = pylogProgram([]);
        pylogEngine = pylogEngineClass(emptyProgram);

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

        // Parse the query
        const goals = parseQuery(`?- ${query}.`);

        // Execute with limits
        const results = [];
        let stepCount = 0;
        let solutionCount = 0;

        try {
            for (const solution of pylogEngine.run(goals)) {
                stepCount++;

                if (stepCount > maxSteps) {
                    console.log('Worker: Max steps exceeded');
                    break;
                }

                if (solution && Object.keys(solution).length > 0) {
                    // Pretty print the solution with operators
                    const prettySolution = {};
                    for (const [key, value] of Object.entries(solution)) {
                        prettySolution[key] = pylogPretty(value, true); // operator_mode=True
                    }
                    results.push(prettySolution);
                    solutionCount++;

                    if (solutionCount >= maxSolutions) {
                        console.log('Worker: Max solutions reached');
                        break;
                    }
                }
            }
        } catch (pyError) {
            // Handle Prolog execution errors
            console.error('Worker: Prolog execution error:', pyError);
            postMessage({
                type: 'error',
                query: query,
                message: `Prolog error: ${pyError.message || pyError}`
            });
            return;
        }

        // Send batched results
        postMessage({
            type: 'results',
            query: query,
            solutions: results,
            stepCount: stepCount,
            solutionCount: solutionCount,
            limits: {
                maxSteps: maxSteps,
                maxSolutions: maxSolutions
            }
        });

    } catch (error) {
        console.error('Worker: Query execution failed:', error);
        postMessage({
            type: 'error',
            query: query,
            message: error.message
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

        case 'run':
            if (!data || !data.query) {
                postMessage({
                    type: 'error',
                    message: 'Query required for run command'
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