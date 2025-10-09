/**
 * PyLog REPL - UI Glue (Main Thread)
 *
 * Manages the REPL interface, handles user input, displays output,
 * and communicates with the Web Worker running Pyodide.
 */

// REPL State
let replWorker = null;
let replState = {
    initialized: false,
    running: false,
    history: [],
    historyIndex: -1
};

// Safety configuration
let safetyConfig = {
    maxSteps: 100000,      // ~100k steps for production
    maxSolutions: 100,     // ~100 solutions max
    timeoutMs: 10000,      // 10 seconds wall-clock timeout
    configurable: true     // Allow runtime configuration
};

// Timeout tracking
let queryTimeoutId = null;

// DOM Elements
let containerEl = null;
let loadingEl = null;
let replEl = null;
let outputEl = null;
let inputEl = null;

/**
 * Get current safety limits configuration
 */
function getSafetyLimits() {
    return {
        maxSteps: safetyConfig.maxSteps,
        maxSolutions: safetyConfig.maxSolutions,
        timeoutMs: safetyConfig.timeoutMs
    };
}

/**
 * Update safety configuration (if configurable)
 */
function updateSafetyConfig(newConfig) {
    if (!safetyConfig.configurable) {
        console.warn('Safety configuration is locked');
        return false;
    }

    // Validate and apply safe limits
    if (newConfig.maxSteps && newConfig.maxSteps > 0 && newConfig.maxSteps <= 1000000) {
        safetyConfig.maxSteps = newConfig.maxSteps;
    }
    if (newConfig.maxSolutions && newConfig.maxSolutions > 0 && newConfig.maxSolutions <= 1000) {
        safetyConfig.maxSolutions = newConfig.maxSolutions;
    }
    if (newConfig.timeoutMs && newConfig.timeoutMs > 0 && newConfig.timeoutMs <= 60000) {
        safetyConfig.timeoutMs = newConfig.timeoutMs;
    }

    console.log('Safety configuration updated:', getSafetyLimits());
    return true;
}

/**
 * Clear active query timeout
 */
function clearQueryTimeout() {
    if (queryTimeoutId) {
        clearTimeout(queryTimeoutId);
        queryTimeoutId = null;
    }
}

/**
 * Handle query timeout
 */
function handleQueryTimeout() {
    console.warn('Query timeout reached, terminating worker...');
    appendOutput(`Query timed out after ${safetyConfig.timeoutMs / 1000}s. Worker terminated.`, 'error');

    // Terminate worker to stop the query
    if (replWorker) {
        replWorker.terminate();
        replWorker = null;
        replState.initialized = false;
    }

    // Reset UI state
    setRunning(false);
    queryTimeoutId = null;

    // Show reinitialize message
    showLoading('Query timed out. Click "Start REPL" to reinitialize.');
}

/**
 * Initialize the REPL interface
 */
function initializeREPL() {
    console.log('PyLog REPL: Initializing UI...');

    // Get DOM elements
    containerEl = document.getElementById('pylog-repl-container');
    loadingEl = document.getElementById('pylog-loading');
    replEl = document.getElementById('pylog-repl');

    if (!containerEl) {
        console.error('PyLog REPL: Container element not found');
        return;
    }

    // Create REPL UI
    createREPLInterface();

    // Add start button to loading screen
    addStartButton();

    console.log('PyLog REPL: UI initialized');
}

/**
 * Add start button to loading screen
 */
function addStartButton() {
    const startButton = document.createElement('button');
    startButton.textContent = 'Start REPL';
    startButton.style.cssText = `
        padding: 10px 20px;
        margin: 10px;
        background: #1976d2;
        color: white;
        border: none;
        border-radius: 4px;
        cursor: pointer;
        font-size: 16px;
    `;

    startButton.onclick = startREPL;
    loadingEl.appendChild(startButton);
}

/**
 * Create the REPL interface elements
 */
function createREPLInterface() {
    replEl.innerHTML = `
        <div id="pylog-output" style="
            background: #f5f5f5;
            border: 1px solid #ddd;
            padding: 10px;
            height: 300px;
            overflow-y: auto;
            font-family: 'Courier New', monospace;
            margin-bottom: 10px;
        ">
            <div style="color: #666;">
                PyLog REPL ready. Type queries and press Ctrl+Enter (Cmd+Enter on Mac).
                <br>Examples: X = 42  |  member(X, [1,2,3])  |  help  |  limits
                <br>Safety: 100k steps, 100 solutions, 10s timeout
            </div>
        </div>

        <div style="display: flex; gap: 10px; align-items: flex-start;">
            <textarea id="pylog-input" placeholder="?- " rows="2" style="
                flex: 1;
                padding: 8px;
                border: 1px solid #ddd;
                border-radius: 4px;
                font-family: 'Courier New', monospace;
                resize: vertical;
                min-height: 40px;
            "></textarea>
            <div style="display: flex; flex-direction: column; gap: 8px;">
                <button id="pylog-run" style="
                    padding: 8px 16px;
                    background: #4caf50;
                    color: white;
                    border: none;
                    border-radius: 4px;
                    cursor: pointer;
                    min-width: 60px;
                ">Run</button>
                <button id="pylog-stop" style="
                    padding: 8px 16px;
                    background: #f44336;
                    color: white;
                    border: none;
                    border-radius: 4px;
                    cursor: pointer;
                    min-width: 60px;
                " disabled>Stop</button>
            </div>
        </div>
    `;

    // Get references to created elements
    outputEl = document.getElementById('pylog-output');
    inputEl = document.getElementById('pylog-input');

    // Add event listeners
    setupEventListeners();
}

/**
 * Setup event listeners for REPL interaction
 */
function setupEventListeners() {
    const runButton = document.getElementById('pylog-run');
    const stopButton = document.getElementById('pylog-stop');

    // Run button click
    runButton.onclick = runQuery;

    // Stop button click
    stopButton.onclick = stopQuery;

    // Keyboard shortcuts in textarea
    inputEl.onkeydown = (e) => {
        if (e.key === 'Enter' && (e.ctrlKey || e.metaKey)) {
            e.preventDefault();
            runQuery();
        } else if (e.key === 'ArrowUp' && e.ctrlKey) {
            e.preventDefault();
            navigateHistory(-1);
        } else if (e.key === 'ArrowDown' && e.ctrlKey) {
            e.preventDefault();
            navigateHistory(1);
        }
    };
}

/**
 * Handle messages from the Web Worker
 */
function handleWorkerMessage(event) {
    const { type, data } = event.data;

    switch (type) {
        case 'initialized':
            console.log('PyLog REPL: Worker initialized successfully');
            replState.initialized = true;
            showREPL();
            appendOutput('PyLog REPL initialized successfully!', 'success');
            break;

        case 'solutions':
            console.log('PyLog REPL: Received solutions:', event.data);
            clearQueryTimeout();
            displaySolutions(event.data);
            setRunning(false);
            break;

        case 'error':
            console.error('PyLog REPL: Worker error:', data);
            clearQueryTimeout();
            appendOutput(`Error: ${data.message}`, 'error');
            setRunning(false);
            break;

        case 'stopped':
            console.log('PyLog REPL: Query stopped');
            clearQueryTimeout();
            appendOutput('Query stopped.', 'info');
            setRunning(false);
            break;

        default:
            console.warn('PyLog REPL: Unknown worker message type:', type);
    }
}

/**
 * Handle Web Worker errors
 */
function handleWorkerError(error) {
    console.error('PyLog REPL: Worker error:', error);
    appendOutput(`Worker error: ${error.message}`, 'error');
    setRunning(false);
}

/**
 * Display query solutions with metadata
 */
function displaySolutions(response) {
    const { query, solutions, stepCount, solutionCount, limits } = response;

    if (solutions.length === 0) {
        appendOutput('false.', 'output');
    } else {
        for (let i = 0; i < solutions.length; i++) {
            const solution = solutions[i];
            const bindings = Object.entries(solution)
                .map(([var_, val]) => `${var_} = ${val}`)
                .join(', ');

            appendOutput(bindings || 'true', 'output');

            if (i < solutions.length - 1) {
                appendOutput(';', 'output');
            } else {
                appendOutput('.', 'output');
            }
        }
    }

    // Show execution metadata
    if (stepCount !== undefined || solutionCount !== undefined) {
        const metadata = [];
        if (solutionCount !== undefined) metadata.push(`${solutionCount} solution(s)`);
        if (stepCount !== undefined) metadata.push(`${stepCount} step(s)`);
        if (limits) {
            const limitInfo = [];
            if (limits.maxSolutions) limitInfo.push(`max ${limits.maxSolutions} solutions`);
            if (limits.maxSteps) limitInfo.push(`max ${limits.maxSteps} steps`);
            if (limitInfo.length > 0) metadata.push(`limits: ${limitInfo.join(', ')}`);
        }
        if (metadata.length > 0) {
            appendOutput(`% ${metadata.join(', ')}`, 'info');
        }
    }
}

/**
 * Start the REPL by initializing the Web Worker
 */
async function startREPL() {
    try {
        showLoading('Initializing Pyodide...');

        // Create Web Worker
        console.log('PyLog REPL: Creating Web Worker...');
        replWorker = new Worker('worker.js');

        // Set up worker message handling
        replWorker.onmessage = handleWorkerMessage;
        replWorker.onerror = handleWorkerError;

        // Initialize the worker
        replWorker.postMessage({ type: 'init' });

        // Wait for initialization (will be updated by worker response)
        console.log('PyLog REPL: Worker created, waiting for initialization...');

    } catch (error) {
        console.error('Failed to start REPL:', error);
        appendOutput(`Error: ${error.message}`, 'error');
    }
}

/**
 * Run a Prolog query
 */
function runQuery() {
    const query = inputEl.value.trim();
    if (!query) return;

    // Add to history
    replState.history.push(query);
    replState.historyIndex = replState.history.length;

    // Display query
    appendOutput(`?- ${query}`, 'query');

    // Clear input
    inputEl.value = '';

    // Set running state
    setRunning(true);

    // Handle special commands
    if (query === 'help') {
        showHelp();
        setRunning(false);
        return;
    } else if (query === 'limits') {
        showSafetyLimits();
        setRunning(false);
        return;
    } else if (query.startsWith('set_limits(')) {
        handleSetLimits(query);
        setRunning(false);
        return;
    }

    // Send query to worker with production safety limits
    if (replWorker && replState.initialized) {
        const safetyLimits = getSafetyLimits();

        // Start wall-clock timeout protection
        queryTimeoutId = setTimeout(handleQueryTimeout, safetyLimits.timeoutMs);

        replWorker.postMessage({
            type: 'query',
            data: {
                query: query,
                options: {
                    maxSteps: safetyLimits.maxSteps,
                    maxSolutions: safetyLimits.maxSolutions
                    // timeoutMs handled by UI, not worker
                }
            }
        });
    } else {
        appendOutput('REPL not initialized. Please click "Start REPL" first.', 'error');
        setRunning(false);
    }
}

/**
 * Stop running query by terminating the worker
 */
function stopQuery() {
    console.log('PyLog REPL: Stopping query...');

    // Clear any active timeout
    clearQueryTimeout();

    if (replWorker) {
        // Terminate the worker to stop any running query
        replWorker.terminate();
        replWorker = null;
        replState.initialized = false;

        // Update UI state
        setRunning(false);
        appendOutput('Query stopped. Worker terminated.', 'info');
        appendOutput('Click "Start REPL" to reinitialize.', 'info');

        // Show loading screen again for reinitialization
        showLoading('REPL stopped. Click "Start REPL" to reinitialize.');
    } else {
        setRunning(false);
        appendOutput('No running query to stop.', 'info');
    }
}

/**
 * Navigate command history
 */
function navigateHistory(direction) {
    if (replState.history.length === 0) return;

    replState.historyIndex += direction;

    if (replState.historyIndex < 0) {
        replState.historyIndex = 0;
    } else if (replState.historyIndex >= replState.history.length) {
        replState.historyIndex = replState.history.length;
        inputEl.value = '';
        return;
    }

    inputEl.value = replState.history[replState.historyIndex] || '';
}

/**
 * Append output to the REPL
 */
function appendOutput(text, type = 'output') {
    const div = document.createElement('div');
    div.textContent = text;

    const colors = {
        query: '#1976d2',
        success: '#4caf50',
        error: '#f44336',
        info: '#ff9800',
        output: '#333'
    };

    div.style.color = colors[type] || colors.output;
    div.style.marginBottom = '4px';

    outputEl.appendChild(div);
    outputEl.scrollTop = outputEl.scrollHeight;
}

/**
 * Show help information
 */
function showHelp() {
    const helpText = `
Available commands:
  help        - Show this help message
  limits      - Show current safety limits
  set_limits(steps, solutions, timeout) - Configure safety limits
  X = 42      - Simple unification
  member(X, [1,2,3]) - List membership
  append([1,2], [3,4], X) - List concatenation

CLP(FD) examples:
  X in 1..10, label([X]) - Domain variable
  X #> 5, X #< 10, label([X]) - Constraints

Keyboard shortcuts:
  Ctrl+Enter  - Run query
  Ctrl+↑/↓    - Navigate history

Safety features:
  • Step limit: Prevents infinite loops
  • Solution limit: Caps enumeration
  • Timeout: Wall-clock protection (10s default)
  • Worker termination: Stop button kills runaway queries
    `.trim();

    appendOutput(helpText, 'info');
}

/**
 * Show current safety limits
 */
function showSafetyLimits() {
    const limits = getSafetyLimits();
    const limitsText = `
Current safety limits:
  Max steps: ${limits.maxSteps.toLocaleString()}
  Max solutions: ${limits.maxSolutions}
  Timeout: ${limits.timeoutMs / 1000}s
  Configurable: ${safetyConfig.configurable ? 'Yes' : 'No'}

Example: set_limits(50000, 50, 5000)
  (50k steps, 50 solutions, 5s timeout)
    `.trim();

    appendOutput(limitsText, 'info');
}

/**
 * Handle set_limits command
 */
function handleSetLimits(query) {
    const match = query.match(/set_limits\((\d+),\s*(\d+),\s*(\d+)\)/);
    if (!match) {
        appendOutput('Usage: set_limits(maxSteps, maxSolutions, timeoutMs)', 'error');
        appendOutput('Example: set_limits(50000, 50, 5000)', 'info');
        return;
    }

    const [, steps, solutions, timeoutMs] = match;
    const newConfig = {
        maxSteps: parseInt(steps),
        maxSolutions: parseInt(solutions),
        timeoutMs: parseInt(timeoutMs)
    };

    const success = updateSafetyConfig(newConfig);
    if (success) {
        appendOutput('Safety limits updated successfully.', 'success');
        showSafetyLimits();
    } else {
        appendOutput('Failed to update safety limits. Configuration may be locked.', 'error');
    }
}

/**
 * Set running state and update UI
 */
function setRunning(running) {
    replState.running = running;

    const runButton = document.getElementById('pylog-run');
    const stopButton = document.getElementById('pylog-stop');

    if (runButton) runButton.disabled = running;
    if (stopButton) stopButton.disabled = !running;
    if (inputEl) inputEl.disabled = running;
}

/**
 * Show loading screen
 */
function showLoading(message = 'Loading...') {
    if (loadingEl) {
        loadingEl.querySelector('p').textContent = message;
        loadingEl.style.display = 'block';
    }
    if (replEl) replEl.style.display = 'none';
}

/**
 * Show REPL interface
 */
function showREPL() {
    if (loadingEl) loadingEl.style.display = 'none';
    if (replEl) replEl.style.display = 'block';
    if (inputEl) inputEl.focus();

    replState.initialized = true;
}

// Initialize when DOM is ready
if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initializeREPL);
} else {
    initializeREPL();
}