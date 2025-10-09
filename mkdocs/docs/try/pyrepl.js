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

// DOM Elements
let containerEl = null;
let loadingEl = null;
let replEl = null;
let outputEl = null;
let inputEl = null;

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
                PyLog REPL ready. Type queries and press Enter.
                <br>Examples: X = 42  |  member(X, [1,2,3])  |  help
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
            console.log('PyLog REPL: Received solutions:', data);
            displaySolutions(data.query, data.solutions);
            setRunning(false);
            break;

        case 'error':
            console.error('PyLog REPL: Worker error:', data);
            appendOutput(`Error: ${data.message}`, 'error');
            setRunning(false);
            break;

        case 'stopped':
            console.log('PyLog REPL: Query stopped');
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
 * Display query solutions
 */
function displaySolutions(query, solutions) {
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
    }

    // Send query to worker
    if (replWorker && replState.initialized) {
        replWorker.postMessage({
            type: 'query',
            data: { query: query }
        });
    } else {
        appendOutput('REPL not initialized. Please click "Start REPL" first.', 'error');
        setRunning(false);
    }
}

/**
 * Stop running query
 */
function stopQuery() {
    console.log('PyLog REPL: Stopping query...');

    if (replWorker) {
        replWorker.postMessage({ type: 'stop' });
    } else {
        setRunning(false);
        appendOutput('Query stopped.', 'info');
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
  X = 42      - Simple unification
  member(X, [1,2,3]) - List membership
  append([1,2], [3,4], X) - List concatenation

CLP(FD) examples:
  X in 1..10, label([X]) - Domain variable
  X #> 5, X #< 10, label([X]) - Constraints

Keyboard shortcuts:
  Ctrl+Enter  - Run query
  Ctrl+↑/↓    - Navigate history
    `.trim();

    appendOutput(helpText, 'info');
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