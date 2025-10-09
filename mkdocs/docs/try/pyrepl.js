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

        <div style="display: flex; gap: 10px;">
            <input type="text" id="pylog-input" placeholder="?- " style="
                flex: 1;
                padding: 8px;
                border: 1px solid #ddd;
                border-radius: 4px;
                font-family: 'Courier New', monospace;
            ">
            <button id="pylog-run" style="
                padding: 8px 16px;
                background: #4caf50;
                color: white;
                border: none;
                border-radius: 4px;
                cursor: pointer;
            ">Run</button>
            <button id="pylog-stop" style="
                padding: 8px 16px;
                background: #f44336;
                color: white;
                border: none;
                border-radius: 4px;
                cursor: pointer;
            " disabled>Stop</button>
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

    // Enter key in input
    inputEl.onkeydown = (e) => {
        if (e.key === 'Enter') {
            runQuery();
        } else if (e.key === 'ArrowUp') {
            e.preventDefault();
            navigateHistory(-1);
        } else if (e.key === 'ArrowDown') {
            e.preventDefault();
            navigateHistory(1);
        }
    };
}

/**
 * Start the REPL by initializing the Web Worker
 */
async function startREPL() {
    try {
        showLoading('Initializing Pyodide...');

        // Initialize worker (placeholder - actual worker creation in worker.js)
        console.log('PyLog REPL: Starting Web Worker...');

        // Simulate initialization delay
        await new Promise(resolve => setTimeout(resolve, 1000));

        showREPL();
        appendOutput('PyLog REPL initialized successfully!', 'success');

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

    // Simulate query execution (placeholder)
    setTimeout(() => {
        if (query === 'help') {
            showHelp();
        } else {
            appendOutput('Query execution not yet implemented.', 'info');
        }
        setRunning(false);
    }, 500);
}

/**
 * Stop running query
 */
function stopQuery() {
    console.log('PyLog REPL: Stopping query...');
    // Placeholder - actual implementation will terminate worker
    setRunning(false);
    appendOutput('Query stopped.', 'info');
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