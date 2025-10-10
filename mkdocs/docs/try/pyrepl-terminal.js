/**
 * PyLog REPL - Terminal-style UI (Single Pane)
 *
 * Provides a true terminal experience with interleaved input/output
 * in a single scrollable pane, similar to a real terminal emulator.
 */

// Terminal State
let terminalState = {
    initialized: false,
    running: false,
    history: [],
    historyIndex: -1,
    currentInput: '',
    waitingForMore: false,
    currentLine: '',
    multilineBuffer: [],
    inputEnabled: true,
    cursorPosition: 0
};

// Worker reference
let replWorker = null;

// DOM Elements
let terminalEl = null;
let currentInputLine = null;
let cursorEl = null;

// Safety configuration (same as before)
let safetyConfig = {
    maxSteps: 100000,
    maxSolutions: 100,
    timeoutMs: 10000,
    configurable: true,
    streaming: false
};

/**
 * Initialize the terminal interface
 */
function initializeTerminal() {
    console.log('PyLog Terminal: Initializing...');

    const container = document.getElementById('pylog-repl-container');
    if (!container) {
        console.error('PyLog Terminal: Container not found');
        return;
    }

    // Create terminal UI
    createTerminalInterface(container);

    // Load history
    loadHistory();

    // Focus terminal
    if (terminalEl) {
        terminalEl.focus();
    }

    console.log('PyLog Terminal: Initialized');
}

/**
 * Create the terminal interface
 */
function createTerminalInterface(container) {
    container.innerHTML = `
        <div id="pylog-terminal" style="
            background: #1e1e1e;
            color: #d4d4d4;
            border: 1px solid #444;
            padding: 10px;
            height: 400px;
            overflow-y: auto;
            font-family: 'Courier New', Consolas, monospace;
            font-size: 14px;
            line-height: 1.4;
            cursor: text;
            position: relative;
        " tabindex="0">
            <div style="color: #6a9955;">
                % PyLog REPL - Terminal Mode
                % Press Enter to run, Shift+Enter for multi-line, Tab for completion
                % Type 'help' for commands, ↑/↓ for history
            </div>
        </div>
        <div style="margin-top: 10px; display: flex; gap: 10px;">
            <button id="pylog-start" style="
                padding: 8px 16px;
                background: #007acc;
                color: white;
                border: none;
                border-radius: 4px;
                cursor: pointer;
            ">Start REPL</button>
            <button id="pylog-clear" style="
                padding: 8px 16px;
                background: #555;
                color: white;
                border: none;
                border-radius: 4px;
                cursor: pointer;
            ">Clear</button>
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

    terminalEl = document.getElementById('pylog-terminal');

    // Setup event handlers
    setupTerminalEvents();

    // Add initial prompt
    addPrompt();
}

/**
 * Setup terminal event handlers
 */
function setupTerminalEvents() {
    // Keyboard input
    terminalEl.addEventListener('keydown', handleTerminalKeydown);
    terminalEl.addEventListener('keypress', handleTerminalKeypress);

    // Click to focus
    terminalEl.addEventListener('click', () => {
        terminalEl.focus();
        updateCursor();
    });

    // Button handlers
    document.getElementById('pylog-start').onclick = startREPL;
    document.getElementById('pylog-clear').onclick = clearTerminal;
    document.getElementById('pylog-stop').onclick = stopQuery;
}

/**
 * Add a prompt line to the terminal
 */
function addPrompt(continuation = false) {
    const promptSpan = document.createElement('span');
    promptSpan.style.color = '#569cd6';
    promptSpan.textContent = continuation ? '|    ' : '?- ';

    const inputSpan = document.createElement('span');
    inputSpan.id = 'current-input';
    inputSpan.style.color = '#d4d4d4';

    const cursorSpan = document.createElement('span');
    cursorSpan.id = 'cursor';
    cursorSpan.style.cssText = `
        background: #d4d4d4;
        color: #1e1e1e;
        animation: blink 1s infinite;
    `;
    cursorSpan.innerHTML = '&nbsp;';

    const lineDiv = document.createElement('div');
    lineDiv.className = 'input-line';
    lineDiv.appendChild(promptSpan);
    lineDiv.appendChild(inputSpan);
    lineDiv.appendChild(cursorSpan);

    terminalEl.appendChild(lineDiv);

    currentInputLine = inputSpan;
    cursorEl = cursorSpan;
    terminalState.currentLine = '';
    terminalState.cursorPosition = 0;

    // Scroll to bottom
    terminalEl.scrollTop = terminalEl.scrollHeight;
}

/**
 * Handle keydown events in terminal
 */
function handleTerminalKeydown(event) {
    if (!terminalState.inputEnabled) return;

    switch(event.key) {
        case 'Enter':
            if (event.shiftKey) {
                // Multi-line input
                event.preventDefault();
                addToMultilineBuffer();
            } else {
                // Execute command
                event.preventDefault();
                executeCurrentInput();
            }
            break;

        case 'ArrowUp':
            event.preventDefault();
            navigateHistory(-1);
            break;

        case 'ArrowDown':
            event.preventDefault();
            navigateHistory(1);
            break;

        case 'ArrowLeft':
            event.preventDefault();
            moveCursor(-1);
            break;

        case 'ArrowRight':
            event.preventDefault();
            moveCursor(1);
            break;

        case 'Backspace':
            event.preventDefault();
            handleBackspace();
            break;

        case 'Delete':
            event.preventDefault();
            handleDelete();
            break;

        case 'Home':
            event.preventDefault();
            terminalState.cursorPosition = 0;
            updateCursor();
            break;

        case 'End':
            event.preventDefault();
            terminalState.cursorPosition = terminalState.currentLine.length;
            updateCursor();
            break;

        case 'Tab':
            event.preventDefault();
            handleTabCompletion();
            break;

        case 'c':
            if (event.ctrlKey || event.metaKey) {
                event.preventDefault();
                handleInterrupt();
            }
            break;

        case 'd':
            if (event.ctrlKey || event.metaKey) {
                event.preventDefault();
                if (terminalState.currentLine === '') {
                    appendOutput('% EOF', 'comment');
                }
            }
            break;

        case 'l':
            if (event.ctrlKey || event.metaKey) {
                event.preventDefault();
                clearTerminal();
            }
            break;
    }
}

/**
 * Handle character input
 */
function handleTerminalKeypress(event) {
    if (!terminalState.inputEnabled) return;
    if (event.ctrlKey || event.metaKey) return;

    event.preventDefault();
    insertCharacter(event.key);
}

/**
 * Insert a character at cursor position
 */
function insertCharacter(char) {
    const before = terminalState.currentLine.substring(0, terminalState.cursorPosition);
    const after = terminalState.currentLine.substring(terminalState.cursorPosition);
    terminalState.currentLine = before + char + after;
    terminalState.cursorPosition++;
    updateInputDisplay();
}

/**
 * Handle backspace
 */
function handleBackspace() {
    if (terminalState.cursorPosition > 0) {
        const before = terminalState.currentLine.substring(0, terminalState.cursorPosition - 1);
        const after = terminalState.currentLine.substring(terminalState.cursorPosition);
        terminalState.currentLine = before + after;
        terminalState.cursorPosition--;
        updateInputDisplay();
    }
}

/**
 * Handle delete
 */
function handleDelete() {
    if (terminalState.cursorPosition < terminalState.currentLine.length) {
        const before = terminalState.currentLine.substring(0, terminalState.cursorPosition);
        const after = terminalState.currentLine.substring(terminalState.cursorPosition + 1);
        terminalState.currentLine = before + after;
        updateInputDisplay();
    }
}

/**
 * Move cursor
 */
function moveCursor(delta) {
    terminalState.cursorPosition = Math.max(0,
        Math.min(terminalState.currentLine.length, terminalState.cursorPosition + delta));
    updateCursor();
}

/**
 * Update input display
 */
function updateInputDisplay() {
    if (currentInputLine) {
        const before = terminalState.currentLine.substring(0, terminalState.cursorPosition);
        const at = terminalState.currentLine[terminalState.cursorPosition] || ' ';
        const after = terminalState.currentLine.substring(terminalState.cursorPosition + 1);

        currentInputLine.textContent = before;
        cursorEl.textContent = at;

        if (after) {
            const afterSpan = document.createElement('span');
            afterSpan.textContent = after;
            cursorEl.parentNode.insertBefore(afterSpan, cursorEl.nextSibling);
        }
    }
}

/**
 * Update cursor position
 */
function updateCursor() {
    updateInputDisplay();
}

/**
 * Execute current input
 */
function executeCurrentInput() {
    const input = terminalState.currentLine.trim();

    // Finalize the current line (remove cursor)
    if (currentInputLine) {
        currentInputLine.textContent = terminalState.currentLine;
        if (cursorEl) cursorEl.remove();
    }

    // Handle empty input
    if (!input && terminalState.multilineBuffer.length === 0) {
        addPrompt();
        return;
    }

    // Build full query from multiline buffer
    let fullQuery = [...terminalState.multilineBuffer, input].join('\n').trim();

    // Clear multiline buffer
    terminalState.multilineBuffer = [];

    // Add to history
    if (fullQuery) {
        addToHistory(fullQuery);
    }

    // Process the query
    processQuery(fullQuery);
}

/**
 * Add current line to multiline buffer
 */
function addToMultilineBuffer() {
    terminalState.multilineBuffer.push(terminalState.currentLine);

    // Finalize current line
    if (currentInputLine) {
        currentInputLine.textContent = terminalState.currentLine;
        if (cursorEl) cursorEl.remove();
    }

    // Add continuation prompt
    addPrompt(true);
}

/**
 * Process a query
 */
function processQuery(query) {
    if (!query) {
        addPrompt();
        return;
    }

    // Disable input during execution
    terminalState.inputEnabled = false;
    terminalState.running = true;

    // Handle special commands
    if (query === 'help') {
        showHelp();
        terminalState.inputEnabled = true;
        terminalState.running = false;
        addPrompt();
        return;
    }

    if (query === 'limits') {
        appendOutput(`% Current limits:
%   maxSteps: ${safetyConfig.maxSteps}
%   maxSolutions: ${safetyConfig.maxSolutions}
%   timeoutMs: ${safetyConfig.timeoutMs}ms
%   streaming: ${safetyConfig.streaming ? 'on' : 'off'}`, 'comment');
        terminalState.inputEnabled = true;
        terminalState.running = false;
        addPrompt();
        return;
    }

    if (query.startsWith('set_limits ')) {
        const parts = query.split(' ');
        if (parts.length === 3) {
            const [_, param, value] = parts;
            const numValue = parseInt(value);
            if (!isNaN(numValue) && numValue > 0) {
                if (param === 'maxSteps') {
                    safetyConfig.maxSteps = numValue;
                    appendOutput(`% maxSteps set to ${numValue}`, 'success');
                } else if (param === 'maxSolutions') {
                    safetyConfig.maxSolutions = numValue;
                    appendOutput(`% maxSolutions set to ${numValue}`, 'success');
                } else if (param === 'timeoutMs') {
                    safetyConfig.timeoutMs = numValue;
                    appendOutput(`% timeoutMs set to ${numValue}`, 'success');
                } else {
                    appendOutput('% Unknown parameter. Use: maxSteps, maxSolutions, or timeoutMs', 'error');
                }
            } else {
                appendOutput('% Value must be a positive integer', 'error');
            }
        } else {
            appendOutput('% Usage: set_limits <param> <value>', 'error');
        }
        terminalState.inputEnabled = true;
        terminalState.running = false;
        addPrompt();
        return;
    }

    if (query.startsWith('streaming ')) {
        const mode = query.substring(10).trim();
        if (mode === 'on') {
            safetyConfig.streaming = true;
            appendOutput('% Streaming mode enabled', 'success');
        } else if (mode === 'off') {
            safetyConfig.streaming = false;
            appendOutput('% Streaming mode disabled', 'success');
        } else {
            appendOutput('% Usage: streaming on/off', 'error');
        }
        terminalState.inputEnabled = true;
        terminalState.running = false;
        addPrompt();
        return;
    }

    // Send to worker if initialized
    if (replWorker && terminalState.initialized) {
        replWorker.postMessage({
            type: 'query',
            data: {
                query: query,
                options: {
                    maxSteps: safetyConfig.maxSteps,
                    maxSolutions: safetyConfig.maxSolutions,
                    timeoutMs: safetyConfig.timeoutMs,
                    streaming: safetyConfig.streaming
                }
            }
        });
    } else {
        appendOutput('% REPL not initialized. Click "Start REPL" first.', 'error');
        terminalState.inputEnabled = true;
        terminalState.running = false;
        addPrompt();
    }
}

/**
 * Append output to terminal
 */
function appendOutput(text, type = 'output') {
    const div = document.createElement('div');
    div.textContent = text;

    const colors = {
        output: '#d4d4d4',
        success: '#4ec9b0',
        error: '#f48771',
        comment: '#6a9955',
        info: '#569cd6'
    };

    div.style.color = colors[type] || colors.output;
    terminalEl.insertBefore(div, document.querySelector('.input-line'));
    terminalEl.scrollTop = terminalEl.scrollHeight;
}

/**
 * Navigate history
 */
function navigateHistory(direction) {
    if (terminalState.history.length === 0) return;

    if (terminalState.historyIndex === terminalState.history.length) {
        terminalState.currentInput = terminalState.currentLine;
    }

    terminalState.historyIndex += direction;
    terminalState.historyIndex = Math.max(0,
        Math.min(terminalState.history.length, terminalState.historyIndex));

    if (terminalState.historyIndex === terminalState.history.length) {
        terminalState.currentLine = terminalState.currentInput;
    } else {
        terminalState.currentLine = terminalState.history[terminalState.historyIndex] || '';
    }

    terminalState.cursorPosition = terminalState.currentLine.length;
    updateInputDisplay();
}

/**
 * Handle tab completion
 */
function handleTabCompletion() {
    const match = terminalState.currentLine.match(/\b([a-z][a-zA-Z0-9_]*)$/);
    if (match) {
        const prefix = match[1];
        const completions = getCompletions(prefix);

        if (completions.length === 1) {
            const completion = completions[0];
            const before = terminalState.currentLine.substring(0,
                terminalState.currentLine.length - prefix.length);
            terminalState.currentLine = before + completion;
            terminalState.cursorPosition = terminalState.currentLine.length;
            updateInputDisplay();
        } else if (completions.length > 1) {
            appendOutput(`% Completions: ${completions.join(', ')}`, 'comment');
        }
    }
}

/**
 * Get completions
 */
function getCompletions(prefix) {
    const builtins = [
        'append', 'member', 'reverse', 'length', 'between',
        'atom', 'integer', 'var', 'nonvar', 'is',
        'true', 'fail', 'repeat', 'call',
        'findall', 'bagof', 'setof',
        'assertz', 'retract', 'abolish',
        'consult', 'listing', 'help', 'trace'
    ];

    return builtins.filter(b => b.startsWith(prefix)).sort();
}

/**
 * Handle interrupt
 */
function handleInterrupt() {
    if (terminalState.running) {
        stopQuery();
    } else {
        appendOutput('^C', 'comment');
        terminalState.currentLine = '';
        terminalState.cursorPosition = 0;
        updateInputDisplay();
    }
}

/**
 * Clear terminal
 */
function clearTerminal() {
    // Keep only the header
    const children = Array.from(terminalEl.children);
    children.forEach((child, i) => {
        if (i > 0 && !child.classList.contains('input-line')) {
            child.remove();
        }
    });
    appendOutput('% Screen cleared', 'comment');
}

/**
 * Show help
 */
function showHelp() {
    const help = `
% Available commands:
%   help        - Show this help
%   limits      - Show current safety limits
%   set_limits <param> <value> - Set safety limits
%   streaming on/off - Toggle streaming mode
%
% Safety limits (configurable):
%   maxSteps    - Maximum execution steps (default: 100000)
%   maxSolutions - Maximum solutions to find (default: 100)
%   timeoutMs   - Query timeout in milliseconds (default: 10000)
%
% Keyboard shortcuts:
%   Enter       - Run query
%   Shift+Enter - New line (multi-line)
%   ↑/↓         - History navigation
%   Tab         - Auto-completion
%   Ctrl+C      - Interrupt/clear
%   Ctrl+D      - EOF signal
%   Ctrl+L      - Clear screen
%
% During queries:
%   ;           - Next solution
%   .           - Stop searching`.trim();

    help.split('\n').forEach(line => {
        appendOutput(line, 'comment');
    });
}

/**
 * Add to history
 */
function addToHistory(command) {
    if (terminalState.history.length === 0 ||
        terminalState.history[terminalState.history.length - 1] !== command) {
        terminalState.history.push(command);
        if (terminalState.history.length > 100) {
            terminalState.history.shift();
        }
        saveHistory();
    }
    terminalState.historyIndex = terminalState.history.length;
}

/**
 * Save history to localStorage
 */
function saveHistory() {
    try {
        localStorage.setItem('pylog_terminal_history',
            JSON.stringify(terminalState.history));
    } catch (e) {
        // Ignore
    }
}

/**
 * Load history from localStorage
 */
function loadHistory() {
    try {
        const saved = localStorage.getItem('pylog_terminal_history');
        if (saved) {
            terminalState.history = JSON.parse(saved);
            terminalState.historyIndex = terminalState.history.length;
        }
    } catch (e) {
        // Ignore
    }
}

/**
 * Start REPL worker
 */
async function startREPL() {
    appendOutput('% Initializing Pyodide...', 'comment');

    replWorker = new Worker('worker.js');
    replWorker.onmessage = handleWorkerMessage;
    replWorker.onerror = (e) => {
        appendOutput(`% Error: ${e.message}`, 'error');
    };

    replWorker.postMessage({ type: 'init' });
}

/**
 * Handle worker messages
 */
function handleWorkerMessage(event) {
    const { type, data } = event.data;

    switch (type) {
        case 'initialized':
            terminalState.initialized = true;
            appendOutput('% PyLog REPL ready!', 'success');
            terminalState.inputEnabled = true;
            terminalState.running = false;
            addPrompt();
            break;

        case 'solutions':
            displaySolutions(event.data);
            terminalState.inputEnabled = true;
            terminalState.running = false;
            addPrompt();
            break;

        case 'error':
            displayError(event.data);
            terminalState.inputEnabled = true;
            terminalState.running = false;
            addPrompt();
            break;

        default:
            console.log('Unknown message:', event.data);
    }
}

/**
 * Format and display error messages with helpful suggestions
 */
function displayError(errorData) {
    const { message, query } = errorData;

    // Display the base error
    appendOutput(`% Error: ${message}`, 'error');

    // Provide helpful suggestions based on error patterns
    if (message.includes('expected opening bracket')) {
        appendOutput('% Hint: Check for missing parentheses or brackets', 'comment');
    }

    if (message.includes('timeout')) {
        appendOutput('% Hint: Query took too long. Try simplifying or use "set_limits timeoutMs <value>" to increase timeout', 'comment');
    }

    if (message.includes('step limit')) {
        appendOutput('% Hint: Too many steps. Try "set_limits maxSteps <value>" to increase limit', 'comment');
    }
}

/**
 * Format error messages for display
 */
function formatError(error) {
    if (typeof error === 'string') {
        return error;
    }

    if (error.message) {
        return error.message;
    }

    return JSON.stringify(error);
}

/**
 * Display query solutions
 */
function displaySolutions(response) {
    const { solutions } = response;

    if (solutions.length === 0) {
        appendOutput('false.', 'output');
    } else {
        solutions.forEach((solution, i) => {
            const bindings = Object.entries(solution)
                .map(([k, v]) => `${k} = ${v}`)
                .join(', ');
            appendOutput(bindings || 'true', 'output');

            if (i < solutions.length - 1) {
                appendOutput(';', 'output');
            } else {
                appendOutput('.', 'output');
            }
        });
    }
}

/**
 * Stop query execution
 */
function stopQuery() {
    if (replWorker) {
        replWorker.terminate();
        replWorker = null;
        terminalState.initialized = false;
    }

    appendOutput('% Query stopped', 'comment');
    terminalState.inputEnabled = true;
    terminalState.running = false;
    addPrompt();
}

/**
 * Get current safety limits
 */
function getSafetyLimits() {
    return {
        maxSteps: safetyConfig.maxSteps,
        maxSolutions: safetyConfig.maxSolutions,
        timeoutMs: safetyConfig.timeoutMs,
        configurable: safetyConfig.configurable,
        streaming: safetyConfig.streaming
    };
}

/**
 * Validate safety limits
 */
function validateSafetyLimits(limits) {
    if (!limits) return false;

    const { maxSteps, maxSolutions, timeoutMs } = limits;

    // Check that all values are positive integers
    if (maxSteps && (!Number.isInteger(maxSteps) || maxSteps <= 0)) {
        return false;
    }

    if (maxSolutions && (!Number.isInteger(maxSolutions) || maxSolutions <= 0)) {
        return false;
    }

    if (timeoutMs && (!Number.isInteger(timeoutMs) || timeoutMs <= 0)) {
        return false;
    }

    return true;
}

// Add cursor blink animation
const style = document.createElement('style');
style.textContent = `
    @keyframes blink {
        0%, 50% { opacity: 1; }
        51%, 100% { opacity: 0; }
    }
`;
document.head.appendChild(style);

// Initialize when DOM ready
if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initializeTerminal);
} else {
    initializeTerminal();
}