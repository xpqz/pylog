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
    currentLine: '',
    multilineBuffer: [],
    inputEnabled: true,
    cursorPosition: 0,
    solutionQueue: [],
    pendingDone: null,
    solutionsDisplayed: 0,
    shownSolutionHint: false,
    awaitingUser: false,
    queryFinished: false,
    currentQuery: ''
};

// Worker reference
let replWorker = null;

// DOM Elements
let terminalEl = null;
let currentInputLine = null;
let cursorEl = null;

// Cached controls
let stopButton = null;

const LIMIT_VALIDATION = {
    maxSteps: 1000000,
    maxSolutions: 1000,
    timeoutMs: 60000
};

let queryTimeoutId = null;

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

    // Auto-start the REPL
    startREPL();

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
                % Initializing Pyodide...
            </div>
        </div>
        <div style="margin-top: 10px; display: flex; gap: 10px;">
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

    // Clipboard support
    terminalEl.addEventListener('paste', handlePaste);
    terminalEl.addEventListener('copy', handleCopy);
    terminalEl.addEventListener('cut', handleCut);

    // Click to focus
    terminalEl.addEventListener('click', () => {
        terminalEl.focus();
        updateCursor();
    });

    // Prevent backspace from navigating back
    document.addEventListener('keydown', (event) => {
        if (event.key === 'Backspace' &&
            !['input', 'textarea'].includes(event.target.tagName.toLowerCase()) &&
            !event.target.isContentEditable) {
            event.preventDefault();
        }
    });

    // Button handlers
    document.getElementById('pylog-clear').onclick = clearTerminal;
    stopButton = document.getElementById('pylog-stop');
    if (stopButton) {
        stopButton.onclick = stopQuery;
        stopButton.disabled = true;
    }

    // Reset interactive state for a fresh session
    resetInteractiveState();
}

/**
 * Reset per-query interactive state
 */
function resetInteractiveState() {
    clearQueryTimeout();
    terminalState.solutionQueue = [];
    terminalState.pendingDone = null;
    terminalState.solutionsDisplayed = 0;
    terminalState.shownSolutionHint = false;
    terminalState.awaitingUser = false;
    terminalState.queryFinished = false;
    terminalState.currentQuery = '';
}

/**
 * Enable or disable the stop button safely
 */
function setStopButtonEnabled(enabled) {
    if (stopButton) {
        stopButton.disabled = !enabled;
    }
}

/**
 * Format metadata summary for display
 */
function formatRunMetadata(metadata) {
    if (!metadata) {
        return null;
    }

    const parts = [];
    const { solutions, stepCount, elapsedMs } = metadata;

    if (typeof solutions === 'number') {
        parts.push(`${solutions} solution(s)`);
    }
    if (typeof stepCount === 'number') {
        parts.push(`${stepCount} step(s)`);
    }
    if (typeof elapsedMs === 'number') {
        const ms = Math.max(0, Math.round(elapsedMs));
        parts.push(`${ms}ms elapsed`);
    }

    return parts.length ? `% ${parts.join(', ')}` : null;
}

/**
 * Validate requested safety limits against configured bounds
 */
function validateSafetyLimits(limits) {
    if (!limits) {
        return false;
    }

    const { maxSteps, maxSolutions, timeoutMs } = limits;

    if (maxSteps !== undefined) {
        if (!Number.isInteger(maxSteps) || maxSteps <= 0 || maxSteps > LIMIT_VALIDATION.maxSteps) {
            return false;
        }
    }

    if (maxSolutions !== undefined) {
        if (!Number.isInteger(maxSolutions) || maxSolutions <= 0 || maxSolutions > LIMIT_VALIDATION.maxSolutions) {
            return false;
        }
    }

    if (timeoutMs !== undefined) {
        if (!Number.isInteger(timeoutMs) || timeoutMs <= 0 || timeoutMs > LIMIT_VALIDATION.timeoutMs) {
            return false;
        }
    }

    return true;
}

/**
 * Cancel any active timeout
 */
function clearQueryTimeout() {
    if (queryTimeoutId) {
        clearTimeout(queryTimeoutId);
        queryTimeoutId = null;
    }
}

/**
 * Schedule timeout for the active query
 */
function scheduleQueryTimeout() {
    clearQueryTimeout();
    queryTimeoutId = setTimeout(() => {
        handleQueryTimeout();
    }, safetyConfig.timeoutMs);
}

/**
 * Handle timeout firing by terminating the worker safely
 */
function handleQueryTimeout() {
    clearQueryTimeout();
    appendOutput('% Query timed out. Worker terminated.', 'comment');

    if (replWorker) {
        try {
            replWorker.terminate();
        } catch (e) {
            console.error('PyLog Terminal: Failed to terminate worker on timeout', e);
        } finally {
            replWorker = null;
            terminalState.initialized = false;
        }
    }

    finalizeQuery({
        solutions: terminalState.solutionsDisplayed
    });

    // Attempt to restart the REPL so user can continue
    startREPL();
}

/**
 * Add a prompt line to the terminal
 */
function addPrompt(continuation = false) {
    // Clear any existing references
    currentInputLine = null;
    cursorEl = null;

    const promptSpan = document.createElement('span');
    promptSpan.style.color = '#569cd6';
    promptSpan.textContent = continuation ? '|    ' : '?- ';

    const inputSpan = document.createElement('span');
    inputSpan.style.color = '#d4d4d4';

    const cursorSpan = document.createElement('span');
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
    terminalState.inputEnabled = true;
    terminalState.awaitingUser = false;

    // Scroll to bottom
    terminalEl.scrollTop = terminalEl.scrollHeight;
}

/**
 * Handle keydown events in terminal
 */
function handleTerminalKeydown(event) {
    // Ensure terminal keeps focus
    if (document.activeElement !== terminalEl) {
        terminalEl.focus();
    }

    if (terminalState.awaitingUser) {
        if (event.key === ';') {
            event.preventDefault();
            requestNextSolution();
            return;
        }
        if (event.key === '.') {
            event.preventDefault();
            stopSolutionBrowsing();
            return;
        }
        if ((event.key === 'c' || event.key === 'C') && (event.ctrlKey || event.metaKey)) {
            event.preventDefault();
            abortSolutionBrowsing();
            return;
        }

        // Ignore any other keys while awaiting user decision
        event.preventDefault();
        return;
    }

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
    if (terminalState.awaitingUser) {
        event.preventDefault();
        return;
    }

    if (!terminalState.inputEnabled) return;
    if (event.ctrlKey || event.metaKey) return;

    event.preventDefault();
    insertCharacter(event.key);
}

/**
 * Handle paste event
 */
function handlePaste(event) {
    if (!terminalState.inputEnabled) return;

    event.preventDefault();

    // Get pasted text
    const pastedText = (event.clipboardData || window.clipboardData).getData('text');

    if (pastedText) {
        // Insert the pasted text at cursor position
        const before = terminalState.currentLine.substring(0, terminalState.cursorPosition);
        const after = terminalState.currentLine.substring(terminalState.cursorPosition);
        terminalState.currentLine = before + pastedText + after;
        terminalState.cursorPosition += pastedText.length;
        updateInputDisplay();
    }
}

/**
 * Handle copy event
 */
function handleCopy(event) {
    // Get selected text
    const selection = window.getSelection();
    if (selection.toString()) {
        // Let the browser handle the copy for selected text
        return;
    }

    // If no selection, copy the current line
    if (terminalState.currentLine) {
        event.preventDefault();
        event.clipboardData.setData('text/plain', terminalState.currentLine);
    }
}

/**
 * Handle cut event
 */
function handleCut(event) {
    if (!terminalState.inputEnabled) return;

    // Get selected text
    const selection = window.getSelection();
    if (selection.toString()) {
        // Let browser handle cut for selected text
        return;
    }

    // If no selection, cut the whole line
    if (terminalState.currentLine) {
        event.preventDefault();
        event.clipboardData.setData('text/plain', terminalState.currentLine);
        terminalState.currentLine = '';
        terminalState.cursorPosition = 0;
        updateInputDisplay();
    }
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

        // Remove any existing tail spans
        while (cursorEl.nextSibling) {
            cursorEl.nextSibling.remove();
        }

        // Add new tail span if there's text after cursor
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

    // Finalize the current line (remove cursor and input-line class)
    if (currentInputLine) {
        currentInputLine.textContent = terminalState.currentLine;
        if (cursorEl) cursorEl.remove();
        // Remove input-line class so output appears after the latest prompt
        const lineDiv = currentInputLine.parentElement;
        if (lineDiv) lineDiv.classList.remove('input-line');
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

    // Finalize current line (remove cursor and input-line class)
    if (currentInputLine) {
        currentInputLine.textContent = terminalState.currentLine;
        if (cursorEl) cursorEl.remove();
        // Remove input-line class so output appears after the latest prompt
        const lineDiv = currentInputLine.parentElement;
        if (lineDiv) lineDiv.classList.remove('input-line');
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
    resetInteractiveState();
    terminalState.inputEnabled = false;
    terminalState.running = true;
    terminalState.currentQuery = query;
    setStopButtonEnabled(true);

    // Handle special commands
    if (query === 'help') {
        showHelp();
        terminalState.inputEnabled = true;
        terminalState.running = false;
        setStopButtonEnabled(false);
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
        setStopButtonEnabled(false);
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
        setStopButtonEnabled(false);
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
        setStopButtonEnabled(false);
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

        scheduleQueryTimeout();
    } else {
        appendOutput('% REPL still initializing. Please wait...', 'comment');
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

    // Insert before the current input line (if it exists) or at the end
    const inputLine = document.querySelector('.input-line');
    if (inputLine) {
        terminalEl.insertBefore(div, inputLine);
    } else {
        terminalEl.appendChild(div);
    }

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
 * Handle streaming solution event
 */
function handleStreamingSolution(message) {
    if (terminalState.queryFinished) {
        return;
    }

    const solutionText = message.pretty || formatBindingsFromStructured(message.bindings);
    queueSolution(solutionText || 'true');
}

/**
 * Handle streaming done event
 */
function handleStreamingDone(message) {
    terminalState.pendingDone = {
        solutions: message.solutions,
        elapsedMs: message.elapsedMs,
        stepCount: message.stepCount
    };

    if (terminalState.queryFinished) {
        return;
    }

    if (terminalState.solutionsDisplayed === 0) {
        appendOutput('false.', 'output');
        finalizeQuery();
        return;
    }

    if (!terminalState.awaitingUser && terminalState.solutionQueue.length === 0) {
        // No user decision pending (e.g., all solutions delivered automatically)
        appendOutput('.', 'output');
        finalizeQuery();
        return;
    }

    if (terminalState.awaitingUser && terminalState.solutionQueue.length === 0) {
        // User already saw final solution; automatically conclude
        appendOutput('.', 'output');
        finalizeQuery();
    }
}

/**
 * Queue solution text for user-controlled browsing
 */
function queueSolution(solutionText) {
    if (terminalState.solutionsDisplayed === 0 && !terminalState.awaitingUser) {
        showSolution(solutionText);
        enterSolutionBrowsing();
    } else {
        terminalState.solutionQueue.push(solutionText);
    }
}

/**
 * Display a solution line
 */
function showSolution(solutionText) {
    appendOutput(solutionText, 'output');
    terminalState.solutionsDisplayed += 1;

    if (!terminalState.shownSolutionHint) {
        appendOutput("% Press ';' for next solution or '.' to stop", 'comment');
        terminalState.shownSolutionHint = true;
    }
}

/**
 * Enter solution browsing state allowing ; / . input
 */
function enterSolutionBrowsing() {
    terminalState.awaitingUser = true;
    terminalState.inputEnabled = true;
}

/**
 * Request the next solution (triggered by ';')
 */
function requestNextSolution() {
    if (!terminalState.awaitingUser) {
        return;
    }

    if (terminalState.solutionQueue.length > 0) {
        const nextSolution = terminalState.solutionQueue.shift();
        showSolution(nextSolution);

        if (terminalState.solutionQueue.length === 0 && terminalState.pendingDone &&
            terminalState.pendingDone.solutions <= terminalState.solutionsDisplayed) {
            appendOutput('.', 'output');
            finalizeQuery();
        }
        return;
    }

    if (terminalState.pendingDone) {
        appendOutput('false.', 'output');
        finalizeQuery();
    } else {
        appendOutput('% Waiting for additional solutions...', 'comment');
    }
}

/**
 * Stop browsing solutions (triggered by '.')
 */
function stopSolutionBrowsing() {
    if (!terminalState.awaitingUser) {
        return;
    }

    appendOutput('.', 'output');
    const metadata = terminalState.pendingDone || {
        solutions: terminalState.solutionsDisplayed
    };
    finalizeQuery(metadata);
}

/**
 * Abort browsing via Ctrl+C
 */
function abortSolutionBrowsing() {
    if (!terminalState.awaitingUser) {
        return;
    }

    appendOutput('^C', 'comment');
    const metadata = terminalState.pendingDone || {
        solutions: terminalState.solutionsDisplayed
    };
    finalizeQuery(metadata);
}

/**
 * Finalize current query, emitting metadata and restoring prompt
 */
function finalizeQuery(metadata = null) {
    if (terminalState.queryFinished) {
        return;
    }

    clearQueryTimeout();

    const summary = formatRunMetadata(metadata || terminalState.pendingDone);

    terminalState.pendingDone = null;
    terminalState.solutionQueue = [];
    terminalState.awaitingUser = false;
    terminalState.running = false;
    terminalState.inputEnabled = true;
    terminalState.queryFinished = true;
    terminalState.currentLine = '';
    terminalState.cursorPosition = 0;
    terminalState.solutionsDisplayed = 0;
    terminalState.shownSolutionHint = false;
    terminalState.currentQuery = '';

    if (summary) {
        appendOutput(summary, 'comment');
    }

    setStopButtonEnabled(false);
    addPrompt();
}

/**
 * Fallback formatter when structured bindings are provided without pretty text
 */
function formatBindingsFromStructured(bindings) {
    if (!bindings || typeof bindings !== 'object') {
        return '';
    }

    const parts = [];
    for (const [key, value] of Object.entries(bindings)) {
        if (value && typeof value === 'object' && 'value' in value) {
            parts.push(`${key} = ${value.value}`);
        } else {
            parts.push(`${key} = ${String(value)}`);
        }
    }

    return parts.join(', ');
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
% Safety features:
%   Step limit        - Prevents infinite loops (default: 100000)
%   Solution limit    - Caps enumeration (default: 100)
%   Timeout protection- Aborts runaway queries (default: 10000ms)
%   Worker termination- Stops queries safely on timeout
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
%   .           - Stop searching
%
% Documentation: https://github.com/xpqz/pylog (see ../basics/terms.md)`.trim();

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
    const { type } = event.data;

    switch (type) {
        case 'initialized':
            terminalState.initialized = true;
            appendOutput('% PyLog REPL ready!', 'success');
            terminalState.inputEnabled = true;
            setStopButtonEnabled(false);
            addPrompt();
            break;

        case 'solutions':
            displaySolutions(event.data);
            break;

        case 'error':
            displayError(event.data);
            finalizeQuery();
            break;

        case 'solution':
            handleStreamingSolution(event.data);
            break;

        case 'done':
            handleStreamingDone(event.data);
            break;

        case 'progress':
            if (event.data.message) {
                appendOutput(`% ${event.data.message}`, 'comment');
            }
            break;

        case 'stdlib-loaded':
            appendOutput(`% Loaded ${event.data.clauseCount} stdlib clauses from ${event.data.fileCount} file(s)`, 'comment');
            break;

        default:
            console.log('Unknown message:', event.data);
    }
}

/**
 * Format and display error messages with helpful suggestions
 */
function displayError(errorData) {
    if (!errorData) {
        return;
    }

    const message = errorData.message || 'Unknown error';

    appendOutput(`% Error: ${message}`, 'error');

    if (errorData.errorType === 'ReaderError' && typeof errorData.position === 'number') {
        displayReaderError(message, errorData.position, errorData.token, errorData.query);
    } else {
        addParseErrorSuggestions(message, errorData.token, errorData.query);
    }

    if (message.includes('timeout')) {
        appendOutput('% Hint: Query took too long. Use "set_limits timeoutMs <value>" to increase timeout.', 'comment');
    }

    if (message.includes('step limit')) {
        appendOutput('% Hint: Too many steps. Use "set_limits maxSteps <value>" to increase the limit.', 'comment');
    }
}

/**
 * Display ReaderError details with caret marker
 */
function displayReaderError(message, position, token, query) {
    const source = query || '';
    const lines = source.split('\n');
    let currentPos = 0;
    let lineNumber = 0;
    let columnNumber = 0;

    for (let i = 0; i < lines.length; i++) {
        const lineLength = lines[i].length + 1;
        if (currentPos + lineLength > position) {
            lineNumber = i;
            columnNumber = position - currentPos;
            break;
        }
        currentPos += lineLength;
    }

    const errorLine = lines[lineNumber] ?? '';
    const marker = ' '.repeat(columnNumber) + '^';

    appendOutput(`% At position ${position} (line ${lineNumber + 1}, column ${columnNumber + 1})`, 'comment');
    appendOutput(`% ${errorLine}`, 'comment');
    appendOutput(`% ${marker}`, 'comment');

    if (token) {
        appendOutput(`% Unexpected token: ${token}`, 'comment');
    }

    addParseErrorSuggestions(message, token, query);
}

/**
 * Provide contextual hints for parse errors
 */
function addParseErrorSuggestions(message, token, query) {
    const lowerMessage = (message || '').toLowerCase();

    if (lowerMessage.includes('expected opening bracket')) {
        appendOutput('% Hint: expected opening bracket - check parentheses or list syntax.', 'comment');
    }

    if (lowerMessage.includes('expected period')) {
        appendOutput('% Hint: expected period - remember to terminate clauses with a period.', 'comment');
    }

    if (lowerMessage.includes('unknown operator')) {
        appendOutput('% Hint: unknown operator - verify operator spelling or precedence.', 'comment');
    }

    appendOutput('% Documentation: ../basics/terms.md for syntax reference', 'comment');
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
    const solutions = response?.solutions || [];
    const metadata = {
        solutions: response?.solutionCount ?? solutions.length,
        stepCount: response?.stepCount,
        elapsedMs: response?.elapsedMs
    };

    if (solutions.length === 0) {
        appendOutput('false.', 'output');
        finalizeQuery(metadata);
        return;
    }

    solutions.forEach((solution, index) => {
        const bindings = Object.entries(solution)
            .map(([k, v]) => `${k} = ${v}`)
            .join(', ');
        appendOutput(bindings || 'true', 'output');

        if (index < solutions.length - 1) {
            appendOutput(';', 'output');
        } else {
            appendOutput('.', 'output');
        }
    });

    finalizeQuery(metadata);
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
    finalizeQuery({
        solutions: terminalState.solutionsDisplayed
    });

    // Restart worker for subsequent queries
    startREPL();
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
