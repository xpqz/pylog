/**
 * PyLog REPL - xterm.js Implementation
 *
 * Professional terminal emulator using xterm.js with local-echo addon
 * for a proper REPL experience.
 */

// Terminal instance
let term = null;
let localEcho = null;
let replWorker = null;
let terminalInitialized = false;

// Configuration
const config = {
    maxSteps: 100000,
    maxSolutions: 100,
    timeoutMs: 10000,
    streaming: false
};

const LIMIT_VALIDATION = {
    maxSteps: 1000000,
    maxSolutions: 1000,
    timeoutMs: 60000
};

let queryTimeoutId = null;

/**
 * Initialize the terminal
 */
async function initializeTerminal() {
    console.log('PyLog xterm.js: Initializing...');

    const container = document.getElementById('pylog-repl-container');
    if (!container) {
        console.error('PyLog xterm.js: Container not found');
        return;
    }

    // Create terminal container
    container.innerHTML = `
        <div id="terminal" style="
            width: 100%;
            height: 500px;
            background: #1e1e1e;
            padding: 10px;
            border: 1px solid #444;
            border-radius: 4px;
        "></div>
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
            " disabled>Stop Query</button>
        </div>
    `;

    // Initialize xterm.js
    term = new Terminal({
        cursorBlink: true,
        fontSize: 14,
        fontFamily: "'Courier New', Consolas, monospace",
        theme: {
            background: '#1e1e1e',
            foreground: '#d4d4d4',
            cursor: '#d4d4d4',
            selection: '#264f78',
            black: '#1e1e1e',
            red: '#f48771',
            green: '#4ec9b0',
            yellow: '#ce9178',
            blue: '#569cd6',
            magenta: '#c586c0',
            cyan: '#9cdcfe',
            white: '#d4d4d4',
            brightBlack: '#545454',
            brightRed: '#f48771',
            brightGreen: '#4ec9b0',
            brightYellow: '#ce9178',
            brightBlue: '#569cd6',
            brightMagenta: '#c586c0',
            brightCyan: '#9cdcfe',
            brightWhite: '#d4d4d4'
        }
    });

    // Open terminal in the container
    term.open(document.getElementById('terminal'));

    // Initialize local-echo for REPL functionality
    localEcho = new LocalEchoController(term);

    // Set up autocompletion
    localEcho.addAutocompleteHandler((index, tokens) => {
        if (index === 0) {
            // Prolog predicates for completion
            const predicates = [
                'append', 'assert', 'atom', 'atom_chars', 'atom_codes',
                'between', 'call', 'catch', 'char_code', 'clause',
                'consult', 'copy_term', 'cut', 'fail', 'findall',
                'functor', 'halt', 'integer', 'is', 'length',
                'listing', 'member', 'nl', 'nonvar', 'number',
                'retract', 'retractall', 'reverse', 'setof', 'sort',
                'trace', 'true', 'unify_with_occurs_check', 'var',
                'write', 'writeln'
            ];

            const token = tokens[index];
            if (token) {
                return predicates.filter(p => p.startsWith(token));
            }
            return [];
        }
        return [];
    });

    // Set up button handlers
    document.getElementById('pylog-clear').onclick = () => {
        term.clear();
    };

    document.getElementById('pylog-stop').onclick = () => {
        if (replWorker) {
            replWorker.terminate();
            replWorker = null;
            terminalInitialized = false;
            term.writeln('\r\n\x1b[33m% Query stopped\x1b[0m');
            startREPL();
        }
    };

    // Show welcome message
    term.writeln('\x1b[32m% PyLog REPL - Prolog in Python\x1b[0m');
    term.writeln('\x1b[90m% Type "help" for commands, Tab for completion\x1b[0m');
    term.writeln('\x1b[90m% Initializing Pyodide...\x1b[0m');
    term.writeln('');

    // Start the REPL worker
    startREPL();
}

/**
 * Start the REPL worker
 */
async function startREPL() {
    replWorker = new Worker('worker.js');

    replWorker.onmessage = (event) => {
        const { type, data } = event.data;

        switch (type) {
            case 'initialized':
                terminalInitialized = true;
                term.writeln('\x1b[32m% PyLog REPL ready!\x1b[0m');
                term.writeln('');
                runREPLLoop();
                break;

            case 'solutions':
                displaySolutions(event.data);
                break;

            case 'error':
                term.writeln(`\x1b[31m% Error: ${event.data.message}\x1b[0m`);
                runREPLLoop();
                break;

            case 'solution':
                // Streaming mode - individual solution
                displaySolution(event.data);
                break;

            case 'done':
                // Streaming mode complete
                if (event.data.solutionCount === 0) {
                    term.writeln('false.');
                } else {
                    term.writeln('.');
                }
                runREPLLoop();
                break;
        }
    };

    replWorker.onerror = (error) => {
        term.writeln(`\x1b[31m% Worker error: ${error.message}\x1b[0m`);
    };

    // Initialize the worker
    replWorker.postMessage({ type: 'init' });
}

/**
 * Run the REPL loop
 */
async function runREPLLoop() {
    while (true) {
        try {
            // Read input with prompt
            const input = await localEcho.read('?- ');

            if (!input || !input.trim()) {
                continue;
            }

            const query = input.trim();

            // Handle special commands
            if (query === 'help') {
                showHelp();
                continue;
            }

            if (query === 'clear') {
                term.clear();
                continue;
            }

            if (query === 'limits') {
                showLimits();
                continue;
            }

            if (query.startsWith('set_limits ')) {
                handleSetLimits(query);
                continue;
            }

            if (query.startsWith('streaming ')) {
                handleStreaming(query);
                continue;
            }

            if (query === 'halt' || query === 'halt.') {
                term.writeln('\x1b[33m% Goodbye\x1b[0m');
                if (replWorker) {
                    replWorker.terminate();
                }
                break;
            }

            // Execute Prolog query
            if (terminalInitialized && replWorker) {
                document.getElementById('pylog-stop').disabled = false;

                replWorker.postMessage({
                    type: 'query',
                    data: {
                        query: query,
                        options: {
                            maxSteps: config.maxSteps,
                            maxSolutions: config.maxSolutions,
                            timeoutMs: config.timeoutMs,
                            streaming: config.streaming
                        }
                    }
                });

                // Wait for response (will be handled in onmessage)
                break;
            } else {
                term.writeln('\x1b[33m% REPL not ready. Please wait...\x1b[0m');
            }

        } catch (error) {
            if (error.message === 'Aborted') {
                // User pressed Ctrl+C
                term.writeln('^C');
                continue;
            }
            term.writeln(`\x1b[31m% Error: ${error.message}\x1b[0m`);
        }
    }
}

/**
 * Display solutions
 */
function displaySolutions(response) {
    const { solutions } = response;

    if (solutions.length === 0) {
        term.writeln('false.');
    } else {
        solutions.forEach((solution, i) => {
            const bindings = Object.entries(solution)
                .map(([k, v]) => `${k} = ${v}`)
                .join(', ');

            if (bindings) {
                term.write(bindings);
            } else {
                term.write('true');
            }

            if (i < solutions.length - 1) {
                term.writeln(' ;');
            } else {
                term.writeln('.');
            }
        });
    }

    document.getElementById('pylog-stop').disabled = true;
    runREPLLoop();
}

/**
 * Display single solution (streaming mode)
 */
function displaySolution(data) {
    const { bindings, index } = data;

    const bindingStr = Object.entries(bindings)
        .map(([k, v]) => `${k} = ${v}`)
        .join(', ');

    if (bindingStr) {
        term.write(bindingStr);
    } else {
        term.write('true');
    }

    term.writeln(' ;');
}

/**
 * Show help
 */
function showHelp() {
    term.writeln('\x1b[36m% Available commands:\x1b[0m');
    term.writeln('%   help         - Show this help');
    term.writeln('%   clear        - Clear the terminal');
    term.writeln('%   limits       - Show current limits');
    term.writeln('%   set_limits <param> <value> - Set limits');
    term.writeln('%   streaming on/off - Toggle streaming mode');
    term.writeln('%   halt         - Exit REPL');
    term.writeln('');
    term.writeln('\x1b[36m% Keyboard shortcuts:\x1b[0m');
    term.writeln('%   Tab          - Autocomplete');
    term.writeln('%   ↑/↓          - History navigation');
    term.writeln('%   Ctrl+C       - Cancel input');
   term.writeln('%   Ctrl+L       - Clear screen');
   term.writeln('%   Ctrl+A/E     - Jump to start/end of line');
   term.writeln('%   Alt+←/→      - Jump by word');
   term.writeln('');
    term.writeln('\x1b[36m% Safety features:\x1b[0m');
    term.writeln('%   Step limit          - Prevents infinite loops');
    term.writeln('%   Solution limit      - Caps enumeration');
    term.writeln('%   Timeout protection  - Aborts long-running queries');
    term.writeln('%   Worker termination  - Stops runaway queries safely');
    term.writeln('');
    term.writeln('\x1b[90m% Documentation: https://github.com/xpqz/pylog (see ../basics/terms.md)\x1b[0m');
}

/**
 * Show current limits
 */
function showLimits() {
    term.writeln('\x1b[36m% Current limits:\x1b[0m');
    term.writeln(`%   maxSteps: ${config.maxSteps}`);
    term.writeln(`%   maxSolutions: ${config.maxSolutions}`);
    term.writeln(`%   timeoutMs: ${config.timeoutMs}`);
    term.writeln(`%   streaming: ${config.streaming ? 'on' : 'off'}`);
}

/**
 * Handle set_limits command
 */
function handleSetLimits(query) {
    const parts = query.split(' ');
    if (parts.length === 3) {
        const [_, param, value] = parts;
        const numValue = parseInt(value);

        if (!isNaN(numValue) && numValue > 0) {
            if (param in config && param !== 'streaming') {
                config[param] = numValue;
                term.writeln(`\x1b[32m% ${param} set to ${numValue}\x1b[0m`);
            } else {
                term.writeln('\x1b[31m% Unknown parameter. Use: maxSteps, maxSolutions, or timeoutMs\x1b[0m');
            }
        } else {
            term.writeln('\x1b[31m% Value must be a positive integer\x1b[0m');
        }
    } else {
        term.writeln('\x1b[31m% Usage: set_limits <param> <value>\x1b[0m');
    }
}

/**
 * Handle streaming command
 */
function handleStreaming(query) {
    const mode = query.substring(10).trim();
    if (mode === 'on') {
        config.streaming = true;
        term.writeln('\x1b[32m% Streaming mode enabled\x1b[0m');
    } else if (mode === 'off') {
        config.streaming = false;
        term.writeln('\x1b[32m% Streaming mode disabled\x1b[0m');
    } else {
        term.writeln('\x1b[31m% Usage: streaming on/off\x1b[0m');
    }
}

/**
 * Expose current safety limits for testing/documentation
 */
function getSafetyLimits() {
    return {
        maxSteps: config.maxSteps,
        maxSolutions: config.maxSolutions,
        timeoutMs: config.timeoutMs,
        streaming: config.streaming
    };
}

/**
 * Validate user-provided safety limits against upper bounds
 */
function validateSafetyLimits(limits) {
    if (!limits) {
        return false;
    }

    if ('maxSteps' in limits && (!Number.isInteger(limits.maxSteps) || limits.maxSteps <= 0 || limits.maxSteps > LIMIT_VALIDATION.maxSteps)) {
        return false;
    }
    if ('maxSolutions' in limits && (!Number.isInteger(limits.maxSolutions) || limits.maxSolutions <= 0 || limits.maxSolutions > LIMIT_VALIDATION.maxSolutions)) {
        return false;
    }
    if ('timeoutMs' in limits && (!Number.isInteger(limits.timeoutMs) || limits.timeoutMs <= 0 || limits.timeoutMs > LIMIT_VALIDATION.timeoutMs)) {
        return false;
    }

    return true;
}

/**
 * Schedule a timeout for the active query
 */
function scheduleQueryTimeout(onTimeout) {
    clearQueryTimeout();
    queryTimeoutId = setTimeout(() => {
        handleQueryTimeout();
        if (typeof onTimeout === 'function') {
            onTimeout();
        }
    }, config.timeoutMs);
}

/**
 * Clear any active query timeout
 */
function clearQueryTimeout() {
    if (queryTimeoutId) {
        clearTimeout(queryTimeoutId);
        queryTimeoutId = null;
    }
}

/**
 * Handle query timeout by terminating the worker and restarting
 */
function handleQueryTimeout() {
    clearQueryTimeout();
    term.writeln('\x1b[33m% Query timed out. Worker terminated.\x1b[0m');
    if (replWorker) {
        try {
            replWorker.terminate();
        } finally {
            replWorker = null;
            terminalInitialized = false;
        }
    }
    startREPL();
}

/**
 * Display formatted error information
 */
function displayError(errorData) {
    if (!errorData) {
        return;
    }

    const message = errorData.message || 'Unknown error';
    term.writeln(`\x1b[31m% Error: ${message}\x1b[0m`);

    if (errorData.errorType === 'ReaderError' && typeof errorData.position === 'number') {
        displayReaderError(message, errorData.position, errorData.token, errorData.query);
    } else {
        addParseErrorSuggestions(message, errorData.token, errorData.query);
    }
}

/**
 * Display ReaderError with caret marker highlighting
 */
function displayReaderError(message, position, token, query) {
    const source = query || '';
    const lines = source.split('\n');
    let currentPos = 0;
    let lineNumber = 0;
    let columnNumber = 0;

    for (let i = 0; i < lines.length; i++) {
        const lineLength = lines[i].length + 1; // account for newline
        if (currentPos + lineLength > position) {
            lineNumber = i;
            columnNumber = position - currentPos;
            break;
        }
        currentPos += lineLength;
    }

    const errorLine = lines[lineNumber] ?? '';
    const marker = ' '.repeat(columnNumber) + '^';

    term.writeln(`% At position ${position} (line ${lineNumber + 1}, column ${columnNumber + 1})`);
    term.writeln(`% ${errorLine}`);
    term.writeln(`% ${marker}`);

    if (token) {
        term.writeln(`% Unexpected token: ${token}`);
    }

    addParseErrorSuggestions(message, token, query);
}

/**
 * Provide contextual suggestions for parse errors
 */
function addParseErrorSuggestions(message, token, query) {
    const lowerMessage = (message || '').toLowerCase();

    if (lowerMessage.includes('expected opening bracket')) {
        term.writeln('% Hint: expected opening bracket - check parentheses or list syntax');
    }
    if (lowerMessage.includes('expected period')) {
        term.writeln('% Hint: expected period - remember to end clauses with a period.');
    }
    if (lowerMessage.includes('unknown operator')) {
        term.writeln('% Hint: unknown operator - verify operator spelling or precedence.');
    }

    term.writeln('% Documentation: ../basics/terms.md for syntax reference');
}

// Initialize when DOM is ready
if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initializeTerminal);
} else {
    initializeTerminal();
}
