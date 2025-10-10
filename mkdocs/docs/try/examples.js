/**
 * PyLog REPL Examples System
 *
 * Provides canned queries organized by category with a dropdown loader.
 * Integrates with the main REPL interface for easy example loading.
 */

// Example categories and queries
const PYLOG_EXAMPLES = {
    'basics': {
        name: 'Basics',
        description: 'Simple unification and basic Prolog concepts',
        examples: [
            {
                title: 'Variable Unification',
                query: 'X = hello',
                description: 'Unify variable X with atom hello'
            },
            {
                title: 'Pattern Matching',
                query: '[H|T] = [1,2,3]',
                description: 'Match head and tail of a list'
            },
            {
                title: 'Structure Unification',
                query: 'point(X, Y) = point(3, 4)',
                description: 'Unify compound terms'
            },
            {
                title: 'Multiple Variables',
                query: 'X = Y, Y = 42',
                description: 'Chain variable unifications'
            }
        ]
    },
    'lists': {
        name: 'Lists',
        description: 'List processing, membership, and manipulation',
        examples: [
            {
                title: 'List Membership',
                query: 'member(X, [1,2,3])',
                description: 'Find members of a list'
            },
            {
                title: 'List Append',
                query: 'append([1,2], [3,4], X)',
                description: 'Concatenate two lists'
            },
            {
                title: 'List Length',
                query: 'length([a,b,c], N)',
                description: 'Calculate list length'
            },
            {
                title: 'Reverse List',
                query: 'reverse([1,2,3], X)',
                description: 'Reverse a list'
            },
            {
                title: 'List Pattern',
                query: '[X,Y|Rest] = [a,b,c,d]',
                description: 'Match multiple list elements'
            }
        ]
    },
    'arithmetic': {
        name: 'Arithmetic',
        description: 'Mathematical expressions and calculations',
        examples: [
            {
                title: 'Basic Arithmetic',
                query: 'X is 2 + 3 * 4',
                description: 'Evaluate arithmetic expression'
            },
            {
                title: 'Arithmetic with Parentheses',
                query: 'Y is (10 - 3) * 2',
                description: 'Complex arithmetic with precedence'
            },
            {
                title: 'Division',
                query: 'Z is 15 / 3',
                description: 'Floating point division'
            },
            {
                title: 'Integer Division',
                query: 'W is 17 // 5',
                description: 'Integer (floor) division'
            },
            {
                title: 'Modulo',
                query: 'R is 17 mod 5',
                description: 'Remainder operation'
            }
        ]
    },
    'clpfd': {
        name: 'CLP(FD)',
        description: 'Constraint Logic Programming over Finite Domains',
        examples: [
            {
                title: 'Domain Variable',
                query: 'X in 1..10, label([X])',
                description: 'Create and label domain variable'
            },
            {
                title: 'Range Constraint',
                query: 'X in 1..10, X #> 5, label([X])',
                description: 'Variable with range constraint'
            },
            {
                title: 'Sum Constraint',
                query: 'X in 1..9, Y in 1..9, X + Y #= 10, label([X,Y])',
                description: 'Two variables summing to 10'
            },
            {
                title: 'All Different',
                query: 'X in 1..3, Y in 1..3, all_different([X,Y]), label([X,Y])',
                description: 'Distinct variable assignment'
            },
            {
                title: 'Multiple Constraints',
                query: 'X in 1..10, Y in 1..10, X #< Y, X + Y #= 12, label([X,Y])',
                description: 'Multiple constraints on variables'
            }
        ]
    }
};

// UI state
let examplesDropdown = null;
let examplesContainer = null;

/**
 * Initialize the examples system
 */
function initializeExamples() {
    console.log('PyLog Examples: Initializing...');

    // Wait for REPL elements to be ready
    const checkREPL = () => {
        const replEl = document.getElementById('pylog-repl');
        const inputEl = document.getElementById('pylog-input');

        if (replEl && inputEl) {
            createExamplesUI();
        } else {
            // Check again in 100ms
            setTimeout(checkREPL, 100);
        }
    };

    checkREPL();
}

/**
 * Create the examples UI components
 */
function createExamplesUI() {
    const replEl = document.getElementById('pylog-repl');
    if (!replEl) return;

    // Create examples container
    examplesContainer = document.createElement('div');
    examplesContainer.id = 'pylog-examples';
    examplesContainer.style.cssText = `
        margin-bottom: 10px;
        padding: 10px;
        background: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 4px;
    `;

    // Create examples UI
    examplesContainer.innerHTML = `
        <div style="display: flex; align-items: center; gap: 10px; margin-bottom: 10px;">
            <label for="examples-category" style="font-weight: bold; color: #495057;">Examples:</label>
            <select id="examples-category" style="
                padding: 4px 8px;
                border: 1px solid #ced4da;
                border-radius: 4px;
                background: white;
                min-width: 120px;
            ">
                <option value="">Choose category...</option>
            </select>
            <select id="examples-query" style="
                padding: 4px 8px;
                border: 1px solid #ced4da;
                border-radius: 4px;
                background: white;
                flex: 1;
                min-width: 200px;
            " disabled>
                <option value="">Select an example...</option>
            </select>
            <button id="examples-load" style="
                padding: 4px 12px;
                background: #007bff;
                color: white;
                border: none;
                border-radius: 4px;
                cursor: pointer;
            " disabled>Load</button>
        </div>
        <div id="examples-description" style="
            font-size: 0.9em;
            color: #6c757d;
            font-style: italic;
            min-height: 1.2em;
        "></div>
    `;

    // Insert before the output area
    const outputEl = document.getElementById('pylog-output');
    if (outputEl) {
        replEl.insertBefore(examplesContainer, outputEl);
    }

    // Populate categories dropdown
    populateCategories();

    // Setup event listeners
    setupExamplesEventListeners();

    console.log('PyLog Examples: UI created successfully');
}

/**
 * Populate the categories dropdown
 */
function populateCategories() {
    const categorySelect = document.getElementById('examples-category');
    if (!categorySelect) return;

    // Clear existing options (except first)
    while (categorySelect.children.length > 1) {
        categorySelect.removeChild(categorySelect.lastChild);
    }

    // Add category options
    for (const [key, category] of Object.entries(PYLOG_EXAMPLES)) {
        const option = document.createElement('option');
        option.value = key;
        option.textContent = `${category.name} (${category.examples.length})`;
        categorySelect.appendChild(option);
    }
}

/**
 * Setup event listeners for examples UI
 */
function setupExamplesEventListeners() {
    const categorySelect = document.getElementById('examples-category');
    const querySelect = document.getElementById('examples-query');
    const loadButton = document.getElementById('examples-load');
    const descriptionDiv = document.getElementById('examples-description');

    if (!categorySelect || !querySelect || !loadButton || !descriptionDiv) return;

    // Category selection
    categorySelect.onchange = () => {
        const categoryKey = categorySelect.value;
        populateQueries(categoryKey);
        updateDescription('');
        querySelect.disabled = !categoryKey;
        loadButton.disabled = true;
    };

    // Query selection
    querySelect.onchange = () => {
        const categoryKey = categorySelect.value;
        const queryIndex = parseInt(querySelect.value);

        if (categoryKey && !isNaN(queryIndex)) {
            const category = PYLOG_EXAMPLES[categoryKey];
            const example = category.examples[queryIndex];
            updateDescription(example.description);
            loadButton.disabled = false;
        } else {
            updateDescription('');
            loadButton.disabled = true;
        }
    };

    // Load button
    loadButton.onclick = () => {
        loadSelectedExample();
    };
}

/**
 * Populate queries dropdown for selected category
 */
function populateQueries(categoryKey) {
    const querySelect = document.getElementById('examples-query');
    if (!querySelect) return;

    // Clear existing options
    querySelect.innerHTML = '<option value="">Select an example...</option>';

    if (!categoryKey || !PYLOG_EXAMPLES[categoryKey]) return;

    const category = PYLOG_EXAMPLES[categoryKey];
    category.examples.forEach((example, index) => {
        const option = document.createElement('option');
        option.value = index.toString();
        option.textContent = example.title;
        querySelect.appendChild(option);
    });
}

/**
 * Update the description text
 */
function updateDescription(description) {
    const descriptionDiv = document.getElementById('examples-description');
    if (descriptionDiv) {
        descriptionDiv.textContent = description || 'Select a category and example to see description.';
    }
}

/**
 * Load the selected example into the input area
 */
function loadSelectedExample() {
    const categorySelect = document.getElementById('examples-category');
    const querySelect = document.getElementById('examples-query');
    const inputEl = document.getElementById('pylog-input');

    if (!categorySelect || !querySelect || !inputEl) return;

    const categoryKey = categorySelect.value;
    const queryIndex = parseInt(querySelect.value);

    if (!categoryKey || isNaN(queryIndex)) return;

    const category = PYLOG_EXAMPLES[categoryKey];
    const example = category.examples[queryIndex];

    if (example) {
        // Load query into input
        inputEl.value = example.query;
        inputEl.focus();

        // Auto-resize textarea if it's multiline
        inputEl.style.height = 'auto';
        inputEl.style.height = Math.max(40, inputEl.scrollHeight) + 'px';

        console.log(`Loaded example: ${example.title}`);
    }
}

/**
 * Get example data for a specific category (for API access)
 */
function getExampleCategory(categoryKey) {
    return PYLOG_EXAMPLES[categoryKey] || null;
}

/**
 * Get all available category keys
 */
function getExampleCategories() {
    return Object.keys(PYLOG_EXAMPLES);
}

// Initialize when DOM is ready and REPL is available
if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initializeExamples);
} else {
    initializeExamples();
}

// Export functions for external access
window.PyLogExamples = {
    getExampleCategory,
    getExampleCategories,
    loadSelectedExample
};