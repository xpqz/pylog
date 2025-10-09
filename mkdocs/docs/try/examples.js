/**
 * PyLog REPL - Example Queries Library
 *
 * Provides canned example queries for easy experimentation.
 * Examples are organized by category and complexity.
 */

// Example query categories
const PYLOG_EXAMPLES = {
    basic: {
        title: "Basic Queries",
        description: "Simple unification and built-ins",
        examples: [
            {
                query: "X = hello",
                description: "Simple unification with atom",
                expected: "X = hello"
            },
            {
                query: "X = 42",
                description: "Unification with integer",
                expected: "X = 42"
            },
            {
                query: "true",
                description: "Always succeeds",
                expected: "true"
            },
            {
                query: "fail",
                description: "Always fails",
                expected: "false"
            }
        ]
    },

    lists: {
        title: "Lists",
        description: "List operations and membership",
        examples: [
            {
                query: "member(X, [1, 2, 3])",
                description: "List membership - find elements",
                expected: "X = 1; X = 2; X = 3"
            },
            {
                query: "member(2, [1, 2, 3])",
                description: "Check if element is in list",
                expected: "true"
            },
            {
                query: "append([1, 2], [3, 4], X)",
                description: "Concatenate two lists",
                expected: "X = [1, 2, 3, 4]"
            },
            {
                query: "append(X, Y, [1, 2, 3])",
                description: "Split list in all possible ways",
                expected: "X = [], Y = [1, 2, 3]; X = [1], Y = [2, 3]; ..."
            }
        ]
    },

    clpfd: {
        title: "CLP(FD) Constraints",
        description: "Constraint Logic Programming over Finite Domains",
        examples: [
            {
                query: "X in 1..10, label([X])",
                description: "Domain variable with labeling",
                expected: "X = 1; X = 2; ...; X = 10"
            },
            {
                query: "X in 1..10, X #> 5, label([X])",
                description: "Domain with greater-than constraint",
                expected: "X = 6; X = 7; X = 8; X = 9; X = 10"
            },
            {
                query: "X + Y #= 10, X in 1..5, Y in 1..5, label([X, Y])",
                description: "Linear equation with domains",
                expected: "X = 5, Y = 5"
            },
            {
                query: "all_different([X, Y, Z]), X in 1..3, Y in 1..3, Z in 1..3, label([X, Y, Z])",
                description: "All different constraint",
                expected: "X = 1, Y = 2, Z = 3; X = 1, Y = 3, Z = 2; ..."
            }
        ]
    },

    advanced: {
        title: "Advanced",
        description: "Complex queries and patterns",
        examples: [
            {
                query: "length(L, 3), member(X, L)",
                description: "Generate lists of length 3 and their elements",
                expected: "Multiple solutions with different lists"
            },
            {
                query: "findall(X, member(X, [1, 2, 3]), Xs)",
                description: "Collect all solutions",
                expected: "Xs = [1, 2, 3]"
            }
        ]
    }
};

/**
 * Get all example categories
 */
function getExampleCategories() {
    return Object.keys(PYLOG_EXAMPLES).map(key => ({
        id: key,
        ...PYLOG_EXAMPLES[key]
    }));
}

/**
 * Get examples for a specific category
 */
function getExamples(categoryId) {
    return PYLOG_EXAMPLES[categoryId]?.examples || [];
}

/**
 * Get a random example from any category
 */
function getRandomExample() {
    const categories = Object.keys(PYLOG_EXAMPLES);
    const randomCategory = categories[Math.floor(Math.random() * categories.length)];
    const examples = PYLOG_EXAMPLES[randomCategory].examples;
    return examples[Math.floor(Math.random() * examples.length)];
}

/**
 * Search examples by query text or description
 */
function searchExamples(searchTerm) {
    const results = [];
    const term = searchTerm.toLowerCase();

    for (const [categoryId, category] of Object.entries(PYLOG_EXAMPLES)) {
        for (const example of category.examples) {
            if (example.query.toLowerCase().includes(term) ||
                example.description.toLowerCase().includes(term)) {
                results.push({
                    ...example,
                    category: categoryId,
                    categoryTitle: category.title
                });
            }
        }
    }

    return results;
}

/**
 * Create example selector UI (placeholder for future implementation)
 */
function createExampleSelector() {
    // This would create a dropdown or modal with example categories
    console.log('Example selector not yet implemented');
    console.log('Available categories:', getExampleCategories());
}

/**
 * Load example into REPL input (placeholder)
 */
function loadExample(example) {
    console.log('Loading example:', example.query);
    // This would set the REPL input value
    const inputEl = document.getElementById('pylog-input');
    if (inputEl) {
        inputEl.value = example.query;
        inputEl.focus();
    }
}

// Export for global access
window.PyLogExamples = {
    getExampleCategories,
    getExamples,
    getRandomExample,
    searchExamples,
    createExampleSelector,
    loadExample
};

console.log('PyLog Examples library loaded');