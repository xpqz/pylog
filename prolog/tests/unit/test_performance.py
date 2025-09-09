"""Performance benchmarks for Stage 1 components.

These tests verify acceptable performance for core functionality.
They use pytest-timeout to prevent hanging on performance issues.
"""

import pytest
import time
from pathlib import Path

from prolog.parser.parser import Parser
from prolog.ast.pretty import PrettyPrinter
from prolog.ast.terms import Atom, Int, Var, List, Struct
from prolog.engine.engine import Engine
from prolog.ast.ast import Program


class TestParserPerformance:
    """Benchmark parser performance on various inputs."""
    
    @pytest.mark.timeout(5)  # Should complete in 5 seconds
    def test_parse_large_fact_database(self):
        """Parser should handle large fact databases efficiently."""
        # Generate a large program with many facts
        facts = []
        for i in range(1000):
            facts.append(f"person(p{i}).")
            facts.append(f"age(p{i}, {i % 100}).")
            facts.append(f"city(p{i}, city{i % 50}).")
        
        program_text = "\n".join(facts)
        
        parser = Parser()
        start = time.time()
        program = parser.parse_program(program_text)
        elapsed = time.time() - start
        
        # Should parse 3000 facts in under 2 seconds
        assert elapsed < 2.0, f"Parsing took {elapsed:.2f}s"
        assert len(program.clauses) == 3000
    
    @pytest.mark.timeout(5)
    def test_parse_deeply_nested_terms(self):
        """Parser should handle deeply nested structures."""
        # Create deeply nested list: [1, [2, [3, [...]]]]
        depth = 100
        nested = "]" * depth
        list_str = "[" + ", [".join(str(i) for i in range(1, depth + 1)) + nested
        
        parser = Parser()
        start = time.time()
        term = parser.parse_term(list_str)
        elapsed = time.time() - start
        
        # Should parse in under 1 second
        assert elapsed < 1.0, f"Parsing took {elapsed:.2f}s"
        
        # Verify structure
        current = term
        for i in range(1, min(10, depth)):  # Check first 10 levels
            assert isinstance(current, List)
            assert len(current.items) > 0
            if i < depth and len(current.items) > 1:
                current = current.items[1]
    
    @pytest.mark.timeout(5)
    def test_parse_large_rules(self):
        """Parser should handle rules with many goals."""
        # Create a rule with many goals in the body
        num_goals = 100
        goals = [f"goal{i}(X{i})" for i in range(num_goals)]
        rule = f"result(X) :- {', '.join(goals)}."
        
        parser = Parser()
        start = time.time()
        program = parser.parse_program(rule)
        elapsed = time.time() - start
        
        # Should parse in under 1 second
        assert elapsed < 1.0, f"Parsing took {elapsed:.2f}s"
        assert len(program.clauses) == 1
        assert len(program.clauses[0].body) == num_goals
    
    @pytest.mark.timeout(5)
    def test_parse_complex_mixed_program(self):
        """Parser should handle complex mixed programs efficiently."""
        program_text = """
        % Facts about family relationships
        parent(tom, bob).
        parent(tom, liz).
        parent(bob, ann).
        parent(bob, pat).
        parent(pat, jim).
        
        % Rules with multiple goals
        grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
        ancestor(X, Y) :- parent(X, Y).
        ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
        
        % Lists and structures
        append([], L, L).
        append([H|T], L, [H|R]) :- append(T, L, R).
        
        member(X, [X|_]).
        member(X, [_|T]) :- member(X, T).
        
        % Complex terms
        complex(foo(bar(baz(qux(quux(corge(grault(garply)))))))).
        nested([1, [2, [3, [4, [5, [6, [7, [8, [9, [10]]]]]]]]]]).
        """ * 10  # Repeat 10 times for larger input
        
        parser = Parser()
        start = time.time()
        program = parser.parse_program(program_text)
        elapsed = time.time() - start
        
        # Should parse in under 2 seconds
        assert elapsed < 2.0, f"Parsing took {elapsed:.2f}s"


class TestPrettyPrinterPerformance:
    """Benchmark pretty printer performance."""
    
    @pytest.mark.timeout(5)
    def test_pretty_print_large_list(self):
        """Pretty printer should handle large lists efficiently."""
        # Create a large list
        items = tuple(Int(i) for i in range(1000))
        large_list = List(items, Atom("[]"))
        
        printer = PrettyPrinter()
        start = time.time()
        result = printer.pretty(large_list)
        elapsed = time.time() - start
        
        # Should format in under 1 second
        assert elapsed < 1.0, f"Pretty printing took {elapsed:.2f}s"
        assert result.startswith("[")
        assert result.endswith("]")
    
    @pytest.mark.timeout(5)
    def test_pretty_print_deeply_nested(self):
        """Pretty printer should handle deeply nested structures."""
        # Create deeply nested structure: foo(bar(baz(...)))
        depth = 100
        term = Atom("base")
        for i in range(depth):
            term = Struct(f"f{i}", (term,))
        
        printer = PrettyPrinter()
        start = time.time()
        result = printer.pretty(term)
        elapsed = time.time() - start
        
        # Should format in under 1 second
        assert elapsed < 1.0, f"Pretty printing took {elapsed:.2f}s"
        assert "f99" in result  # Should have outer functor
    
    @pytest.mark.timeout(5)
    def test_pretty_print_many_variables(self):
        """Pretty printer should handle many distinct variables."""
        # Create structure with many variables
        vars = tuple(Var(i, f"X{i}") for i in range(100))
        struct = Struct("pred", vars)
        
        printer = PrettyPrinter()
        start = time.time()
        result = printer.pretty(struct)
        elapsed = time.time() - start
        
        # Should format in under 0.5 seconds
        assert elapsed < 0.5, f"Pretty printing took {elapsed:.2f}s"
        assert "pred(" in result
        # Should maintain variable names
        assert "X0" in result or "_G0" in result


class TestBuiltinPerformance:
    """Benchmark builtin predicate performance."""
    
    @pytest.mark.timeout(5)
    def test_univ_large_structure(self):
        """=../2 should handle large structures efficiently."""
        engine = Engine()
        parser = Parser()
        engine.program = Program([])
        
        # Create a structure with many arguments
        args = ", ".join(f"arg{i}" for i in range(100))
        query_text = f"'=..'(foo({args}), L)."
        
        query = parser.parse_query(query_text)
        
        start = time.time()
        solutions = list(engine.solve(query.goals))
        elapsed = time.time() - start
        
        # Should complete in under 1 second
        assert elapsed < 1.0, f"Univ took {elapsed:.2f}s"
        assert len(solutions) == 1
        
        # Result should be a list with 101 elements (functor + 100 args)
        result_list = solutions[0]["L"]
        # Count elements in the list
        count = 0
        current = result_list
        while isinstance(current, List) and current.items:
            count += len(current.items)
            current = current.tail
        assert count == 101
    
    @pytest.mark.timeout(5)
    def test_functor_construction_performance(self):
        """functor/3 construction should be efficient."""
        engine = Engine()
        parser = Parser()
        engine.program = Program([])
        
        # Test creating structures with various arities
        queries = []
        for arity in [0, 1, 5, 10, 20, 50]:
            queries.append(f"functor(X{arity}, f{arity}, {arity})")
        
        query_text = f"({', '.join(queries)})."
        query = parser.parse_query(query_text)
        
        start = time.time()
        solutions = list(engine.solve(query.goals))
        elapsed = time.time() - start
        
        # Should complete in under 1 second
        assert elapsed < 1.0, f"Functor construction took {elapsed:.2f}s"
        assert len(solutions) == 1
    
    @pytest.mark.timeout(5)
    def test_arg_random_access(self):
        """arg/3 should provide efficient random access."""
        engine = Engine()
        parser = Parser()
        engine.program = Program([])
        
        # Create a structure with 100 arguments and access various positions
        args = ", ".join(f"a{i}" for i in range(100))
        
        # Access multiple positions
        goals = []
        for pos in [1, 10, 25, 50, 75, 100]:
            goals.append(f"arg({pos}, big({args}), X{pos})")
        
        query_text = f"({', '.join(goals)})."
        query = parser.parse_query(query_text)
        
        start = time.time()
        solutions = list(engine.solve(query.goals))
        elapsed = time.time() - start
        
        # Should complete in under 1 second
        assert elapsed < 1.0, f"Arg access took {elapsed:.2f}s"
        assert len(solutions) == 1


class TestLibraryPredicatePerformance:
    """Benchmark library predicate performance."""
    
    @pytest.mark.timeout(5)
    def test_append_performance(self):
        """append/3 should handle reasonable list sizes."""
        engine = Engine()
        parser = Parser()
        
        # Define append if not already available
        program = parser.parse_program("""
            append([], L, L).
            append([H|T], L, [H|R]) :- append(T, L, R).
        """)
        engine.program = program
        
        # Test appending moderate size lists
        list1 = "[" + ", ".join(str(i) for i in range(50)) + "]"
        list2 = "[" + ", ".join(str(i) for i in range(50, 100)) + "]"
        
        query = parser.parse_query(f"append({list1}, {list2}, Result).")
        
        start = time.time()
        solutions = list(engine.solve(query.goals))
        elapsed = time.time() - start
        
        # Should complete in under 2 seconds
        assert elapsed < 2.0, f"Append took {elapsed:.2f}s"
        assert len(solutions) == 1
    
    @pytest.mark.timeout(5)
    def test_member_performance(self):
        """member/2 should handle searching in large lists."""
        engine = Engine()
        parser = Parser()
        
        # Define member
        program = parser.parse_program("""
            member(X, [X|_]).
            member(X, [_|T]) :- member(X, T).
        """)
        engine.program = program
        
        # Search in a large list
        large_list = "[" + ", ".join(f"elem{i}" for i in range(100)) + "]"
        
        # Search for element near the end
        query = parser.parse_query(f"member(elem99, {large_list}).")
        
        start = time.time()
        solutions = list(engine.solve(query.goals))
        elapsed = time.time() - start
        
        # Should complete in under 1 second
        assert elapsed < 1.0, f"Member search took {elapsed:.2f}s"
        assert len(solutions) == 1
    
    @pytest.mark.timeout(5)
    def test_reverse_performance(self):
        """reverse/2 should handle moderate list sizes."""
        engine = Engine()
        parser = Parser()
        
        # Define reverse with accumulator
        program = parser.parse_program("""
            reverse(L, R) :- reverse(L, [], R).
            reverse([], Acc, Acc).
            reverse([H|T], Acc, R) :- reverse(T, [H|Acc], R).
        """)
        engine.program = program
        
        # Reverse a moderate size list
        list_str = "[" + ", ".join(str(i) for i in range(50)) + "]"
        
        query = parser.parse_query(f"reverse({list_str}, R).")
        
        start = time.time()
        solutions = list(engine.solve(query.goals))
        elapsed = time.time() - start
        
        # Should complete in under 1 second
        assert elapsed < 1.0, f"Reverse took {elapsed:.2f}s"
        assert len(solutions) == 1
    
    @pytest.mark.timeout(5)
    def test_between_generation(self):
        """between/3 should efficiently generate ranges."""
        engine = Engine()
        parser = Parser()
        
        # Define between
        program = parser.parse_program("""
            between(Low, High, Low) :- '=<'(Low, High).
            between(Low, High, X) :- 
                '<'(Low, High),
                is(Low1, '+'(Low, 1)),
                between(Low1, High, X).
        """)
        engine.program = program
        
        # Generate a range
        query = parser.parse_query("between(1, 100, X).")
        
        start = time.time()
        solutions = list(engine.solve(query.goals))
        elapsed = time.time() - start
        
        # Should generate 100 solutions in under 2 seconds
        assert elapsed < 2.0, f"Between generation took {elapsed:.2f}s"
        assert len(solutions) == 100


class TestEnginePerformance:
    """Test overall engine performance."""
    
    @pytest.mark.timeout(10)
    def test_backtracking_performance(self):
        """Engine should handle deep backtracking efficiently."""
        engine = Engine()
        parser = Parser()
        
        # Create a program that causes backtracking
        program = parser.parse_program("""
            choice(1). choice(2). choice(3). choice(4). choice(5).
            
            test(R) :- 
                choice(A), choice(B), choice(C),
                '<'(A, B), '<'(B, C),
                is(R, '+'('+'(A, B), C)).
        """)
        engine.program = program
        
        query = parser.parse_query("test(R).")
        
        start = time.time()
        solutions = list(engine.solve(query.goals))
        elapsed = time.time() - start
        
        # Should complete in under 5 seconds despite backtracking
        assert elapsed < 5.0, f"Backtracking took {elapsed:.2f}s"
        # Should find multiple solutions
        assert len(solutions) > 0
    
    @pytest.mark.timeout(10)
    def test_recursive_performance(self):
        """Engine should handle recursive predicates efficiently."""
        engine = Engine()
        parser = Parser()
        
        # Factorial using successor arithmetic (limited depth)
        program = parser.parse_program("""
            nat(0).
            nat(s(N)) :- nat(N).
            
            % Limited factorial for testing
            factorial(0, s(0)).
            factorial(s(N), F) :- 
                factorial(N, F1),
                multiply(s(N), F1, F).
            
            % Simplified multiply
            multiply(_, 0, 0).
            multiply(0, _, 0).
            multiply(s(0), X, X).
            multiply(s(s(N)), X, R) :- 
                multiply(s(N), X, R1),
                add(X, R1, R).
            
            add(0, Y, Y).
            add(s(X), Y, s(Z)) :- add(X, Y, Z).
        """)
        engine.program = program
        
        # Calculate factorial of a small number
        query = parser.parse_query("factorial(s(s(s(0))), F).")  # factorial(3)
        
        start = time.time()
        solutions = list(engine.solve(query.goals))
        elapsed = time.time() - start
        
        # Should complete in under 3 seconds
        assert elapsed < 3.0, f"Recursive factorial took {elapsed:.2f}s"
        assert len(solutions) > 0
    
    @pytest.mark.timeout(5) 
    def test_memory_efficiency(self):
        """Engine should handle memory efficiently with cleanup."""
        engine = Engine()
        parser = Parser()
        
        # Run multiple queries and ensure cleanup
        program = parser.parse_program("""
            data(X) :- between(1, 100, X).
            between(L, H, L) :- '=<'(L, H).
            between(L, H, X) :- '<'(L, H), is(L1, '+'(L, 1)), between(L1, H, X).
        """)
        engine.program = program
        
        # Run the same query multiple times
        for _ in range(10):
            query = parser.parse_query("data(X).")
            solutions = list(engine.solve(query.goals))
            
            # After each query, state should be clean
            assert engine.goal_stack.height() == 0
            assert len(engine.cp_stack) == 0
            assert engine.trail.position() == 0
        
        # Should not have memory issues after multiple queries
        assert True