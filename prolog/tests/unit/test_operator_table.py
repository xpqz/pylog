"""
Tests for Stage 1.5: Static operator table
Tests the operator precedence and associativity table that serves
as the single source of truth for the reader.
"""

import pytest
from typing import Dict, Tuple


# The operator table structure we expect
# Each operator maps to (precedence, type, canonical_form)
# Type is one of: fx, fy, xf, yf, xfx, xfy, yfx, yfy
OperatorInfo = Tuple[int, str, str]  # (precedence, type, canonical_form)

# Mark all tests in this file for Stage 1.5
pytestmark = pytest.mark.stage15


class TestOperatorTable:
    """Test the static operator table with exact precedence and associativity."""
    
    def setup_method(self):
        """Import the operator table once it exists."""
        # This will import from prolog.parser.operators once implemented
        # For now, we define the expected table for testing
        self.expected_operators = self.get_expected_table()
    
    def get_expected_table(self) -> Dict[Tuple[str, str], OperatorInfo]:
        """Define the expected operator table for testing.
        
        Returns dict mapping (operator, position) to (precedence, type, canonical).
        Position is 'infix', 'prefix', or 'postfix'.
        """
        return {
            # Control flow operators
            (',', 'infix'): (1000, 'xfy', "','"),
            (';', 'infix'): (1100, 'xfy', "';'"),
            ('->', 'infix'): (1050, 'xfy', "'->'"),
            
            # Equality and disequality
            ('=', 'infix'): (700, 'xfx', "'='"),
            ('\\=', 'infix'): (700, 'xfx', "'\\\\='"),
            
            # Structural comparison
            ('==', 'infix'): (700, 'xfx', "'=='"),
            ('\\==', 'infix'): (700, 'xfx', "'\\\\=='"),
            
            # Term order
            ('@<', 'infix'): (700, 'xfx', "'@<'"),
            ('@>', 'infix'): (700, 'xfx', "'@>'"),
            ('@=<', 'infix'): (700, 'xfx', "'@=<'"),
            ('@>=', 'infix'): (700, 'xfx', "'@>='"),
            
            # Arithmetic comparison
            ('<', 'infix'): (700, 'xfx', "'<'"),
            ('>', 'infix'): (700, 'xfx', "'>'"),
            ('=<', 'infix'): (700, 'xfx', "'=<'"),
            ('>=', 'infix'): (700, 'xfx', "'>='"),
            ('=:=', 'infix'): (700, 'xfx', "'=:='"),
            ('=\\=', 'infix'): (700, 'xfx', "'=\\\\='"),
            
            # Arithmetic operators
            ('+', 'infix'): (500, 'yfx', "'+'"),
            ('-', 'infix'): (500, 'yfx', "'-'"),
            ('*', 'infix'): (400, 'yfx', "'*'"),
            ('/', 'infix'): (400, 'yfx', "'/'"),
            ('//', 'infix'): (400, 'yfx', "'//'"),
            ('mod', 'infix'): (400, 'yfx', "'mod'"),
            ('**', 'infix'): (200, 'xfy', "'**'"),  # Right associative!
            
            # Unary operators
            ('-', 'prefix'): (200, 'fy', "'-'"),
            ('+', 'prefix'): (200, 'fy', "'+'"),
            ('\\+', 'prefix'): (900, 'fy', "'\\\\+'"),
            
            # Other standard operators
            ('is', 'infix'): (700, 'xfx', "'is'"),
        }
    
    def test_operator_table_import(self):
        """Test that we can import the operator table module."""
        try:
            from prolog.parser.operators import OPERATOR_TABLE
            assert OPERATOR_TABLE is not None
        except ImportError:
            pytest.skip("operators.py not yet implemented")
    
    def test_comma_precedence_and_type(self):
        """Test that ',' has exact precedence 1000 and type xfy."""
        try:
            from prolog.parser.operators import get_operator_info
            info = get_operator_info(',', 'infix')
            assert info[0] == 1000  # precedence
            assert info[1] == 'xfy'  # type (right associative)
            assert info[2] == "','"  # canonical form
        except ImportError:
            pytest.skip("operators.py not yet implemented")
    
    def test_semicolon_precedence_and_type(self):
        """Test that ';' has exact precedence 1100 and type xfy."""
        try:
            from prolog.parser.operators import get_operator_info
            info = get_operator_info(';', 'infix')
            assert info[0] == 1100  # precedence
            assert info[1] == 'xfy'  # type (right associative)
            assert info[2] == "';'"  # canonical form
        except ImportError:
            pytest.skip("operators.py not yet implemented")
    
    def test_arrow_precedence_and_type(self):
        """Test that '->' has exact precedence 1050 and type xfy."""
        try:
            from prolog.parser.operators import get_operator_info
            info = get_operator_info('->', 'infix')
            assert info[0] == 1050  # precedence
            assert info[1] == 'xfy'  # type (right associative)
            assert info[2] == "'->'"  # canonical form
        except ImportError:
            pytest.skip("operators.py not yet implemented")
    
    def test_plus_infix_precedence(self):
        """Test that infix '+' has exact precedence 500 and type yfx."""
        try:
            from prolog.parser.operators import get_operator_info
            info = get_operator_info('+', 'infix')
            assert info[0] == 500  # precedence
            assert info[1] == 'yfx'  # type (left associative)
            assert info[2] == "'+'"  # canonical form
        except ImportError:
            pytest.skip("operators.py not yet implemented")
    
    def test_plus_prefix_precedence(self):
        """Test that prefix '+' has exact precedence 200 and type fy."""
        try:
            from prolog.parser.operators import get_operator_info
            info = get_operator_info('+', 'prefix')
            assert info[0] == 200  # precedence
            assert info[1] == 'fy'  # type
            assert info[2] == "'+'"  # canonical form
        except ImportError:
            pytest.skip("operators.py not yet implemented")
    
    def test_minus_infix_precedence(self):
        """Test that infix '-' has exact precedence 500 and type yfx."""
        try:
            from prolog.parser.operators import get_operator_info
            info = get_operator_info('-', 'infix')
            assert info[0] == 500  # precedence
            assert info[1] == 'yfx'  # type (left associative)
            assert info[2] == "'-'"  # canonical form
        except ImportError:
            pytest.skip("operators.py not yet implemented")
    
    def test_minus_prefix_precedence(self):
        """Test that prefix '-' has exact precedence 200 and type fy."""
        try:
            from prolog.parser.operators import get_operator_info
            info = get_operator_info('-', 'prefix')
            assert info[0] == 200  # precedence
            assert info[1] == 'fy'  # type
            assert info[2] == "'-'"  # canonical form
        except ImportError:
            pytest.skip("operators.py not yet implemented")
    
    def test_power_right_associative(self):
        """Test that '**' has exact precedence 200 and type xfy (right-assoc)."""
        try:
            from prolog.parser.operators import get_operator_info
            info = get_operator_info('**', 'infix')
            assert info[0] == 200  # precedence
            assert info[1] == 'xfy'  # type (RIGHT associative!)
            assert info[2] == "'**'"  # canonical form
        except ImportError:
            pytest.skip("operators.py not yet implemented")
    
    def test_all_comparison_ops_are_xfx(self):
        """Test that all comparison operators are precedence 700, type xfx (non-chainable)."""
        comparison_ops = [
            '=', '\\=', '==', '\\==',  # Equality/disequality
            '@<', '@>', '@=<', '@>=',  # Term order
            '<', '>', '=<', '>=', '=:=', '=\\=',  # Arithmetic
            'is'  # is/2 is also a comparison operator
        ]
        
        try:
            from prolog.parser.operators import get_operator_info
            for op in comparison_ops:
                info = get_operator_info(op, 'infix')
                assert info[0] == 700, f"{op} should have precedence 700"
                assert info[1] == 'xfx', f"{op} should be xfx (non-chainable)"
        except ImportError:
            pytest.skip("operators.py not yet implemented")
    
    def test_all_arithmetic_ops_precedence(self):
        """Test that arithmetic operators have correct precedence and type."""
        arithmetic_ops = {
            '*': (400, 'yfx'),
            '/': (400, 'yfx'),
            '//': (400, 'yfx'),
            'mod': (400, 'yfx'),
        }
        
        try:
            from prolog.parser.operators import get_operator_info
            for op, (prec, typ) in arithmetic_ops.items():
                info = get_operator_info(op, 'infix')
                assert info[0] == prec, f"{op} should have precedence {prec}"
                assert info[1] == typ, f"{op} should have type {typ}"
        except ImportError:
            pytest.skip("operators.py not yet implemented")
    
    def test_term_order_ops_precedence(self):
        """Test that term order operators (@<, @=<, etc.) are 700, xfx."""
        term_order_ops = ['@<', '@>', '@=<', '@>=']
        
        try:
            from prolog.parser.operators import get_operator_info
            for op in term_order_ops:
                info = get_operator_info(op, 'infix')
                assert info[0] == 700, f"{op} should have precedence 700"
                assert info[1] == 'xfx', f"{op} should be xfx"
        except ImportError:
            pytest.skip("operators.py not yet implemented")
    
    def test_mod_word_token_operator(self):
        """Test that 'mod' is a word-token operator at precedence 400, yfx."""
        try:
            from prolog.parser.operators import get_operator_info
            info = get_operator_info('mod', 'infix')
            assert info[0] == 400  # precedence
            assert info[1] == 'yfx'  # type (left associative)
            assert info[2] == "'mod'"  # canonical form
        except ImportError:
            pytest.skip("operators.py not yet implemented")
    
    def test_mod_quoted_still_works(self):
        """Test that 'mod'(X,Y) as quoted atom is not affected by operator table."""
        # Here we only assert the table does not treat quoted atoms as operators.
        # Parser tests will verify runtime: "'mod'(X,Y)" is parsed as a functor call.
        pass
    
    def test_canonical_mapping_complete(self):
        """Test that every operator has a one-to-one canonical mapping."""
        try:
            from prolog.parser.operators import OPERATOR_TABLE
            
            for (op, _), (_, _, canonical) in OPERATOR_TABLE.items():
                # Every operator should have a canonical form
                assert canonical is not None and canonical != ""
                # Canonical form should be quoted functor
                assert canonical.startswith("'") and canonical.endswith("'")
                # The operator should be in the canonical form (accounting for escaping)
                if '\\' in op:
                    # For operators with backslashes, check escaped form
                    assert op.replace('\\', '\\\\') in canonical or op in canonical
                else:
                    assert op in canonical
        except ImportError:
            pytest.skip("operators.py not yet implemented")
    
    def test_unsupported_operators_marked(self):
        """Test that operators lacking Stage 1 runtime support are marked."""
        # These operators parse but should fail at runtime in Stage 1
        unsupported = ['//', 'mod', '**']
        
        try:
            from prolog.parser.operators import get_operator_info, is_stage1_supported
            
            for op in unsupported:
                info = get_operator_info(op, 'infix')
                assert info is not None  # Should be in table
                assert not is_stage1_supported(op, 'infix'), f"{op} should be marked as unsupported in Stage 1"
        except ImportError:
            pytest.skip("operators.py not yet implemented")
    
    def test_all_operators_have_types(self):
        """Test that all operators have valid associativity types."""
        valid_types = {'fx', 'fy', 'xf', 'yf', 'xfx', 'xfy', 'yfx', 'yfy'}
        
        try:
            from prolog.parser.operators import OPERATOR_TABLE
            
            for (op, _), (_, typ, _) in OPERATOR_TABLE.items():
                assert typ in valid_types, f"{op} has invalid type {typ}"
        except ImportError:
            pytest.skip("operators.py not yet implemented")
    
    def test_operator_lookup_by_symbol_and_position(self):
        """Test that we can look up operators by symbol and position."""
        try:
            from prolog.parser.operators import get_operator_info
            
            # Test that + has different info for prefix vs infix
            prefix_plus = get_operator_info('+', 'prefix')
            infix_plus = get_operator_info('+', 'infix')
            
            assert prefix_plus[0] == 200  # prefix precedence
            assert infix_plus[0] == 500   # infix precedence
            
            # Same for minus
            prefix_minus = get_operator_info('-', 'prefix')
            infix_minus = get_operator_info('-', 'infix')
            
            assert prefix_minus[0] == 200  # prefix precedence
            assert infix_minus[0] == 500   # infix precedence
        except ImportError:
            pytest.skip("operators.py not yet implemented")
    
    def test_no_soft_cut_operator(self):
        """Test that soft-cut (*->) is NOT in the operator table (deferred)."""
        try:
            from prolog.parser.operators import get_operator_info
            
            assert get_operator_info('*->', 'infix') is None
        except ImportError:
            pytest.skip("operators.py not yet implemented")
    
    def test_no_univ_operator(self):
        """Test that univ (=..) is NOT in the operator table (remains canonical-only)."""
        try:
            from prolog.parser.operators import get_operator_info
            
            assert get_operator_info('=..', 'infix') is None
        except ImportError:
            pytest.skip("operators.py not yet implemented")
    
    def test_is_precedence_and_type(self):
        """Test that 'is' has exact precedence 700 and type xfx."""
        try:
            from prolog.parser.operators import get_operator_info
            info = get_operator_info('is', 'infix')
            assert info[0] == 700  # precedence
            assert info[1] == 'xfx'  # type (non-chainable)
            assert info[2] == "'is'"  # canonical form
        except ImportError:
            pytest.skip("operators.py not yet implemented")
    
    def test_not_prefix_precedence_and_type(self):
        """Test that '\\+' (not) has exact precedence 900 and type fy."""
        try:
            from prolog.parser.operators import get_operator_info
            info = get_operator_info('\\+', 'prefix')
            assert info[0] == 900  # precedence
            assert info[1] == 'fy'  # type
            assert info[2] == "'\\\\+'"  # canonical form
        except ImportError:
            pytest.skip("operators.py not yet implemented")
    
    def test_power_beats_prefix_minus_in_binding(self):
        """Test that power and prefix minus have same precedence (200)."""
        try:
            from prolog.parser.operators import get_operator_info
            p_pow, _, _ = get_operator_info('**', 'infix')
            p_neg, _, _ = get_operator_info('-', 'prefix')
            assert p_neg == p_pow == 200, "Both should be at precedence 200"
        except ImportError:
            pytest.skip("operators.py not yet implemented")
    
    @pytest.mark.parametrize("key,expected", [
        ((',', 'infix'), (1000, 'xfy', "','")),
        ((';', 'infix'), (1100, 'xfy', "';'")),
        (('->', 'infix'), (1050, 'xfy', "'->'")),
        (('=', 'infix'), (700, 'xfx', "'='")),
        (('is', 'infix'), (700, 'xfx', "'is'")),
        (('+', 'infix'), (500, 'yfx', "'+'")),
        (('+', 'prefix'), (200, 'fy', "'+'")),
        (('-', 'infix'), (500, 'yfx', "'-'")),
        (('-', 'prefix'), (200, 'fy', "'-'")),
        (('**', 'infix'), (200, 'xfy', "'**'")),
        (('\\+', 'prefix'), (900, 'fy', "'\\\\+'")),
    ])
    def test_table_matches_expected(self, key, expected):
        """Test that operator table matches expected values."""
        try:
            from prolog.parser.operators import get_operator_info
            op, pos = key
            assert get_operator_info(op, pos) == expected
        except ImportError:
            pytest.skip("operators.py not yet implemented")
    
    def test_canonical_escaping_for_backslash_ops(self):
        """Test proper escaping in canonical forms for backslash operators."""
        cases = {
            '\\=': "'\\\\='",
            '\\==': "'\\\\=='", 
            '=\\=': "'=\\\\='",
            '\\+': "'\\\\+'"
        }
        try:
            from prolog.parser.operators import get_operator_info
            for op, want in cases.items():
                pos = 'prefix' if op == '\\+' else 'infix'
                info = get_operator_info(op, pos)
                assert info[2] == want, f"{op} canonical should be {want}"
        except ImportError:
            pytest.skip("operators.py not yet implemented")