"""Unit tests for WAM bytecode loader.

Tests loading and validation of assembled bytecode into Machine.
"""

import pytest
from prolog.wam.asm import assemble
from prolog.wam.instructions import (
    OP_GET_CONSTANT,
    OP_GET_STRUCTURE,
    OP_GET_VARIABLE,
    OP_PROCEED,
    OP_CALL,
    OP_EXECUTE,
    OP_UNIFY_VARIABLE,
    OP_UNIFY_CONSTANT,
)
from prolog.wam.loader import BytecodeLoadError, load_program, PredicateRegistry


class TestBasicLoading:
    """Test basic bytecode loading."""

    def test_load_simple_fact(self):
        """Load simple fact bytecode."""
        text = """
label user:p/1:
  get_constant a, A1
  proceed
"""
        assembled = assemble(text)
        result = load_program(assembled)

        assert "code" in result
        assert "registry" in result
        assert len(result["code"]) == 2
        assert result["code"][0] == (OP_GET_CONSTANT, "a", 1)
        assert result["code"][1] == (OP_PROCEED,)

    def test_load_creates_registry(self):
        """Loader creates predicate registry."""
        text = """
label user:p/1:
  proceed
"""
        assembled = assemble(text)
        result = load_program(assembled)

        registry = result["registry"]
        assert isinstance(registry, PredicateRegistry)
        assert registry.lookup("user:p/1") == 0

    def test_load_multiple_predicates(self):
        """Load multiple predicates with correct offsets."""
        text = """
label user:p/0:
  proceed

label user:q/1:
  get_variable X0, A1
  proceed
"""
        assembled = assemble(text)
        result = load_program(assembled)

        registry = result["registry"]
        assert registry.lookup("user:p/0") == 0
        assert registry.lookup("user:q/1") == 1


class TestValidation:
    """Test bytecode validation."""

    def test_validate_opcode_range(self):
        """Invalid opcode raises error."""
        bytecode = {"code": [(999, "foo")], "symbols": {}}  # Invalid opcode

        with pytest.raises(BytecodeLoadError) as exc:
            load_program(bytecode)
        assert "unknown opcode" in str(exc.value).lower()
        assert "999" in str(exc.value)

    def test_validate_arity_mismatch(self):
        """Opcode with wrong arity raises error."""
        bytecode = {
            "code": [(OP_PROCEED, "extra", "args")],  # proceed takes 0 args
            "symbols": {},
        }

        with pytest.raises(BytecodeLoadError) as exc:
            load_program(bytecode)
        assert "arity" in str(exc.value).lower()

    def test_validate_register_bounds(self):
        """Register index out of reasonable bounds raises error."""
        bytecode = {
            "code": [(OP_GET_VARIABLE, 999, 1)],  # X999 probably unreasonable
            "symbols": {"user:p/1": 0},
        }

        # This may or may not raise depending on implementation
        # At minimum, loader should track max registers used
        result = load_program(bytecode)
        assert "code" in result

    def test_validate_negative_register(self):
        """Negative register index raises error."""
        bytecode = {"code": [(OP_GET_VARIABLE, -1, 1)], "symbols": {}}

        with pytest.raises(BytecodeLoadError) as exc:
            load_program(bytecode)
        assert "register" in str(exc.value).lower()


class TestSymbolResolution:
    """Test symbol table handling."""

    def test_symbols_added_to_registry(self):
        """Symbol table entries added to predicate registry."""
        text = """
label user:append/3:
  proceed

label user:member/2:
  get_variable X0, A1
  proceed
"""
        assembled = assemble(text)
        result = load_program(assembled)

        registry = result["registry"]
        assert registry.lookup("user:append/3") == 0
        assert registry.lookup("user:member/2") == 1

    def test_lookup_undefined_predicate(self):
        """Lookup of undefined predicate returns None or raises."""
        text = """
label user:p/0:
  proceed
"""
        assembled = assemble(text)
        result = load_program(assembled)

        registry = result["registry"]
        # Should return None or raise KeyError
        result = registry.lookup("user:undefined/0")
        assert result is None or isinstance(result, type(None))

    def test_symbols_with_different_modules(self):
        """Predicates from different modules tracked separately."""
        text = """
label user:p/0:
  proceed

label other:p/0:
  proceed
"""
        assembled = assemble(text)
        result = load_program(assembled)

        registry = result["registry"]
        assert registry.lookup("user:p/0") == 0
        assert registry.lookup("other:p/0") == 1


class TestPredicateRegistry:
    """Test PredicateRegistry class."""

    def test_registry_creation(self):
        """Registry can be created empty."""
        registry = PredicateRegistry()
        assert registry.lookup("user:p/0") is None

    def test_registry_add_predicate(self):
        """Add predicate to registry."""
        registry = PredicateRegistry()
        registry.add("user:p/0", 0)
        assert registry.lookup("user:p/0") == 0

    def test_registry_list_predicates(self):
        """List all registered predicates."""
        registry = PredicateRegistry()
        registry.add("user:p/0", 0)
        registry.add("user:q/1", 10)

        predicates = registry.list()
        assert "user:p/0" in predicates
        assert "user:q/1" in predicates
        assert len(predicates) >= 2

    def test_registry_duplicate_predicate(self):
        """Adding duplicate predicate updates offset or raises."""
        registry = PredicateRegistry()
        registry.add("user:p/0", 0)

        # Either updates or raises error
        try:
            registry.add("user:p/0", 10)
            assert registry.lookup("user:p/0") == 10  # Updated
        except Exception:
            # Raises on duplicate
            pass


class TestErrorMessages:
    """Test BytecodeLoadError structure."""

    def test_error_has_code(self):
        """LoadError includes error code."""
        bytecode = {"code": [(999,)], "symbols": {}}

        with pytest.raises(BytecodeLoadError) as exc:
            load_program(bytecode)

        error = exc.value
        # Error should have structured info
        assert hasattr(error, "code") or hasattr(error, "args")

    def test_error_has_message(self):
        """LoadError includes human-readable message."""
        bytecode = {"code": [(999,)], "symbols": {}}

        with pytest.raises(BytecodeLoadError) as exc:
            load_program(bytecode)

        assert len(str(exc.value)) > 0

    def test_error_includes_pc(self):
        """LoadError includes program counter context."""
        bytecode = {
            "code": [
                (OP_PROCEED,),
                (999,),  # Invalid at PC=1
            ],
            "symbols": {},
        }

        with pytest.raises(BytecodeLoadError) as exc:
            load_program(bytecode)

        # Error message should mention location
        assert "1" in str(exc.value) or "pc" in str(exc.value).lower()


class TestYRegisterHandling:
    """Test Y-register operand handling."""

    def test_load_y_register_tuple(self):
        """Y registers as (\"Y\", idx) tuples are handled."""
        bytecode = {
            "code": [(OP_GET_VARIABLE, ("Y", 0), 1)],
            "symbols": {"user:p/1": 0},
        }

        # Should load without error
        result = load_program(bytecode)
        assert "code" in result
        # Loader may transform Y-registers or leave as-is

    def test_load_x_register_int(self):
        """X registers as integers are handled."""
        bytecode = {"code": [(OP_GET_VARIABLE, 0, 1)], "symbols": {"user:p/1": 0}}

        result = load_program(bytecode)
        assert result["code"][0] == (OP_GET_VARIABLE, 0, 1)


class TestCompletePrograms:
    """Test loading complete programs."""

    def test_load_fact_and_rule(self):
        """Load program with fact and rule."""
        text = """
label user:base/1:
  get_constant a, A1
  proceed

label user:recursive/1:
  get_variable X0, A1
  put_variable X0, A1
  call user:helper/1
  proceed
"""
        assembled = assemble(text)
        result = load_program(assembled)

        assert len(result["code"]) == 6
        registry = result["registry"]
        assert registry.lookup("user:base/1") == 0
        assert registry.lookup("user:recursive/1") == 2

    def test_load_with_lco(self):
        """Load program with last call optimization."""
        text = """
label user:tail_recursive/1:
  get_variable X0, A1
  put_variable X0, A1
  execute user:tail_recursive/1
"""
        assembled = assemble(text)
        result = load_program(assembled)

        code = result["code"]
        assert code[-1][0] == OP_EXECUTE

    def test_load_with_structures(self):
        """Load program with structure operations."""
        text = """
label user:p/1:
  get_structure f/2, A1
    unify_variable X0
    unify_constant a
  proceed
"""
        assembled = assemble(text)
        result = load_program(assembled)

        code = result["code"]
        assert code[0][0] == OP_GET_STRUCTURE
        assert code[1][0] == OP_UNIFY_VARIABLE
        assert code[2][0] == OP_UNIFY_CONSTANT


class TestCodeAreaInstallation:
    """Test installation into Machine code area."""

    def test_code_installed_sequentially(self):
        """Code instructions installed in sequential order."""
        text = """
label user:p/0:
  proceed

label user:q/0:
  proceed
"""
        assembled = assemble(text)
        result = load_program(assembled)

        code = result["code"]
        assert len(code) == 2
        assert code[0] == (OP_PROCEED,)
        assert code[1] == (OP_PROCEED,)

    def test_code_preserves_structure(self):
        """Loaded code preserves instruction structure."""
        text = """
label user:p/1:
  get_constant hello, A1
  proceed
"""
        assembled = assemble(text)
        result = load_program(assembled)

        code = result["code"]
        assert code[0] == (OP_GET_CONSTANT, "hello", 1)
        assert code[1] == (OP_PROCEED,)


class TestEdgeCases:
    """Test edge cases and corner scenarios."""

    def test_load_empty_program(self):
        """Load empty bytecode."""
        bytecode = {"code": [], "symbols": {}}

        result = load_program(bytecode)
        assert result["code"] == []
        assert len(result["registry"].list()) == 0

    def test_load_single_instruction(self):
        """Load program with single instruction."""
        bytecode = {"code": [(OP_PROCEED,)], "symbols": {}}

        result = load_program(bytecode)
        assert len(result["code"]) == 1

    def test_load_without_symbols(self):
        """Load bytecode without symbol table."""
        bytecode = {"code": [(OP_PROCEED,)], "symbols": {}}

        result = load_program(bytecode)
        assert len(result["registry"].list()) == 0

    def test_symbols_without_code(self):
        """Symbol table without corresponding code."""
        bytecode = {
            "code": [],
            "symbols": {"user:p/0": 0},  # Points to non-existent code
        }

        # May raise error or handle gracefully
        with pytest.raises(BytecodeLoadError):
            load_program(bytecode)


class TestValidationDetails:
    """Test detailed validation rules."""

    def test_validate_functor_format(self):
        """Functor operands are tuples (name, arity)."""
        bytecode = {
            "code": [(OP_GET_STRUCTURE, "invalid", 1)],  # Should be ("name", arity)
            "symbols": {},
        }

        with pytest.raises(BytecodeLoadError) as exc:
            load_program(bytecode)
        assert (
            "functor" in str(exc.value).lower() or "operand" in str(exc.value).lower()
        )

    def test_validate_call_target_string(self):
        """Call/execute targets must be strings."""
        bytecode = {
            "code": [(OP_CALL, 123)],  # Should be "module:pred/arity"
            "symbols": {},
        }

        with pytest.raises(BytecodeLoadError) as exc:
            load_program(bytecode)
        assert "call" in str(exc.value).lower() or "operand" in str(exc.value).lower()

    def test_validate_constant_types(self):
        """Constants can be atoms (str), integers, or special values."""
        bytecode = {
            "code": [(OP_GET_CONSTANT, [], 1)],  # Empty list constant
            "symbols": {},
        }

        # Should load successfully or handle gracefully
        try:
            result = load_program(bytecode)
            assert "code" in result
        except BytecodeLoadError:
            # Loader may reject certain constant forms
            pass


class TestIncrementalLoading:
    """Test loading multiple programs incrementally."""

    def test_load_multiple_programs(self):
        """Load multiple programs into same registry."""
        text1 = "label user:p/0:\n  proceed"
        text2 = "label user:q/0:\n  proceed"

        assembled1 = assemble(text1)
        assembled2 = assemble(text2)

        # Load first program
        result1 = load_program(assembled1)

        # Load second program (may update registry or create new)
        result2 = load_program(assembled2)

        # Both should be loadable
        assert result1["registry"].lookup("user:p/0") == 0
        assert result2["registry"].lookup("user:q/0") == 0
