"""Unit tests for WAM text assembler.

Tests parsing text assembly format to bytecode tuples.
"""

import pytest
from prolog.wam.asm import AssemblerError, assemble, parse_instruction, parse_line
from prolog.wam.instructions import (
    OP_GET_VARIABLE,
    OP_GET_VALUE,
    OP_GET_CONSTANT,
    OP_GET_STRUCTURE,
    OP_PUT_VARIABLE,
    OP_PUT_VALUE,
    OP_PUT_CONSTANT,
    OP_PUT_STRUCTURE,
    OP_UNIFY_VARIABLE,
    OP_UNIFY_VALUE,
    OP_UNIFY_CONSTANT,
    OP_CALL,
    OP_EXECUTE,
    OP_PROCEED,
    OP_ALLOCATE,
    OP_DEALLOCATE,
    OP_TRY_ME_ELSE,
    OP_RETRY_ME_ELSE,
    OP_TRUST_ME,
)


class TestBasicInstructions:
    """Test parsing basic WAM instructions."""

    def test_parse_proceed(self):
        """Parse simple proceed instruction."""

        result = parse_instruction("proceed")
        assert result == (OP_PROCEED,)

    def test_parse_allocate(self):
        """Parse allocate with count."""

        result = parse_instruction("allocate 2")
        assert result == (OP_ALLOCATE, 2)

    def test_parse_deallocate(self):
        """Parse deallocate instruction."""

        result = parse_instruction("deallocate")
        assert result == (OP_DEALLOCATE,)


class TestGetInstructions:
    """Test parsing get-family instructions."""

    def test_parse_get_variable_x(self):
        """Parse get_variable with X register."""

        result = parse_instruction("get_variable X1, A2")
        assert result == (OP_GET_VARIABLE, 1, 2)

    def test_parse_get_variable_y(self):
        """Parse get_variable with Y register."""

        # Note: For now, assembler may emit integer indices
        # Y-register handling TBD based on operand format resolution
        result = parse_instruction("get_variable Y0, A1")
        # Could be (OP_GET_VARIABLE, ("Y", 0), 1) or special handling
        assert result[0] == OP_GET_VARIABLE
        assert result[2] == 1  # Argument register

    def test_parse_get_value(self):
        """Parse get_value instruction."""

        result = parse_instruction("get_value X3, A1")
        assert result == (OP_GET_VALUE, 3, 1)

    def test_parse_get_constant_atom(self):
        """Parse get_constant with atom."""

        result = parse_instruction("get_constant foo, A1")
        assert result == (OP_GET_CONSTANT, "foo", 1)

    def test_parse_get_constant_integer(self):
        """Parse get_constant with integer."""

        result = parse_instruction("get_constant 42, A2")
        assert result == (OP_GET_CONSTANT, 42, 2)

    def test_parse_get_constant_empty_list(self):
        """Parse get_constant with empty list."""

        result = parse_instruction("get_constant [], A1")
        assert result == (OP_GET_CONSTANT, "[]", 1)

    def test_parse_get_structure(self):
        """Parse get_structure with functor."""

        result = parse_instruction("get_structure f/2, A1")
        assert result == (OP_GET_STRUCTURE, ("f", 2), 1)

    def test_parse_get_structure_dot(self):
        """Parse get_structure with dot functor (list)."""

        result = parse_instruction("get_structure ./2, A1")
        assert result == (OP_GET_STRUCTURE, (".", 2), 1)


class TestPutInstructions:
    """Test parsing put-family instructions."""

    def test_parse_put_variable(self):
        """Parse put_variable instruction."""

        result = parse_instruction("put_variable X0, A1")
        assert result == (OP_PUT_VARIABLE, 0, 1)

    def test_parse_put_value(self):
        """Parse put_value instruction."""

        result = parse_instruction("put_value X2, A3")
        assert result == (OP_PUT_VALUE, 2, 3)

    def test_parse_put_constant(self):
        """Parse put_constant instruction."""

        result = parse_instruction("put_constant hello, A1")
        assert result == (OP_PUT_CONSTANT, "hello", 1)

    def test_parse_put_structure(self):
        """Parse put_structure instruction."""

        result = parse_instruction("put_structure g/1, X5")
        assert result == (OP_PUT_STRUCTURE, ("g", 1), 5)


class TestUnifyInstructions:
    """Test parsing unify-family instructions."""

    def test_parse_unify_variable(self):
        """Parse unify_variable instruction."""

        result = parse_instruction("unify_variable X1")
        assert result == (OP_UNIFY_VARIABLE, 1)

    def test_parse_unify_value(self):
        """Parse unify_value instruction."""

        result = parse_instruction("unify_value X0")
        assert result == (OP_UNIFY_VALUE, 0)

    def test_parse_unify_constant(self):
        """Parse unify_constant instruction."""

        result = parse_instruction("unify_constant a")
        assert result == (OP_UNIFY_CONSTANT, "a")


class TestControlInstructions:
    """Test parsing control-flow instructions."""

    def test_parse_call(self):
        """Parse call instruction."""

        result = parse_instruction("call user:q/1")
        assert result == (OP_CALL, "user:q/1")

    def test_parse_execute(self):
        """Parse execute instruction."""

        result = parse_instruction("execute user:r/2")
        assert result == (OP_EXECUTE, "user:r/2")

    def test_parse_try_me_else(self):
        """Parse try_me_else with label."""

        result = parse_instruction("try_me_else L1")
        assert result == (OP_TRY_ME_ELSE, "L1")

    def test_parse_retry_me_else(self):
        """Parse retry_me_else with label."""

        result = parse_instruction("retry_me_else L2")
        assert result == (OP_RETRY_ME_ELSE, "L2")

    def test_parse_trust_me(self):
        """Parse trust_me instruction."""

        result = parse_instruction("trust_me")
        assert result == (OP_TRUST_ME,)


class TestModuleAndLabels:
    """Test parsing module directives and labels."""

    def test_parse_module_directive(self):
        """Parse module directive."""

        result = parse_line("module user.")
        assert result == {"type": "module", "name": "user"}

    def test_parse_label_simple(self):
        """Parse simple label."""

        result = parse_line("label L1:")
        assert result == {"type": "label", "name": "L1"}

    def test_parse_label_predicate(self):
        """Parse predicate label."""

        result = parse_line("label user:append/3:")
        assert result == {"type": "label", "name": "user:append/3"}

    def test_parse_comment(self):
        """Comments are ignored."""

        result = parse_line("  # This is a comment")
        assert result is None or result == {"type": "comment"}

    def test_parse_empty_line(self):
        """Empty lines are ignored."""

        result = parse_line("")
        assert result is None

        result = parse_line("   ")
        assert result is None


class TestProgramParsing:
    """Test parsing complete assembly programs."""

    def test_parse_simple_fact(self):
        """Parse assembly for simple fact."""

        text = """
module user.

label user:p/1:
  get_constant a, A1
  proceed
"""
        result = assemble(text)
        assert "code" in result
        assert "symbols" in result
        assert result["symbols"]["user:p/1"] == 0
        assert result["code"][0] == (OP_GET_CONSTANT, "a", 1)
        assert result["code"][1] == (OP_PROCEED,)

    def test_parse_simple_rule(self):
        """Parse assembly for simple rule."""

        text = """
label user:p/1:
  get_variable X0, A1
  put_value X0, A1
  call user:q/1
  proceed
"""
        result = assemble(text)
        code = result["code"]
        assert len(code) == 4
        assert code[0] == (OP_GET_VARIABLE, 0, 1)
        assert code[1] == (OP_PUT_VALUE, 0, 1)
        assert code[2] == (OP_CALL, "user:q/1")
        assert code[3] == (OP_PROCEED,)

    def test_parse_with_structure(self):
        """Parse assembly with structure."""

        text = """
label user:p/1:
  get_structure f/2, A1
    unify_variable X1
    unify_constant a
  proceed
"""
        result = assemble(text)
        code = result["code"]
        assert code[0] == (OP_GET_STRUCTURE, ("f", 2), 1)
        assert code[1] == (OP_UNIFY_VARIABLE, 1)
        assert code[2] == (OP_UNIFY_CONSTANT, "a")
        assert code[3] == (OP_PROCEED,)

    def test_parse_with_lco(self):
        """Parse assembly with last call optimization."""

        text = """
label user:p/0:
  call user:q/0
  execute user:r/0
"""
        result = assemble(text)
        code = result["code"]
        assert code[0] == (OP_CALL, "user:q/0")
        assert code[1] == (OP_EXECUTE, "user:r/0")


class TestLabelResolution:
    """Test label resolution to code offsets."""

    def test_resolve_simple_label(self):
        """Resolve simple forward label reference."""

        text = """
label user:p/0:
  try_me_else L1
  proceed

label L1:
  trust_me
  proceed
"""
        result = assemble(text)
        code = result["code"]
        # try_me_else L1 should resolve to offset 2 (where L1 is)
        assert code[0] == (OP_TRY_ME_ELSE, 2)
        assert code[1] == (OP_PROCEED,)
        assert code[2] == (OP_TRUST_ME,)
        assert code[3] == (OP_PROCEED,)

    def test_resolve_multiple_labels(self):
        """Resolve multiple label references."""

        text = """
label user:append/3:
  try_me_else L1
  get_constant [], A1
  proceed

label L1:
  retry_me_else L2
  get_structure ./2, A1
  proceed

label L2:
  trust_me
  proceed
"""
        result = assemble(text)
        code = result["code"]
        # try_me_else -> offset 3 (L1)
        assert code[0] == (OP_TRY_ME_ELSE, 3)
        # retry_me_else -> offset 6 (L2)
        assert code[3] == (OP_RETRY_ME_ELSE, 6)

    def test_symbol_table_multiple_predicates(self):
        """Symbol table tracks multiple predicate entry points."""

        text = """
label user:p/1:
  proceed

label user:q/2:
  get_variable X0, A1
  proceed
"""
        result = assemble(text)
        assert result["symbols"]["user:p/1"] == 0
        assert result["symbols"]["user:q/2"] == 1


class TestIndentation:
    """Test handling of indentation."""

    def test_indented_instructions(self):
        """Instructions can be indented."""

        text = """
label user:p/0:
    proceed
"""
        result = assemble(text)
        assert result["code"][0] == (OP_PROCEED,)

    def test_nested_unify_indented(self):
        """Unify instructions typically indented under structure."""

        text = """
label user:p/1:
  get_structure f/1, A1
    unify_variable X0
  proceed
"""
        result = assemble(text)
        code = result["code"]
        assert code[0] == (OP_GET_STRUCTURE, ("f", 1), 1)
        assert code[1] == (OP_UNIFY_VARIABLE, 0)


class TestErrorCases:
    """Test error handling in assembler."""

    def test_unknown_opcode(self):
        """Unknown opcode raises error."""

        text = """
label user:p/0:
  invalid_opcode
"""
        with pytest.raises(AssemblerError, match="unknown.*opcode"):
            assemble(text)

    def test_malformed_register(self):
        """Malformed register notation raises error."""

        text = """
label user:p/0:
  get_variable Z99, A1
"""
        with pytest.raises(AssemblerError, match="register"):
            assemble(text)

    def test_unresolved_label(self):
        """Unresolved label reference raises error."""

        text = """
label user:p/0:
  try_me_else MISSING_LABEL
"""
        with pytest.raises(AssemblerError, match="unresolved.*label"):
            assemble(text)

    def test_duplicate_label(self):
        """Duplicate label definition raises error."""

        text = """
label L1:
  proceed

label L1:
  proceed
"""
        with pytest.raises(AssemblerError, match="duplicate.*label"):
            assemble(text)


class TestRegisterNotation:
    """Test different register notations."""

    def test_x_register_zero(self):
        """X0 is valid."""

        result = parse_instruction("get_variable X0, A1")
        assert result == (OP_GET_VARIABLE, 0, 1)

    def test_x_register_high(self):
        """High X register indices."""

        result = parse_instruction("put_value X99, A1")
        assert result == (OP_PUT_VALUE, 99, 1)

    def test_y_register(self):
        """Y register notation."""

        result = parse_instruction("get_variable Y5, A2")
        assert result[0] == OP_GET_VARIABLE
        # Y-register format TBD based on operand resolution

    def test_argument_register(self):
        """Argument register notation A0-A9."""

        result = parse_instruction("get_constant foo, A0")
        assert result == (OP_GET_CONSTANT, "foo", 0)


class TestConstantParsing:
    """Test parsing different constant types."""

    def test_atom_simple(self):
        """Simple atom constant."""

        result = parse_instruction("get_constant hello, A1")
        assert result == (OP_GET_CONSTANT, "hello", 1)

    def test_atom_with_underscore(self):
        """Atom with underscores."""

        result = parse_instruction("get_constant foo_bar, A1")
        assert result == (OP_GET_CONSTANT, "foo_bar", 1)

    def test_integer_positive(self):
        """Positive integer constant."""

        result = parse_instruction("put_constant 123, A1")
        assert result == (OP_PUT_CONSTANT, 123, 1)

    def test_integer_negative(self):
        """Negative integer constant."""

        result = parse_instruction("put_constant -42, A2")
        assert result == (OP_PUT_CONSTANT, -42, 2)

    def test_empty_list_constant(self):
        """Empty list constant []."""

        result = parse_instruction("unify_constant []")
        assert result == (OP_UNIFY_CONSTANT, "[]")


class TestFunctorParsing:
    """Test parsing functor/arity notation."""

    def test_functor_simple(self):
        """Simple functor notation."""

        result = parse_instruction("get_structure foo/2, A1")
        assert result == (OP_GET_STRUCTURE, ("foo", 2), 1)

    def test_functor_zero_arity(self):
        """Zero-arity functor."""

        result = parse_instruction("put_structure bar/0, X1")
        assert result == (OP_PUT_STRUCTURE, ("bar", 0), 1)

    def test_functor_dot(self):
        """Dot functor for lists."""

        result = parse_instruction("get_structure ./2, A1")
        assert result == (OP_GET_STRUCTURE, (".", 2), 1)

    def test_functor_special_chars(self):
        """Functor with special characters."""

        result = parse_instruction("get_structure '+'/2, A1")
        assert result == (OP_GET_STRUCTURE, ("+", 2), 1)
