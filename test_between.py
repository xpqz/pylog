from prolog.parser.reader import Reader
from prolog.ast.clauses import Program
from prolog.engine.engine import Engine

reader = Reader()
prog = """
    between(Low, High, Low) :- Low =< High.
    between(Low, High, X) :- Low < High, Low1 is Low + 1, between(Low1, High, X).
"""
parsed = reader.read_program(prog)
program = Program(parsed)
engine = Engine(program)
results = list(engine.query("between(1, 3, X)"))
print(f"between(1, 3, X) solutions: {results}")
