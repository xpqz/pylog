from prolog.engine.engine import Engine, Program
from prolog.parser.reader import Reader


def test_boolean_first_vs_integer_first_labeling_complete():
    r = Reader()

    # Boolean-first labeling
    q1 = r.read_term(
        """
        X in 1..5, Y in 1..5,
        B1 #<=> (X #< Y),
        B2 #<=> (X #= Y),
        B3 #<=> (X #> Y),
        label([B1, B2, B3, X, Y])
        """
    )
    e1 = Engine(Program(()))
    sols1 = list(e1.solve(q1))
    assert len(sols1) == 25

    # Integer-first labeling
    q2 = r.read_term(
        """
        X in 1..5, Y in 1..5,
        B1 #<=> (X #< Y),
        B2 #<=> (X #= Y),
        B3 #<=> (X #> Y),
        label([X, Y, B1, B2, B3])
        """
    )
    e2 = Engine(Program(()))
    sols2 = list(e2.solve(q2))
    assert len(sols2) == 25
