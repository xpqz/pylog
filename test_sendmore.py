"""Test SEND+MORE problem with linear arithmetic constraints."""

import time
from prolog.engine.engine import Engine, Program

prog_text = """
% SEND+MORE cryptarithmetic problem
sendmore(S,E,N,D,M,O,R,Y) :-
    S in 0..9,
    E in 0..9,
    N in 0..9,
    D in 0..9,
    M in 0..9,
    O in 0..9,
    R in 0..9,
    Y in 0..9,
    all_different([S,E,N,D,M,O,R,Y]),
    S #> 0,
    M #> 0,
    1000*S + 100*E + 10*N + D +
    1000*M + 100*O + 10*R + E #=
    10000*M + 1000*O + 100*N + 10*E + Y,
    label([S,E,N,D,M,O,R,Y]).
"""

engine = Engine(Program(()))
engine.consult_string(prog_text)

print("Running SEND+MORE...")
start = time.perf_counter()
solutions = list(engine.query("?- sendmore(S,E,N,D,M,O,R,Y)."))
elapsed = time.perf_counter() - start

print(f"Found {len(solutions)} solution(s) in {elapsed:.3f}s")

# Look for the correct solution
correct_found = False
for sol in solutions:
    s = sol['S'].value
    e = sol['E'].value
    n = sol['N'].value
    d = sol['D'].value
    m = sol['M'].value
    o = sol['O'].value
    r = sol['R'].value
    y = sol['Y'].value

    send = 1000*s + 100*e + 10*n + d
    more = 1000*m + 100*o + 10*r + e
    money = 10000*m + 1000*o + 100*n + 10*e + y

    if send + more == money:
        correct_found = True
        print(f"✓ Found correct solution: S={s}, E={e}, N={n}, D={d}, M={m}, O={o}, R={r}, Y={y}")
        print(f"  {send} + {more} = {money}")
        break

if not correct_found:
    print("X No correct solution found!")

if solutions and False:
    sol = solutions[0]
    print(f"Solution: S={sol['S'].value}, E={sol['E'].value}, N={sol['N'].value}, D={sol['D'].value}")
    print(f"         M={sol['M'].value}, O={sol['O'].value}, R={sol['R'].value}, Y={sol['Y'].value}")

    s = sol['S'].value
    e = sol['E'].value
    n = sol['N'].value
    d = sol['D'].value
    m = sol['M'].value
    o = sol['O'].value
    r = sol['R'].value
    y = sol['Y'].value

    send = 1000*s + 100*e + 10*n + d
    more = 1000*m + 100*o + 10*r + e
    money = 10000*m + 1000*o + 100*n + 10*e + y

    print(f"\nVerification:")
    print(f"  {send:4d} (SEND)")
    print(f"+ {more:4d} (MORE)")
    print(f"------")
    print(f"{money:5d} (MONEY)")
    print(f"\n{'✓ Correct!' if send + more == money else 'X FAILED!'}")