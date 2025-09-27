# Reification

Reification links the truth of a constraint to a Boolean variable (0 or 1), enabling
complex constraint modeling where the satisfaction of constraints can be used as
conditions in logical expressions.

PyLog fully implements the standard CLP(FD) reification operators:

- `B #<=> C` — equivalence: B is 1 iff constraint C holds, B is 0 iff C fails
- `B #==> C` — forward implication: if B is 1 then C must hold
- `B #<== C` — backward implication: if C holds then B must be 1

## Operator Precedence

- All three are non‑associative `xfx` at precedence 900
- They bind tighter than `,` and `;`, but looser than arithmetic comparisons
- Chaining requires parentheses: `B1 #<=> (B2 #<=> B3)`

## Basic Examples

```prolog
% B is 1 if X equals Y, 0 otherwise
?- B in 0..1, B #<=> (X #= Y).

% If B is 1, then X must be less than 5
?- B in 0..1, B #==> (X #< 5).

% If Y is greater than 0, then B must be 1
?- B in 0..1, B #<== (Y #> 0).
```

## Advanced Constraint Modeling

Reification enables encoding complex logical conditions:

```prolog
% Exclusive OR: exactly one of X=1 or Y=1
?- X in 0..1, Y in 0..1, B in 0..1,
   B #<=> ((X #= 1) #\ (Y #= 1)).

% Conditional constraints: if A then X>5, else X<3
?- A in 0..1, X in 0..10,
   A #==> (X #> 5),
   (1-A) #==> (X #< 3).

% Count satisfied constraints
?- B1 #<=> (X #> 0),
   B2 #<=> (Y #> 0),
   B3 #<=> (Z #> 0),
   Sum #= B1 + B2 + B3.  % Sum is number of positive variables
```

## Entailment Detection

PyLog automatically detects when constraints are entailed (always true) or
disentailed (always false) based on current domain bounds:

```prolog
% Immediate entailment when domains determine truth
?- X in 5..10, B #<=> (X #> 3).
% B = 1 (entailed: X is always > 3)

?- X in 1..2, B #<=> (X #> 5).
% B = 0 (disentailed: X can never be > 5)
```

## Performance Notes

- Boolean variables are automatically constrained to domain {0,1}
- Entailment detection avoids unnecessary propagator creation
- Reification integrates with the propagation queue for efficient constraint solving

See also:

- Reference → Operators table
- CLP(FD) → Linear Constraints

