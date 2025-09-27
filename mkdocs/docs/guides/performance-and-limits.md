# Performance and limits

Tips:

- post domains early with `in/2`
- use `all_different/1` instead of many `#\=/2`
- keep constraints linear
- label only after pruning is effective

Constraints use bounds consistency. This is fast but weaker than full arc consistency. Add implied constraints if needed.

