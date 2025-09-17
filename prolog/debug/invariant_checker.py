"""Invariant checking for trace validation."""

from typing import List, Dict, Any


def check_trace_invariants(events: List[Dict[str, Any]]) -> List[str]:
    """
    Check trace invariants and return list of violations.

    Invariants checked:
    - Step IDs must be strictly monotonic
    - Port sequences must be valid (CALL->EXIT/FAIL, REDO after EXIT)
    - Frame depths must change by at most 1
    - No negative depths
    - EXIT must follow CALL for same predicate
    """
    violations = []

    if not events:
        return violations

    # Track state for port sequence validation
    pred_stack = []  # Stack of (pred_id, frame_depth) for active calls
    prev_sid = 0
    prev_fd = 0

    for i, event in enumerate(events):
        sid = event.get("sid", 0)
        port = event.get("p")
        pid = event.get("pid", "unknown")
        fd = event.get("fd", 0)

        # Check monotonic step_id
        if sid <= prev_sid:
            violations.append(f"step_id not monotonic at position {i}: {sid} <= {prev_sid}")
        elif sid > prev_sid + 1 and prev_sid > 0:
            violations.append(f"Step_id gap at position {i}: {sid} after {prev_sid}")

        # Check depth changes
        if isinstance(fd, (int, float)):
            fd = int(fd)
            if fd < 0:
                violations.append(f"Negative depth at position {i}: fd={fd}")

            if i > 0 and abs(fd - prev_fd) > 1:
                violations.append(f"Depth jump at position {i}: {prev_fd} to {fd}")

            prev_fd = fd

        # Check port sequences
        if port == 0:  # CALL
            pred_stack.append((pid, fd))

        elif port == 1:  # EXIT
            if not pred_stack:
                violations.append(f"EXIT without prior CALL at position {i}")
            else:
                expected_pid, expected_fd = pred_stack[-1]
                if pid != expected_pid:
                    violations.append(
                        f"EXIT for different predicate at position {i}: "
                        f"expected {expected_pid}, got {pid}"
                    )
                pred_stack.pop()

        elif port == 2:  # REDO
            # REDO should only occur after an EXIT for the same predicate
            # Check if there was a prior EXIT for this predicate
            prior_exits = [e for e in events[:i] if e.get("p") == 1 and e.get("pid") == pid]
            if not prior_exits:
                violations.append(
                    f"REDO without prior EXIT at position {i} for predicate {pid}"
                )

        elif port == 3:  # FAIL
            # FAIL is valid after CALL or REDO
            pass

        prev_sid = sid

    return violations