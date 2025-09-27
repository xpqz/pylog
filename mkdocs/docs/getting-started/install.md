# Installation

PyLog requires Python 3.11 or newer.

Option A: install from source (recommended for development):

```bash
git clone https://github.com/xpqz/pylog.git
cd pylog
python -m venv .venv
source .venv/bin/activate
pip install -e .[dev]
```

Option B: run from a checkout without installing:

```bash
git clone https://github.com/xpqz/pylog.git
cd pylog
python -m prolog.repl
```

Verify with a simple query in the REPL:

```text
?- true.
true.
```

