# BORNA

BORNA is a standalone browser tool for exploring two-node Boolean networks.
The name is used in the app with a hexagonal logo.

Open:

```text
index.html
```

The tool has two modes:

- `89 presets`: the ordered 89 Boolean-network realizations used in the paper.
- `Custom rules`: direct selection of Boolean rules for `A'` and `B'`.

For each selected network/rule pair, the page shows:

- the inferred or preset signed regulatory network,
- the synchronous and BoolNet-style asynchronous truth table,
- the selected state-transition graph,
- terminal SCC / attractor states highlighted in green.

Asynchronous transitions are one-node updates:

```text
A-only: (A, B) -> (A'(A,B), B)
B-only: (A, B) -> (A, B'(A,B))
```

If `Show self-loops` is unchecked, self-loops are hidden unless a state has no
non-self outgoing asynchronous transition.
