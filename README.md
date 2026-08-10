# BORNA — BOolean network Reduction via state traNsition Graph Analysis

**BORNA** is an interactive web application for exploring two-node signed Boolean networks and their state-transition graph (STG) dynamics. It was developed alongside the accompanying research study to make the complete network catalogue and its dynamical behaviors accessible for interactive exploration.

The application runs directly in a web browser and requires no installation or external dependencies.

## Features

BORNA provides two main exploration modes:

* **89 Presets** — the complete set of 89 Boolean-network realizations analyzed in the study.
* **Custom Rules** — interactive selection of Boolean update rules for nodes (A) and (B).

For each selected network, BORNA provides:

* The corresponding signed regulatory network structure.
* The Boolean update rules for (A') and (B').
* The synchronous truth table and state-transition graph.
* The asynchronous state-transition graph using one-node-at-a-time updating.
* Attractor and terminal strongly connected component (SCC) information.
* Interactive visualization of network states and transitions.
* Optional display of self-loops.

## Synchronous dynamics

For a Boolean network with two nodes, the state space is

[
S = {00,01,10,11}.
]

Under synchronous updating, both nodes are updated simultaneously according to

[
(A,B) \rightarrow (A'(A,B), B'(A,B)).
]

This produces a deterministic state-transition graph in which every state has exactly one successor.

## Asynchronous dynamics

BORNA also implements one-node-at-a-time asynchronous updating. From a state ((A,B)), two possible updates are considered:

```text
A-only: (A, B) -> (A'(A,B), B)
B-only: (A, B) -> (A, B'(A,B))
```

Consequently, the asynchronous STG can be nondeterministic, with a state potentially having more than one successor.

Terminal strongly connected components (SCCs) are used to characterize the terminal dynamical structures of the asynchronous STG.

## Self-loops

BORNA provides an option to show or hide self-loops.

When **Show self-loops** is disabled, self-loops are hidden whenever a state has at least one non-self outgoing asynchronous transition. Self-loops are retained when no non-self asynchronous transition is available, preserving fixed-point states while avoiding visually redundant transitions.

## 89-network catalogue

The **89 Presets** correspond to the Boolean-network realizations systematically enumerated and analyzed in the accompanying study. Each realization is associated with its signed regulatory structure, Boolean update rules, and corresponding synchronous and asynchronous dynamical representations.

This catalogue allows users to explore how different network structures and logical rules give rise to distinct or shared dynamical behaviors.

## Running BORNA

BORNA is a standalone client-side application.

To run it locally:

1. Clone or download this repository.
2. Open `index.html` in a modern web browser.

No server, Python environment, package manager, or additional installation is required.

## Repository structure

```text
.
├── index.html
├── ...
└── README.md
```

The main application is contained in `index.html` together with the supporting files included in this repository.

## Research context

BORNA was developed as a companion resource for the study:

> **[Add manuscript title here]**

The application provides an interactive way to explore the two-node Boolean-network catalogue, state-transition graphs, attractor structures, and the differences between synchronous and asynchronous updating.

## Citation

If you use BORNA or the associated network catalogue in your research, please cite the accompanying publication:

> **[Add citation here]**

## License

[Add license information here.]

## Contact

For questions, suggestions, or issues related to BORNA, please open an issue in this repository or contact the authors through the information provided in the accompanying publication.
