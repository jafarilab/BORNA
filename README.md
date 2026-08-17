# BORNA

<p align="center">
  <img src="logo-transparent.png" alt="BORNA logo" width="240">
</p>

<p align="center">
  <strong>BOolean network two-node dynamics via state-tRansition graph aNalysis</strong>
</p>

<p align="center">
  An interactive explorer of two-node Boolean network dynamics and state-transition graphs.
</p>

**BORNA** is a standalone browser tool for exploring two-node signed Boolean networks and their state-transition graph (STG) dynamics. It was developed as a companion tool for the associated research study and provides an interactive way to explore the complete catalogue of two-node Boolean-network realizations and their dynamical behaviors.

The application runs directly in a web browser and requires no installation or external dependencies.

## Online Application

The latest version of BORNA is available at:

**https://jafarilab.github.io/BORNA/**

## Running Locally

To run BORNA locally, clone or download this repository and open:

```text
index.html
```

No server, Python environment, package manager, or additional installation is required.

## Features

The tool has two exploration modes:

- **89 presets** — the ordered 89 Boolean-network realizations used in the paper.
- **Custom rules** — direct selection of Boolean rules for `A'` and `B'`.

For each selected network/rule pair, the page shows:

- the inferred or preset signed regulatory network,
- the Boolean update rules for nodes `A` and `B`,
- the synchronous and BoolNet-style asynchronous truth table,
- the selected state-transition graph,
- terminal SCC / attractor states highlighted in green,
- optional self-loops.

## Synchronous Dynamics

For synchronous updating, both nodes are updated simultaneously:

```text
(A, B) -> (A'(A,B), B'(A,B))
```

For the two-node network, the state space consists of four possible states:

```text
00, 01, 10, 11
```

Each state therefore has one synchronous successor.

## Asynchronous Dynamics

BORNA also supports one-node-at-a-time asynchronous updating. From a given state, either node can be updated:

```text
A-only: (A, B) -> (A'(A,B), B)
B-only: (A, B) -> (A, B'(A,B))
```

This can produce a nondeterministic state-transition graph in which a state may have more than one possible successor.

Terminal strongly connected components (SCCs) are used to identify terminal dynamical structures and attractor states.

## Self-Loops

BORNA provides a **Show self-loops** option.

When the option is disabled, self-loops are hidden whenever a state has at least one non-self outgoing asynchronous transition. Self-loops are retained when no non-self asynchronous transition is available, preserving fixed-point states while reducing unnecessary visual clutter.

## The 89 Presets

The 89 presets represent the ordered Boolean-network realizations analyzed in the accompanying study. Each realization is associated with its regulatory structure, Boolean update rules, and corresponding synchronous and asynchronous dynamics.

The catalogue allows users to examine how different Boolean rules and signed network structures give rise to different state-transition graphs and attractor behaviors.

## Repository Structure

```text
.
├── index.html
├── styles.css
├── app.js
├── logo-transparent.png
└── README.md
```

- `index.html` — main application interface.
- `styles.css` — application styling and layout.
- `app.js` — Boolean-network logic, STG generation, and visualization.
- `logo-transparent.png` — BORNA logo.
- `README.md` — project documentation.

## Data and Code Availability

The complete network catalogue and source code for the BORNA application are available in this repository:

**https://github.com/jafarilab/BORNA**

The repository contains the code used for Boolean rule generation, network enumeration, synchronous and asynchronous state-transition graph analysis, attractor identification, and interactive visualization.

## Research Context

BORNA was developed as a companion resource for the associated research study on two-node Boolean network dynamics.

> **Reducing Boolean Networks via Analysis of Dynamic Network Subgraph Behavior**

## Citation

If you use BORNA or the associated network catalogue in your research, please cite the accompanying publication:

> **[Add publication citation here]**

## License

CC BY-NC-ND

## Contact

For questions, suggestions, or issues related to BORNA, please open an issue in this repository.
