# BORNA Final Reproducible Code

BORNA stands for **Boolean Regulatory Network Analyzer**. This repository
contains the final reproducible Python code used to generate the two-node
Boolean-network catalogue, state-transition graphs (STGs), feature matrices,
cluster assignments, figures, and tables used in the final manuscript.

This is the **code-first** version of the analysis. It does **not** depend on the
BORNA web app and does **not** read an app-exported JSON file. Instead, the final
39 connected signed graph classes and the final 89 Boolean Network realizations
are generated directly from the Python source code.

## What This Code Generates

The main script starts from signed two-node regulatory graphs with nodes `A` and
`B`. Each directed edge can be:

- `+1`: activation
- `-1`: inhibition
- `0`: no edge

The four possible edge positions are:

- `A -> A`: self-regulation of `A`
- `A -> B`: regulation of `B` by `A`
- `B -> A`: regulation of `A` by `B`
- `B -> B`: self-regulation of `B`

Only connected two-node graphs are kept, meaning each graph must contain at
least one cross-node interaction between `A` and `B`. Graphs with only isolated
self-loops are excluded. After removing isomorphic duplicates, the final
catalogue contains:

- **39 connected signed graph classes**
- **89 Boolean Network realizations**

Each Boolean Network is assigned a final manuscript-order ID:

```text
N01, N02, ..., N89
```

and a class/graph label:

```text
C<class>-G<graph_in_class>
```

For example:

```text
N29 = C5-G1, rule A' = A | B, rule B' = A
```

## Main Script

```text
borna_generate_final_outputs.py
```

This is the only script needed to regenerate the final data products.

Run:

```bash
pip install -r requirements.txt
python borna_generate_final_outputs.py
```

If your local environment has Python-version/package conflicts, Python 3.12 was
used successfully during final testing:

```bash
python3.12 borna_generate_final_outputs.py
```

## Pipeline Overview

The script performs the full analysis in the following order.

### 1. Generate the 39 connected signed graphs

The function `final_graph_catalogue()` defines the final connected graph
catalogue. Each graph includes:

- structural class number
- graph number inside that class
- signed adjacency entries
- human-readable graph description

The graph classes correspond to increasing structural complexity:

- Class 1: one cross edge
- Class 2: reciprocal cross edges
- Class 3: one self-loop plus an outgoing cross edge
- Class 4: one self-loop plus an incoming cross edge
- Class 5: one self-loop plus reciprocal cross edges
- Class 6: two self-loops plus one cross edge
- Class 7: two self-loops plus reciprocal cross edges

### 2. Generate the 89 Boolean Networks

The function `generate_final_network_catalogue()` applies the final AND/OR rule
conventions to the 39 graphs. This produces the final 89 Boolean Network
realizations in the exact order used by the manuscript and supplementary files.

The rule code means:

- `AA`: A-rule uses AND, B-rule uses AND
- `AO`: A-rule uses AND, B-rule uses OR
- `OA`: A-rule uses OR, B-rule uses AND
- `OO`: A-rule uses OR, B-rule uses OR

Only the final manuscript rule combinations are used for each structural class.

### 3. Build synchronous STGs

In synchronous updating, both Boolean rules are evaluated at the same time:

```text
(A, B) -> (A', B')
```

Each of the four states:

```text
00, 01, 10, 11
```

has exactly one synchronous successor. The output is a deterministic STG.

### 4. Build asynchronous STGs

The asynchronous STG follows the BoolNet-style one-node update definition.
From the current state, only one node is updated at a time:

```text
update A only: (A, B) -> (A', B)
update B only: (A, B) -> (A, B')
```

The new state becomes the current state for the next asynchronous update.
Therefore, asynchronous trajectories are sequential paths inside the
asynchronous STG.

The asynchronous STG contains the set of all possible one-node transitions from
each current state. Self-loops are removed when a state has at least one
non-self outgoing transition. A self-loop is retained only when no non-self
outgoing transition exists, so fixed points remain visible.

### 5. Extract STG features

The code extracts numerical descriptors from both synchronous and asynchronous
STGs. These include attractor, basin, transient, degree, entropy, sensitivity,
and spectral features.

The final retained feature counts used for clustering are:

- Synchronous: 20 retained nonconstant STG features
- Asynchronous: 39 retained nonconstant STG features

Catalogue metadata such as network ID, class, graph number, and edge signs are
not used as clustering features.

### 6. Cluster and plot

The script robust-scales the retained features using median/IQR scaling, then
runs:

- PCA
- t-SNE
- UMAP
- K-means clustering with `K = 4`

The resulting plots reproduce the final manuscript Figures 4 and 5.

### 7. Validate final aggregate results

The script checks that the regenerated data match the final manuscript
aggregate results.

Expected synchronous attractor-type totals:

| Type | Count |
|---|---:|
| Stable fixed-point | 22 |
| Pure oscillatory | 28 |
| Complex composite | 23 |
| Hybrid multimodal | 16 |

Expected asynchronous attractor-type totals:

| Type | Count |
|---|---:|
| Single fixed point | 29 |
| Multi-attractor fixed-point | 25 |
| Cyclic / trap dominated | 28 |
| Mixed fixed + trap | 7 |

Expected asynchronous number-of-attractors distribution:

| Number of attractors | Networks |
|---:|---:|
| 1 | 57 |
| 2 | 25 |
| 3 | 7 |

If any of these values change, the script raises an error.

## Output Folder

All generated files are written to:

```text
BORNA_GITHUB_FINAL_RESULTS/
```

## Output Files Explained

### Network catalogue

```text
borna_final_89_network_catalogue.csv
```

Contains one row per Boolean Network. This is the master catalogue of the 89
networks, including:

- network ID
- class
- graph number
- signed edge values
- rule code
- Boolean rules for `A` and `B`

### Truth tables

```text
borna_final_sync_truth_table_89.csv
borna_final_async_truth_table_89.csv
```

The synchronous truth table gives the synchronous next state for every network
and every starting state.

The asynchronous truth table gives two one-node-update outcomes for every
network and every starting state:

- `A_update_only`
- `B_update_only`

All state labels are stored as strings (`00`, `01`, `10`, `11`) so leading zeros
are preserved.

### STG transition tables

```text
borna_final_sync_stg_transitions_89.csv
borna_final_async_stg_transitions_89.csv
```

These files contain the final STG edges.

The synchronous transition file has one edge per state per network.

The asynchronous transition file contains all retained one-node update edges
after applying the self-loop rule described above.

### Feature matrices

```text
borna_final_sync_features_89.csv
borna_final_async_features_89.csv
```

These contain the raw STG-derived feature matrices before scaling.

Final shapes:

- synchronous raw features: `89 x 34`
- asynchronous raw features: `89 x 56`

These include identifying metadata and attractor-type labels.

### Scaled feature matrices

```text
borna_final_sync_scaled_features.csv
borna_final_async_scaled_features.csv
```

These contain the robust-scaled retained nonconstant numerical features used for
PCA, t-SNE, UMAP, and K-means.

Final shapes:

- synchronous scaled matrix: `89 x 21`
- asynchronous scaled matrix: `89 x 40`

The first column is `network_id`; the remaining columns are retained scaled
features.

### Cluster assignments

```text
borna_final_sync_cluster_assignments.csv
borna_final_async_cluster_assignments.csv
```

These map each of the 89 networks to its final K-means cluster and attractor
type.

### Embedding coordinates

```text
borna_final_sync_embedding_coordinates.csv
borna_final_async_embedding_coordinates.csv
```

These contain the PCA, t-SNE, and UMAP coordinates for every network.

### Figures

```text
borna_final_figure4_sync_pca_tsne_umap.png
borna_final_figure5_async_pca_tsne_umap.png
```

These are the final clustering plots used for the synchronous and asynchronous
analyses.

### Tables

```text
borna_final_table2_sync_cluster_attractor_types.csv
borna_final_table3_async_cluster_attractor_types.csv
borna_final_tables_2_3_latex.tex
```

These reproduce the corrected manuscript Tables 2 and 3.

### STG catalogue PDF

```text
borna_final_sync_async_stg_catalogue_89.pdf
```

This PDF has exactly 89 pages, one page per Boolean Network. Each page shows:

- signed regulatory graph
- synchronous STG
- asynchronous STG
- Boolean rules
- synchronous attractor summary
- asynchronous attractor summary

## Requirements

See:

```text
requirements.txt
```

Main packages:

- `numpy`
- `pandas`
- `networkx`
- `matplotlib`
- `scikit-learn`
- `umap-learn==0.5.6`

## Reproducibility Notes

Randomized methods use fixed seeds:

- K-means: `random_state=42`
- t-SNE: `random_state=42`
- UMAP: `random_state=42`

Because t-SNE and UMAP can be sensitive to package versions, the exact
coordinates may vary slightly across environments, but the final script was
tested locally and regenerated the manuscript-level cluster counts and tables.

## Important Conceptual Distinction

The STG and a random asynchronous trajectory are not the same thing.

The asynchronous STG is the full graph of all possible one-node updates from
each state. A trajectory is one possible path through that STG, depending on
which node is selected at each step.

This final GitHub script generates the STG-level catalogue and STG-derived
features used in the manuscript.
