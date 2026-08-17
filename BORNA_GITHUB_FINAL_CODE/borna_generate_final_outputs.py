#!/usr/bin/env python3
"""BORNA final GitHub-ready data-generation pipeline.

This script generates the final BORNA two-node Boolean-network data from code,
without using the web app or app-exported JSON files.

It performs the full final pipeline:
    1. Define the final 39 connected signed graph classes.
    2. Generate the final 89 Boolean Network realizations in the manuscript order.
    3. Build synchronous and BoolNet-style asynchronous STGs.
    4. Save truth tables and transition CSVs.
    5. Extract synchronous and asynchronous STG feature matrices.
    6. Run robust scaling, K-means clustering, PCA, t-SNE, and UMAP.
    7. Save Figures 4/5, Tables 2/3, LaTeX table code, and a PDF STG catalogue.

Outputs:
    BORNA_GITHUB_FINAL_RESULTS/
        borna_final_sync_truth_table_89.csv
        borna_final_async_truth_table_89.csv
        borna_final_sync_stg_transitions_89.csv
        borna_final_async_stg_transitions_89.csv
        borna_final_sync_features_89.csv
        borna_final_async_features_89.csv
        borna_final_sync_cluster_assignments.csv
        borna_final_async_cluster_assignments.csv
        borna_final_sync_scaled_features.csv
        borna_final_async_scaled_features.csv
        borna_final_sync_embedding_coordinates.csv
        borna_final_async_embedding_coordinates.csv
        borna_final_figure4_sync_pca_tsne_umap.png
        borna_final_figure5_async_pca_tsne_umap.png
        borna_final_table2_sync_cluster_attractor_types.csv
        borna_final_table3_async_cluster_attractor_types.csv
        borna_final_tables_2_3_latex.tex
        borna_final_89_network_catalogue.csv
        borna_final_sync_async_stg_catalogue_89.pdf

Numeric metadata columns are excluded from clustering so the embeddings reflect
STG dynamics rather than catalogue order.
"""

from __future__ import annotations

import json
import math
import os
import sys
import textwrap
from collections import Counter
from pathlib import Path
from typing import Any

for local_package_dir in [
    Path(__file__).resolve().parent / ".python_packages",
    Path(__file__).resolve().parent.parent / ".python_packages",
]:
    if local_package_dir.exists():
        sys.path.insert(0, str(local_package_dir))
sys.modules.setdefault("tensorflow", None)
os.environ.setdefault("MPLCONFIGDIR", str(Path(__file__).resolve().parent / ".matplotlib_cache"))

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt
import networkx as nx
import numpy as np
import pandas as pd
from matplotlib.patches import Ellipse, FancyArrowPatch
from matplotlib.backends.backend_pdf import PdfPages
from sklearn.cluster import KMeans
from sklearn.decomposition import PCA
from sklearn.impute import SimpleImputer
from sklearn.manifold import TSNE

try:
    import umap  # type: ignore
except Exception as exc:  # pragma: no cover
    raise RuntimeError("umap-learn is required. Install umap-learn==0.5.6") from exc

# Compatibility for umap-learn 0.5.6 with newer scikit-learn releases where
# check_array(force_all_finite=...) was renamed to check_array(ensure_all_finite=...).
try:  # pragma: no cover - environment compatibility only
    import inspect

    if "force_all_finite" not in inspect.signature(umap.umap_.check_array).parameters:
        _umap_check_array = umap.umap_.check_array

        def _check_array_compat(*args: Any, force_all_finite: Any = None, **kwargs: Any) -> Any:
            if force_all_finite is not None and "ensure_all_finite" not in kwargs:
                kwargs["ensure_all_finite"] = force_all_finite
            return _umap_check_array(*args, **kwargs)

        umap.umap_.check_array = _check_array_compat
except Exception:
    pass


ROOT = Path(__file__).resolve().parent
OLD_TRUTH_CANDIDATES = [
    ROOT / "TRUTH_TABLE_CHECK_89" / "sync_truth_table_89.csv",
    ROOT.parent / "TRUTH_TABLE_CHECK_89" / "sync_truth_table_89.csv",
]
OUT = ROOT / "BORNA_GITHUB_FINAL_RESULTS"
OUT.mkdir(parents=True, exist_ok=True)

STATES = ("00", "01", "10", "11")
GOOGLE_COLORS = ["#4285F4", "#DB4437", "#F4B400", "#0F9D58"]
METADATA_NUMERIC = {
    "network_id", "class", "graph_in_class", "graph_global_id",
    "edge_AA", "edge_AB", "edge_BA", "edge_BB",
}

LOGIC_TYPES = {
    "AA": ("AND", "AND"),
    "AO": ("AND", "OR"),
    "OA": ("OR", "AND"),
    "OO": ("OR", "OR"),
}

CLASS_RULE_SETS = {
    1: ["AA", "OA"],
    2: ["AA"],
    3: ["AA"],
    4: ["AA", "AO", "OA", "OO"],
    5: ["AA", "OA"],
    6: ["AA", "OA"],
    7: ["AA", "OA", "OO"],
}


def edge_dict(pos_edges: list[tuple[str, str]], neg_edges: list[tuple[str, str]]) -> dict[str, int]:
    out = {"AA": 0, "AB": 0, "BA": 0, "BB": 0}
    for source, target in pos_edges:
        out[source + target] = 1
    for source, target in neg_edges:
        out[source + target] = -1
    return out


def final_graph_catalogue() -> list[dict[str, Any]]:
    """Return the final 39 connected signed graphs in manuscript/app order.

    Edge keys:
        AA = A -> A, AB = A -> B, BA = B -> A, BB = B -> B.
        +1 = activation, -1 = inhibition, 0 = absent.
    """
    raw: list[tuple[int, int, str, list[tuple[str, str]], list[tuple[str, str]]]] = [
        # Class 1: one cross edge A -> B only.
        (1, 1, "A->B", [("A", "B")], []),
        (1, 2, "A-|B", [], [("A", "B")]),
        # Class 2: reciprocal cross edges, no self-loops.
        (2, 1, "A->B, B->A", [("A", "B"), ("B", "A")], []),
        (2, 2, "A->B, B-|A", [("A", "B")], [("B", "A")]),
        (2, 3, "A-|B, B-|A", [], [("A", "B"), ("B", "A")]),
        # Class 3: A self-loop + A -> B.
        (3, 1, "A->A, A->B", [("A", "A"), ("A", "B")], []),
        (3, 2, "A->A, A-|B", [("A", "A")], [("A", "B")]),
        (3, 3, "A-|A, A->B", [("A", "B")], [("A", "A")]),
        (3, 4, "A-|A, A-|B", [], [("A", "A"), ("A", "B")]),
        # Class 4: A self-loop + B -> A.
        (4, 1, "A->A, B->A", [("A", "A"), ("B", "A")], []),
        (4, 2, "A->A, B-|A", [("A", "A")], [("B", "A")]),
        (4, 3, "A-|A, B->A", [("B", "A")], [("A", "A")]),
        (4, 4, "A-|A, B-|A", [], [("A", "A"), ("B", "A")]),
        # Class 5: A self-loop + both cross edges.
        (5, 1, "A->A, A->B, B->A", [("A", "A"), ("A", "B"), ("B", "A")], []),
        (5, 2, "A->A, A->B, B-|A", [("A", "A"), ("A", "B")], [("B", "A")]),
        (5, 3, "A->A, A-|B, B->A", [("A", "A"), ("B", "A")], [("A", "B")]),
        (5, 4, "A->A, A-|B, B-|A", [("A", "A")], [("A", "B"), ("B", "A")]),
        (5, 5, "A-|A, A->B, B->A", [("A", "B"), ("B", "A")], [("A", "A")]),
        (5, 6, "A-|A, A->B, B-|A", [("A", "B")], [("A", "A"), ("B", "A")]),
        (5, 7, "A-|A, A-|B, B->A", [("B", "A")], [("A", "A"), ("A", "B")]),
        (5, 8, "A-|A, A-|B, B-|A", [], [("A", "A"), ("A", "B"), ("B", "A")]),
        # Class 6: both self-loops + one cross edge B -> A.
        (6, 1, "A->A, B->B, B->A", [("A", "A"), ("B", "B"), ("B", "A")], []),
        (6, 2, "A->A, B->B, B-|A", [("A", "A"), ("B", "B")], [("B", "A")]),
        (6, 3, "A->A, B-|B, B->A", [("A", "A"), ("B", "A")], [("B", "B")]),
        (6, 4, "A-|A, B->B, B-|A", [("B", "B")], [("A", "A"), ("B", "A")]),
        (6, 5, "A-|A, B->B, B->A", [("B", "B"), ("B", "A")], [("A", "A")]),
        (6, 6, "A->A, B-|B, B-|A", [("A", "A")], [("B", "B"), ("B", "A")]),
        (6, 7, "A-|A, B-|B, B->A", [("B", "A")], [("A", "A"), ("B", "B")]),
        (6, 8, "A-|A, B-|B, B-|A", [], [("A", "A"), ("B", "B"), ("B", "A")]),
        # Class 7: both self-loops + both cross edges.
        (7, 1, "A->A, B->B, A->B, B->A", [("A", "A"), ("B", "B"), ("A", "B"), ("B", "A")], []),
        (7, 2, "A->A, B->B, A->B, B-|A", [("A", "A"), ("B", "B"), ("A", "B")], [("B", "A")]),
        (7, 3, "A->A, B->B, A-|B, B-|A", [("A", "A"), ("B", "B")], [("A", "B"), ("B", "A")]),
        (7, 4, "A->A, B-|B, A->B, B-|A", [("A", "A"), ("A", "B")], [("B", "B"), ("B", "A")]),
        (7, 5, "A->A, B-|B, A-|B, B->A", [("A", "A"), ("B", "A")], [("B", "B"), ("A", "B")]),
        (7, 6, "A->A, B-|B, A-|B, B-|A", [("A", "A")], [("B", "B"), ("A", "B"), ("B", "A")]),
        (7, 7, "A-|A, B->B, A->B, B->A", [("A", "B"), ("B", "B"), ("B", "A")], [("A", "A")]),
        (7, 8, "A-|A, B-|B, A->B, B->A", [("A", "B"), ("B", "A")], [("A", "A"), ("B", "B")]),
        (7, 9, "A-|A, B-|B, A->B, B-|A", [("A", "B")], [("A", "A"), ("B", "B"), ("B", "A")]),
        (7, 10, "A-|A, B-|B, A-|B, B-|A", [], [("A", "A"), ("B", "B"), ("A", "B"), ("B", "A")]),
    ]
    graphs = []
    for global_id, (cls, graph_in_class, desc, pos, neg) in enumerate(raw, start=1):
        graphs.append(
            {
                "graph_global_id": global_id,
                "class": cls,
                "graph_in_class": graph_in_class,
                "graph_desc": desc,
                "edges": edge_dict(pos, neg),
            }
        )
    if len(graphs) != 39:
        raise RuntimeError(f"Expected 39 connected signed graphs, got {len(graphs)}")
    return graphs


def literal(variable: str, sign: int) -> str:
    if sign == 1:
        return variable
    if sign == -1:
        return f"!{variable}"
    raise ValueError("literal called for absent edge")


def build_rule_expression(edges: dict[str, int], node: str, op: str) -> str:
    """Build the final signed-literal AND/OR rule for A or B."""
    if node == "A":
        checks = [("AA", "A"), ("BA", "B")]
    elif node == "B":
        checks = [("BB", "B"), ("AB", "A")]
    else:
        raise ValueError("node must be A or B")
    terms = [literal(variable, edges[key]) for key, variable in checks if edges[key] != 0]
    if not terms:
        return "1" if op == "AND" else "0"
    return (" & " if op == "AND" else " | ").join(terms)


def generate_final_network_catalogue() -> list[dict[str, Any]]:
    """Generate the final 89 Boolean Networks from the graph/rule catalogue."""
    networks = []
    network_id = 0
    for graph in final_graph_catalogue():
        for logic_code in CLASS_RULE_SETS[graph["class"]]:
            network_id += 1
            op_a, op_b = LOGIC_TYPES[logic_code]
            expr_a = build_rule_expression(graph["edges"], "A", op_a)
            expr_b = build_rule_expression(graph["edges"], "B", op_b)
            networks.append(
                {
                    "network_id": network_id,
                    "class": graph["class"],
                    "graph_in_class": graph["graph_in_class"],
                    "graph_global_id": graph["graph_global_id"],
                    "graph_desc": graph["graph_desc"],
                    "logic_code": logic_code,
                    "gate_A": op_a,
                    "gate_B": op_b,
                    "rule_A": f"A' = {expr_a}",
                    "rule_B": f"B' = {expr_b}",
                    **{f"edge_{k}": v for k, v in graph["edges"].items()},
                }
            )
    if len(networks) != 89:
        raise RuntimeError(f"Expected 89 Boolean Networks, got {len(networks)}")
    return networks


def state_tuple(label: str) -> tuple[int, int]:
    return int(label[0]), int(label[1])


def eval_rule(rule: str, state: str) -> int:
    """Evaluate one final-app Boolean rule for state label 00/01/10/11."""
    a, b = state_tuple(state)
    expr = rule.split("=", 1)[1].strip()
    expr = expr.replace("!", " not ")
    expr = expr.replace("&", " and ")
    expr = expr.replace("|", " or ")
    env = {"A": bool(a), "B": bool(b)}
    return int(bool(eval(expr, {"__builtins__": {}}, env)))


def shannon_entropy(values: list[Any]) -> float:
    if not values:
        return 0.0
    counts = Counter(values)
    n = sum(counts.values())
    return float(-sum((c / n) * math.log2(c / n) for c in counts.values() if c))


def skewness(values: list[float]) -> float:
    arr = np.asarray(values, dtype=float)
    if arr.size == 0:
        return 0.0
    std = float(arr.std(ddof=0))
    if std == 0.0:
        return 0.0
    return float(np.mean(((arr - arr.mean()) / std) ** 3))


def hamming(a: str, b: str) -> int:
    return sum(x != y for x, y in zip(a, b))


def components_to_text(components: list[set[str]], bracket_cycles: bool = False) -> str:
    if not components:
        return "None"
    parts = []
    for comp in components:
        ordered = sorted(comp, key=STATES.index)
        if len(ordered) == 1:
            parts.append(ordered[0])
        else:
            left, right = ("[", "]") if bracket_cycles else ("{", "}")
            parts.append(left + ",".join(ordered) + right)
    return "; ".join(parts)


def terminal_sccs(graph: nx.DiGraph) -> list[set[str]]:
    terminals: list[set[str]] = []
    for comp in nx.strongly_connected_components(graph):
        comp_set = set(comp)
        if all(v in comp_set for u in comp_set for v in graph.successors(u)):
            terminals.append(comp_set)
    return sorted(terminals, key=lambda c: (len(c), [STATES.index(s) for s in sorted(c)]))


def basin_sizes(graph: nx.DiGraph, attractors: list[set[str]]) -> list[int]:
    reverse_graph = graph.reverse(copy=True)
    sizes: list[int] = []
    for attractor in attractors:
        basin = set(attractor)
        queue = list(attractor)
        while queue:
            current = queue.pop(0)
            for predecessor in reverse_graph.successors(current):
                if predecessor not in basin:
                    basin.add(predecessor)
                    queue.append(predecessor)
        sizes.append(len(basin))
    return sizes


def classify_sync(fixed_count: int, cycle_count: int, attractor_count: int) -> str:
    if fixed_count > 0 and cycle_count == 0 and attractor_count == 1:
        return "Stable Fixed-point"
    if fixed_count == 0 and cycle_count > 0:
        return "Pure Oscillatory"
    if fixed_count > 0 and cycle_count == 0 and attractor_count > 1:
        return "Complex Composite"
    if fixed_count > 0 and cycle_count > 0:
        return "Hybrid Multimodal"
    return "Other"


def classify_async(fixed_count: int, trap_count: int, attractor_count: int) -> str:
    if fixed_count == 1 and trap_count == 0 and attractor_count == 1:
        return "Single fixed point"
    if fixed_count > 1 and trap_count == 0:
        return "Multi-attractor fixed-point"
    if fixed_count == 0 and trap_count > 0:
        return "Cyclic / trap dominated"
    if fixed_count > 0 and trap_count > 0:
        return "Mixed fixed + trap"
    return "Other"


def load_metadata() -> dict[int, dict[str, Any]]:
    old_truth = next((path for path in OLD_TRUTH_CANDIDATES if path.exists()), None)
    if old_truth is None:
        return {}
    old = pd.read_csv(old_truth, dtype=str)
    meta: dict[int, dict[str, Any]] = {}
    for nid, group in old.groupby("network_id", sort=False):
        row = group.iloc[0].to_dict()
        meta[int(nid)] = {
            "graph_global_id": int(row.get("graph_global_id", nid)),
            "graph_desc": row.get("graph_desc", ""),
            "gate_A": row.get("gate_A", ""),
            "gate_B": row.get("gate_B", ""),
        }
    return meta


def build_truth_tables(app_rows: list[dict[str, Any]]) -> tuple[pd.DataFrame, pd.DataFrame]:
    old_meta = load_metadata()
    sync_rows: list[dict[str, Any]] = []
    async_rows: list[dict[str, Any]] = []
    for item in app_rows:
        nid = int(item["network_id"])
        meta = old_meta.get(nid, {})
        base = {
            "network_id": nid,
            "class": int(item["class"]),
            "graph_in_class": int(item["graph_in_class"]),
            "graph_global_id": int(meta.get("graph_global_id", item["network_id"])),
            "graph_desc": meta.get("graph_desc", f"C{item['class']}-G{item['graph_in_class']}"),
            "logic_code": item["logic_code"],
            "gate_A": meta.get("gate_A", item.get("gate_A", "")),
            "gate_B": meta.get("gate_B", item.get("gate_B", "")),
            "rule_A": item["rule_A"],
            "rule_B": item["rule_B"],
            "edge_AA": item.get("edge_AA", 0),
            "edge_AB": item.get("edge_AB", 0),
            "edge_BA": item.get("edge_BA", 0),
            "edge_BB": item.get("edge_BB", 0),
        }
        for state in STATES:
            a, b = state_tuple(state)
            a_next = eval_rule(item["rule_A"], state)
            b_next = eval_rule(item["rule_B"], state)
            sync_rows.append({**base, "start_state": state, "sync_next_state": f"{a_next}{b_next}"})
            async_rows.append(
                {
                    **base,
                    "start_state": state,
                    "A_update_only": f"{a_next}{b}",
                    "B_update_only": f"{a}{b_next}",
                }
            )
    return pd.DataFrame(sync_rows), pd.DataFrame(async_rows)


def build_sync_transition_rows(sync_truth: pd.DataFrame) -> pd.DataFrame:
    rows: list[dict[str, Any]] = []
    for _, r in sync_truth.iterrows():
        rows.append(
            {
                "network_id": r["network_id"],
                "class": r["class"],
                "graph_in_class": r["graph_in_class"],
                "graph_global_id": r["graph_global_id"],
                "graph_desc": r["graph_desc"],
                "logic_code": r["logic_code"],
                "rule_A": r["rule_A"],
                "rule_B": r["rule_B"],
                "from_state": r["start_state"],
                "to_state": r["sync_next_state"],
                "update_scheme": "sync",
                "selected_node": "A+B",
                "is_self_loop": int(r["start_state"] == r["sync_next_state"]),
            }
        )
    return pd.DataFrame(rows)


def build_async_transition_rows(async_truth: pd.DataFrame) -> pd.DataFrame:
    """Full final asynchronous STG edges after self-loop removal convention.

    The asynchronous STG is the set of all possible one-node updates from each
    current state. Self-loops are removed when at least one non-self outgoing
    transition exists from that same state, and retained only for fixed states
    with no non-self asynchronous successor.
    """
    rows: list[dict[str, Any]] = []
    for _, r in async_truth.iterrows():
        start = r["start_state"]
        candidates = [("A", r["A_update_only"]), ("B", r["B_update_only"])]
        non_self = [(node, target) for node, target in candidates if target != start]
        kept = non_self if non_self else candidates[:1]
        for node, target in kept:
            rows.append(
                {
                    "network_id": r["network_id"],
                    "class": r["class"],
                    "graph_in_class": r["graph_in_class"],
                    "graph_global_id": r["graph_global_id"],
                    "graph_desc": r["graph_desc"],
                    "logic_code": r["logic_code"],
                    "rule_A": r["rule_A"],
                    "rule_B": r["rule_B"],
                    "from_state": start,
                    "to_state": target,
                    "update_scheme": "async",
                    "selected_node": node,
                    "is_self_loop": int(start == target),
                }
            )
    return pd.DataFrame(rows)


def find_sync_attractors(mapping: dict[str, str]) -> tuple[list[set[str]], dict[str, int], dict[str, int]]:
    cycles: list[list[str]] = []
    cycle_keys: dict[tuple[str, ...], int] = {}
    basin_id: dict[str, int] = {}
    transient: dict[str, int] = {}
    for start in STATES:
        path: list[str] = []
        seen: dict[str, int] = {}
        cur = start
        while cur not in seen:
            seen[cur] = len(path)
            path.append(cur)
            cur = mapping[cur]
        cycle = path[seen[cur] :]
        rotations = [tuple(cycle[i:] + cycle[:i]) for i in range(len(cycle))]
        key = min(rotations)
        if key not in cycle_keys:
            cycle_keys[key] = len(cycles)
            cycles.append(list(key))
        cid = cycle_keys[key]
        for idx, state in enumerate(path):
            basin_id[state] = cid
            transient[state] = max(0, seen[cur] - idx)
    return [set(c) for c in cycles], basin_id, transient


def sync_features(sync_truth: pd.DataFrame) -> pd.DataFrame:
    rows: list[dict[str, Any]] = []
    for nid, group in sync_truth.groupby("network_id", sort=True):
        mapping = {r["start_state"]: r["sync_next_state"] for _, r in group.iterrows()}
        first = group.iloc[0].to_dict()
        cycles, basin_id, transient = find_sync_attractors(mapping)
        fixed = [c for c in cycles if len(c) == 1]
        limit = [c for c in cycles if len(c) > 1]
        basin_values = list(Counter(basin_id.values()).values())
        attractor_lengths = [len(c) for c in cycles]
        image_states = list(mapping.values())
        preimage_counts = [image_states.count(s) for s in STATES]
        hamming_steps = [hamming(s, mapping[s]) for s in STATES]
        local_sensitivity = []
        for state in STATES:
            for bit in (0, 1):
                chars = list(state)
                chars[bit] = "1" if chars[bit] == "0" else "0"
                local_sensitivity.append(hamming(mapping[state], mapping["".join(chars)]))
        derrida = np.mean([hamming(mapping[a], mapping[b]) for a in STATES for b in STATES])
        attractor_states = set().union(*cycles) if cycles else set()
        common = int(len({s[0] for s in attractor_states}) == 1) + int(len({s[1] for s in attractor_states}) == 1)
        row = {
            **{k: first[k] for k in [
                "network_id", "class", "graph_in_class", "graph_global_id", "graph_desc",
                "logic_code", "gate_A", "gate_B", "rule_A", "rule_B"
            ]},
            "Number of Attractors": len(cycles),
            "Number of Fixed Points": len(fixed),
            "Number of Limit Cycles": len(limit),
            "Avg Attractor Length": float(np.mean(attractor_lengths)),
            "Max Attractor Length": int(max(attractor_lengths)),
            "Attractor Coverage": 1.0,
            "Avg Basin Size": float(np.mean(basin_values)),
            "Max Basin Size": int(max(basin_values)),
            "Basin Size Variance": float(np.var(basin_values, ddof=0)),
            "Basin Size Skewness": skewness(basin_values),
            "Average Transient Length": float(np.mean(list(transient.values()))),
            "Maximum Transient Length": int(max(transient.values())),
            "Transient Length Variance": float(np.var(list(transient.values()), ddof=0)),
            "Transient Length Skewness": skewness(list(transient.values())),
            "Garden of Eden Count": int(sum(c == 0 for c in preimage_counts)),
            "Common Sea Count": common,
            "Specific Part Count": 2 - common,
            "Avg Pre-image Count": float(np.mean(preimage_counts)),
            "Max Pre-image Count": int(max(preimage_counts)),
            "Avg Hamming Distance per Transition": float(np.mean(hamming_steps)),
            "Average Sensitivity": float(np.mean(local_sensitivity)),
            "Derrida Coefficient": float(derrida),
            "Attractor Components": components_to_text(cycles, bracket_cycles=True),
        }
        row["Attractor_Type"] = classify_sync(row["Number of Fixed Points"], row["Number of Limit Cycles"], row["Number of Attractors"])
        rows.append(row)
    out = pd.DataFrame(rows)
    for col in ["network_id", "class", "graph_in_class", "graph_global_id"]:
        out[col] = out[col].astype(int)
    return out


def build_async_graph(group: pd.DataFrame) -> tuple[nx.DiGraph, list[dict[str, Any]], dict[str, dict[str, str]]]:
    graph = nx.DiGraph()
    graph.add_nodes_from(STATES)
    rows: list[dict[str, Any]] = []
    maps = {"A": {}, "B": {}}
    for _, r in group.iterrows():
        start = r["start_state"]
        maps["A"][start] = r["A_update_only"]
        maps["B"][start] = r["B_update_only"]
        candidates = [("A", r["A_update_only"]), ("B", r["B_update_only"])]
        non_self = [(node, target) for node, target in candidates if target != start]
        kept = non_self if non_self else candidates[:1]
        for node, target in kept:
            graph.add_edge(start, target)
            rows.append({"from_state": start, "to_state": target, "selected_node": node, "is_self_loop": int(start == target)})
    return graph, rows, maps


def async_operator_sensitivity(maps: dict[str, dict[str, str]]) -> tuple[float, float]:
    distances = []
    for state in STATES:
        for bit in (0, 1):
            chars = list(state)
            chars[bit] = "1" if chars[bit] == "0" else "0"
            perturbed = "".join(chars)
            for node in ("A", "B"):
                distances.append(hamming(maps[node][state], maps[node][perturbed]))
    avg = float(np.mean(distances)) if distances else 0.0
    return avg, avg


def async_features(async_truth: pd.DataFrame) -> pd.DataFrame:
    rows: list[dict[str, Any]] = []
    for nid, group in async_truth.groupby("network_id", sort=True):
        first = group.iloc[0].to_dict()
        graph, transition_rows, maps = build_async_graph(group)
        in_deg = [graph.in_degree(s) for s in STATES]
        out_deg = [graph.out_degree(s) for s in STATES]
        sccs = [set(c) for c in nx.strongly_connected_components(graph)]
        attractors = terminal_sccs(graph)
        fixed = [c for c in attractors if len(c) == 1]
        traps = [c for c in attractors if len(c) > 1]
        lengths = [len(c) for c in attractors]
        basin_values = basin_sizes(graph, attractors)
        attractor_states = set().union(*attractors) if attractors else set()
        shortest = []
        for source in STATES:
            for target in STATES:
                if source != target and nx.has_path(graph, source, target):
                    shortest.append(nx.shortest_path_length(graph, source, target))
        transient_lengths = []
        for start in STATES:
            if start in attractor_states:
                transient_lengths.append(0)
            else:
                distances = [nx.shortest_path_length(graph, start, target) for target in attractor_states if nx.has_path(graph, start, target)]
                transient_lengths.append(min(distances) if distances else 0)
        common = int(len({s[0] for s in attractor_states}) == 1) + int(len({s[1] for s in attractor_states}) == 1) if attractor_states else 0
        derrida, sens = async_operator_sensitivity(maps)
        adjacency = nx.to_numpy_array(graph, nodelist=STATES, dtype=float)
        eig = np.linalg.eigvals(adjacency)
        undirected = nx.to_numpy_array(graph.to_undirected(), nodelist=STATES, dtype=float)
        lap = np.diag(undirected.sum(axis=1)) - undirected
        lap_eig = np.linalg.eigvalsh(lap)
        row = {
            **{k: first[k] for k in [
                "network_id", "class", "graph_in_class", "graph_global_id", "graph_desc",
                "logic_code", "gate_A", "gate_B", "rule_A", "rule_B"
            ]},
            "Number of Nodes": graph.number_of_nodes(),
            "Number of Edges": graph.number_of_edges(),
            "Density": nx.density(graph),
            "Strongly Connected": int(nx.is_strongly_connected(graph)),
            "Number of SCCs": len(sccs),
            "Largest SCC Size": max(len(c) for c in sccs),
            "Avg In-Degree": float(np.mean(in_deg)),
            "Avg Out-Degree": float(np.mean(out_deg)),
            "Max In-Degree": int(max(in_deg)),
            "Max Out-Degree": int(max(out_deg)),
            "Has Cycle": int(bool(traps)),
            "Number of Cycles": len(traps),
            "Number of Limit Cycles": len(traps),
            "Max Cycle Length": int(max((len(c) for c in traps), default=0)),
            "Avg Cycle Length": float(np.mean([len(c) for c in traps])) if traps else 0.0,
            "Shannon Entropy (Out-degree)": shannon_entropy(out_deg),
            "Shannon Entropy (In-degree)": shannon_entropy(in_deg),
            "Transition Entropy": shannon_entropy([r["to_state"] for r in transition_rows]),
            "Basin Entropy": shannon_entropy(basin_values),
            "Spectral Radius": float(np.max(np.abs(eig))) if eig.size else 0.0,
            "Algebraic Connectivity": float(sorted(lap_eig)[1]) if len(lap_eig) > 1 else 0.0,
            "Number of Attractors": len(attractors),
            "Number of Fixed Points": len(fixed),
            "Has Fixed Point Attractor": int(bool(fixed)),
            "Avg Attractor Length": float(np.mean(lengths)) if lengths else 0.0,
            "Max Attractor Length": int(max(lengths)) if lengths else 0,
            "Attractor Coverage": float(len(attractor_states) / len(STATES)),
            "Avg Basin Size": float(np.mean(basin_values)) if basin_values else 0.0,
            "Max Basin Size": int(max(basin_values)) if basin_values else 0,
            "Basin Size Variance": float(np.var(basin_values, ddof=0)) if basin_values else 0.0,
            "Basin Size Skewness": skewness(basin_values),
            "Average Transient Length": float(np.mean(transient_lengths)),
            "Maximum Transient Length": int(max(transient_lengths)),
            "Diameter": int(max(shortest)) if shortest else 0,
            "Average Shortest Path Length": float(np.mean(shortest)) if shortest else 0.0,
            "Garden of Eden Count": int(sum(d == 0 for d in in_deg)),
            "Common Sea Count": common,
            "Specific Part Count": 2 - common,
            "Backward Boolean Equivalence": int(all(maps["A"][s] == maps["B"][s] for s in STATES)),
            "Derrida Coefficient": derrida,
            "Average Sensitivity": sens,
            "Fixed Point Attractors": components_to_text(fixed),
            "Cycle or Trap Components": components_to_text(traps),
            "Attractor Components": components_to_text(attractors),
            "Basin Sizes": "; ".join(str(v) for v in basin_values) if basin_values else "None",
        }
        row["Attractor_Type"] = classify_async(row["Number of Fixed Points"], row["Number of Limit Cycles"], row["Number of Attractors"])
        rows.append(row)
    out = pd.DataFrame(rows)
    for col in ["network_id", "class", "graph_in_class", "graph_global_id"]:
        out[col] = out[col].astype(int)
    return out


def prepare_scaled(df: pd.DataFrame) -> tuple[pd.DataFrame, np.ndarray, list[str]]:
    numeric = df.select_dtypes(include=np.number).copy()
    numeric = numeric.drop(columns=[c for c in METADATA_NUMERIC if c in numeric.columns], errors="ignore")
    numeric = numeric.replace([np.inf, -np.inf], np.nan)
    numeric = numeric.loc[:, numeric.nunique(dropna=True) > 1]
    numeric = numeric.drop(columns=numeric.columns[numeric.isna().all()])
    imputed = pd.DataFrame(SimpleImputer(strategy="median").fit_transform(numeric), columns=numeric.columns)
    med = imputed.median()
    iqr = (imputed.quantile(0.75) - imputed.quantile(0.25)).replace(0, 1.0).fillna(1.0)
    scaled = ((imputed - med) / iqr).clip(-1e6, 1e6)
    arr = scaled.to_numpy(dtype=float)
    arr[~np.isfinite(arr)] = 0.0
    return scaled, arr, list(scaled.columns)


def axis_label(model: Any, z: np.ndarray, arr: np.ndarray, feature_names: list[str], dim: int) -> str:
    if model is not None and hasattr(model, "components_"):
        loadings = model.components_[dim - 1]
        order = np.argsort(-np.abs(loadings))[:3]
        label = "↑ high | " + " | ".join(("↑ " if loadings[i] > 0 else "↓ ") + feature_names[i] for i in order)
        return "\n".join(textwrap.wrap(label, width=72, break_long_words=False))
    coord = z[:, dim - 1]
    corrs: list[tuple[float, bool, str]] = []
    for i, name in enumerate(feature_names):
        feature = arr[:, i]
        if np.std(feature) > 0:
            r = np.corrcoef(coord, feature)[0, 1]
            if not np.isnan(r):
                corrs.append((abs(r), r > 0, name))
    corrs.sort(reverse=True)
    label = "↑ high | " + " | ".join(("↑ " if pos else "↓ ") + name for _, pos, name in corrs[:3])
    return "\n".join(textwrap.wrap(label, width=72, break_long_words=False))


def add_cluster_points(ax: Any, z: np.ndarray, labels: np.ndarray) -> None:
    x_span = float(np.ptp(z[:, 0])) or 1.0
    y_span = float(np.ptp(z[:, 1])) or 1.0
    for i, color in enumerate(GOOGLE_COLORS):
        mask = labels == i
        ax.scatter(z[mask, 0], z[mask, 1], s=100, color=color, alpha=0.94, edgecolor="white", lw=0.5, label=f"Cluster {i + 1}")
        center = z[mask].mean(axis=0)
        if len(z[mask]) >= 3:
            cov = np.cov(z[mask].T)
            vals, vecs = np.linalg.eigh(cov)
            order = vals.argsort()[::-1]
            vals, vecs = vals[order], vecs[:, order]
            angle = np.degrees(np.arctan2(*vecs[:, 0][::-1]))
            width, height = 2 * np.sqrt(np.maximum(vals, 0)) * 1.6
            ax.add_patch(Ellipse(center, width, height, angle=angle, facecolor=color, alpha=0.16, edgecolor=color, lw=1.3))
        ax.text(
            center[0] + 0.02 * x_span,
            center[1] + 0.02 * y_span,
            f"Cluster {i + 1}",
            ha="center",
            va="center",
            fontsize=12,
            color="black",
            bbox=dict(facecolor="white", edgecolor="none", alpha=0.74, pad=1.8),
            zorder=20,
        )


def cluster_and_plot(df: pd.DataFrame, prefix: str, figure_path: Path) -> tuple[pd.DataFrame, pd.DataFrame, np.ndarray, list[str]]:
    scaled, arr, feature_names = prepare_scaled(df)
    labels = KMeans(n_clusters=4, random_state=42, n_init=20).fit_predict(arr)
    pca = PCA(n_components=2, random_state=42)
    z_pca = pca.fit_transform(arr)
    z_tsne = TSNE(n_components=2, perplexity=min(30, len(arr) // 3), init="pca", random_state=42, learning_rate="auto").fit_transform(arr)
    z_umap = umap.UMAP(n_components=2, n_neighbors=15, min_dist=0.1, random_state=42).fit_transform(arr)

    plt.rcParams["figure.dpi"] = 140
    plt.style.use("seaborn-v0_8-pastel")
    fig, axes = plt.subplots(1, 3, figsize=(20, 6.5))
    for ax, z, title, model in zip(axes, [z_pca, z_tsne, z_umap], ["PCA", "t-SNE", "UMAP"], [pca, None, None]):
        add_cluster_points(ax, z, labels)
        ax.set_title(title, fontsize=15, pad=20)
        ax.set_xlabel(axis_label(model, z, arr, feature_names, 1), fontsize=9)
        ax.set_ylabel(axis_label(model, z, arr, feature_names, 2), fontsize=9)
        ax.axhline(0, color="gray", lw=0.9, alpha=0.5, linestyle="--")
        ax.axvline(0, color="gray", lw=0.9, alpha=0.5, linestyle="--")
        ax.grid(False)
    handles, legend_labels = axes[0].get_legend_handles_labels()
    fig.legend(handles, legend_labels, title="4 clusters", loc="center right", bbox_to_anchor=(1.02, 0.5), fontsize=11, title_fontsize=12, frameon=True, edgecolor="lightgray")
    fig.subplots_adjust(left=0.055, right=0.90, bottom=0.23, top=0.88, wspace=0.32)
    fig.savefig(figure_path, dpi=300, bbox_inches="tight")
    plt.close(fig)

    assignments = df[
        [
            "network_id", "class", "graph_in_class", "graph_global_id", "graph_desc",
            "logic_code", "rule_A", "rule_B", "Attractor_Type",
        ]
    ].copy()
    assignments["Cluster"] = [f"Cluster_{i + 1}" for i in labels]
    assignments["cluster_label_numeric"] = labels
    coords = pd.DataFrame(
        {
            "network_id": df["network_id"],
            "Cluster": assignments["Cluster"],
            "PCA_1": z_pca[:, 0],
            "PCA_2": z_pca[:, 1],
            "TSNE_1": z_tsne[:, 0],
            "TSNE_2": z_tsne[:, 1],
            "UMAP_1": z_umap[:, 0],
            "UMAP_2": z_umap[:, 1],
        }
    )
    scaled.insert(0, "network_id", df["network_id"].to_numpy())
    scaled.to_csv(OUT / f"borna_final_{prefix}_scaled_features.csv", index=False, encoding="utf-8-sig")
    coords.to_csv(OUT / f"borna_final_{prefix}_embedding_coordinates.csv", index=False, encoding="utf-8-sig")
    return assignments, scaled, arr, feature_names


def make_count_table(assignments: pd.DataFrame, columns: list[str]) -> pd.DataFrame:
    table = pd.crosstab(assignments["Cluster"], assignments["Attractor_Type"])
    table = table.reindex(index=[f"Cluster_{i}" for i in range(1, 5)], columns=columns, fill_value=0)
    table["Total"] = table.sum(axis=1)
    total = pd.DataFrame([["Total", *[int(table[c].sum()) for c in columns], int(table["Total"].sum())]], columns=["Cluster", *columns, "Total"])
    table = table.reset_index()
    return pd.concat([table, total], ignore_index=True)


def latex_tables(sync_table: pd.DataFrame, async_table: pd.DataFrame) -> str:
    def rows(df: pd.DataFrame) -> str:
        lines = []
        for _, row in df.iterrows():
            name = row["Cluster"]
            display = r"\textbf{Total}" if name == "Total" else rf"\(Cluster_{name.split('_')[-1]}\)"
            nums = " & ".join(str(int(row[c])) for c in df.columns[1:])
            lines.append(f"{display} & {nums} \\\\")
        return "\n".join(lines)

    return rf"""\begin{{table}}[h!]
\centering
\caption{{Distribution of synchronous attractor types across the four clusters.}}
\label{{tab:sync_attractor_cluster_mapping}}
\begin{{tabular}}{{lccccc}}
\toprule
\textbf{{Clusters}} &
\makecell{{\textbf{{Stable}}\\\textbf{{fixed-point}}}} &
\makecell{{\textbf{{Pure}}\\\textbf{{oscillatory}}}} &
\makecell{{\textbf{{Complex}}\\\textbf{{composite}}}} &
\makecell{{\textbf{{Hybrid}}\\\textbf{{multimodal}}}} &
\textbf{{Total}} \\
\midrule
{rows(sync_table.iloc[:4])}
\midrule
{rows(sync_table.iloc[4:])}
\bottomrule
\end{{tabular}}
\end{{table}}

\begin{{table}}[h!]
\centering
\caption{{Distribution of asynchronous attractor types across the four clusters.}}
\label{{tab:async_attractor_cluster_mapping}}
\begin{{tabular}}{{lccccc}}
\toprule
\textbf{{Clusters}} &
\makecell{{\textbf{{Single}}\\\textbf{{fixed point}}}} &
\makecell{{\textbf{{Multi-attractor}}\\\textbf{{fixed-point}}}} &
\makecell{{\textbf{{Cyclic / trap}}\\\textbf{{dominated}}}} &
\makecell{{\textbf{{Mixed}}\\\textbf{{fixed + trap}}}} &
\textbf{{Total}} \\
\midrule
{rows(async_table.iloc[:4])}
\midrule
{rows(async_table.iloc[4:])}
\bottomrule
\end{{tabular}}
\end{{table}}
"""


def draw_arrow(ax: Any, start: tuple[float, float], end: tuple[float, float], rad: float = 0.0,
               color: str = "#333333", linestyle: str = "-", label: str | None = None) -> None:
    arrow = FancyArrowPatch(
        start,
        end,
        connectionstyle=f"arc3,rad={rad}",
        arrowstyle="-|>",
        mutation_scale=13,
        lw=1.35,
        color=color,
        linestyle=linestyle,
        shrinkA=20,
        shrinkB=20,
        zorder=2,
    )
    ax.add_patch(arrow)
    if label:
        mx = (start[0] + end[0]) / 2
        my = (start[1] + end[1]) / 2 + rad * 0.45
        ax.text(mx, my, label, ha="center", va="center", fontsize=8, bbox=dict(fc="white", ec="none", alpha=0.8), zorder=5)


def draw_self_loop(ax: Any, center: tuple[float, float], label: str | None = None,
                   color: str = "#333333", linestyle: str = "-") -> None:
    x, y = center
    start = (x + 0.16, y + 0.22)
    end = (x - 0.16, y + 0.22)
    arrow = FancyArrowPatch(
        start,
        end,
        connectionstyle="arc3,rad=1.65",
        arrowstyle="-|>",
        mutation_scale=13,
        lw=1.25,
        color=color,
        linestyle=linestyle,
        shrinkA=3,
        shrinkB=3,
        zorder=2,
    )
    ax.add_patch(arrow)
    if label:
        ax.text(x, y + 0.45, label, ha="center", va="center", fontsize=8, bbox=dict(fc="white", ec="none", alpha=0.8), zorder=5)


def draw_state_nodes(ax: Any, attractor_states: set[str]) -> dict[str, tuple[float, float]]:
    pos = {"00": (-1.0, 0.75), "01": (1.0, 0.75), "10": (-1.0, -0.75), "11": (1.0, -0.75)}
    for state, (x, y) in pos.items():
        face = "#dbeafe" if state in attractor_states else "white"
        ax.add_patch(plt.Rectangle((x - 0.22, y - 0.16), 0.44, 0.32, facecolor=face, edgecolor="#1f2a44", lw=1.1, zorder=3))
        ax.text(x, y, state, ha="center", va="center", fontsize=10, color="#111827", zorder=4)
    ax.set_xlim(-1.55, 1.55)
    ax.set_ylim(-1.2, 1.25)
    ax.set_aspect("equal")
    ax.axis("off")
    return pos


def draw_stg(ax: Any, transitions: list[dict[str, Any]], attractor_states: set[str], title: str) -> None:
    pos = draw_state_nodes(ax, attractor_states)
    grouped: dict[tuple[str, str], list[str]] = {}
    for row in transitions:
        grouped.setdefault((row["from_state"], row["to_state"]), []).append(str(row["selected_node"]))
    for (source, target), labels in grouped.items():
        label = "/".join(sorted(set(labels)))
        if source == target:
            draw_self_loop(ax, pos[source], label=label)
            continue
        pair = tuple(sorted((source, target)))
        rad = 0.18 if source < target else -0.18
        if (target, source) not in grouped:
            rad = 0.08
        draw_arrow(ax, pos[source], pos[target], rad=rad, label=label)
    ax.set_title(title, fontsize=11, pad=8)


def draw_regulatory_graph(ax: Any, row: pd.Series) -> None:
    pos = {"A": (-0.8, 0.0), "B": (0.8, 0.0)}
    for node, (x, y) in pos.items():
        circ = plt.Circle((x, y), 0.25, facecolor="white", edgecolor="#1f2a44", lw=1.8, zorder=4)
        ax.add_patch(circ)
        ax.text(x, y, node, ha="center", va="center", fontsize=15, weight="bold", zorder=5)
    edges = [
        ("A", "A", int(row["edge_AA"])),
        ("A", "B", int(row["edge_AB"])),
        ("B", "A", int(row["edge_BA"])),
        ("B", "B", int(row["edge_BB"])),
    ]
    for source, target, sign in edges:
        if sign == 0:
            continue
        color = "#16a34a" if sign == 1 else "#dc2626"
        linestyle = "-" if sign == 1 else "--"
        label = "+" if sign == 1 else "-"
        if source == target:
            draw_self_loop(ax, pos[source], label=label, color=color, linestyle=linestyle)
        else:
            rad = 0.25 if source == "A" else -0.25
            draw_arrow(ax, pos[source], pos[target], rad=rad, color=color, linestyle=linestyle, label=label)
    ax.set_xlim(-1.35, 1.35)
    ax.set_ylim(-0.95, 0.95)
    ax.set_aspect("equal")
    ax.axis("off")
    ax.set_title("Signed graph", fontsize=11, pad=8)


def sync_attractor_states_for_network(sync_truth: pd.DataFrame) -> set[str]:
    mapping = {r["start_state"]: r["sync_next_state"] for _, r in sync_truth.iterrows()}
    components, _, _ = find_sync_attractors(mapping)
    return set().union(*components) if components else set()


def async_attractor_states_for_network(async_truth: pd.DataFrame) -> set[str]:
    graph, _, _ = build_async_graph(async_truth)
    components = terminal_sccs(graph)
    return set().union(*components) if components else set()


def save_stg_catalogue_pdf(catalogue: pd.DataFrame, sync_truth: pd.DataFrame, async_truth: pd.DataFrame,
                           sync_features_df: pd.DataFrame, async_features_df: pd.DataFrame) -> None:
    pdf_path = OUT / "borna_final_sync_async_stg_catalogue_89.pdf"
    sync_transition_df = build_sync_transition_rows(sync_truth)
    async_transition_df = build_async_transition_rows(async_truth)
    sync_features_by_id = sync_features_df.set_index("network_id")
    async_features_by_id = async_features_df.set_index("network_id")
    with PdfPages(pdf_path) as pdf:
        for _, net in catalogue.iterrows():
            nid = int(net["network_id"])
            st = sync_truth[sync_truth["network_id"] == nid]
            at = async_truth[async_truth["network_id"] == nid]
            sync_transitions = sync_transition_df[sync_transition_df["network_id"] == nid].to_dict("records")
            async_transitions = async_transition_df[async_transition_df["network_id"] == nid].to_dict("records")
            fig = plt.figure(figsize=(11.69, 8.27))
            fig.suptitle(
                f"N{nid:02d}  C{int(net['class'])}-G{int(net['graph_in_class'])}  {net['logic_code']}",
                fontsize=15,
                weight="bold",
                y=0.97,
            )
            ax_graph = fig.add_axes([0.04, 0.47, 0.22, 0.38])
            ax_sync = fig.add_axes([0.30, 0.43, 0.28, 0.45])
            ax_async = fig.add_axes([0.63, 0.43, 0.28, 0.45])
            ax_text = fig.add_axes([0.05, 0.07, 0.88, 0.27])
            draw_regulatory_graph(ax_graph, net)
            draw_stg(ax_sync, sync_transitions, sync_attractor_states_for_network(st), "Synchronous STG")
            draw_stg(ax_async, async_transitions, async_attractor_states_for_network(at), "Asynchronous STG")
            sf = sync_features_by_id.loc[nid]
            af = async_features_by_id.loc[nid]
            text = (
                f"Graph: {net['graph_desc']}\n"
                f"Rules: {net['rule_A']}    {net['rule_B']}\n\n"
                f"Sync attractors: {sf['Attractor Components']} | type: {sf['Attractor_Type']}\n"
                f"Async attractors: {af['Attractor Components']} | type: {af['Attractor_Type']}\n\n"
                "Async convention: the STG contains all possible one-node updates from the current state. "
                "Self-loops are retained only when no non-self outgoing transition exists from that state."
            )
            ax_text.text(0, 1, text, ha="left", va="top", fontsize=10, linespacing=1.35)
            ax_text.axis("off")
            pdf.savefig(fig, bbox_inches="tight")
            plt.close(fig)


def validate_final_expected_outputs(sync_df: pd.DataFrame, async_df: pd.DataFrame) -> None:
    """Validate against the final manuscript/app-verified aggregate results."""
    if len(sync_df) != 89 or len(async_df) != 89:
        raise RuntimeError("Expected 89 synchronous and 89 asynchronous feature rows.")
    sync_counts = Counter(sync_df["Attractor_Type"])
    async_counts = Counter(async_df["Attractor_Type"])
    expected_sync = {
        "Stable Fixed-point": 22,
        "Pure Oscillatory": 28,
        "Complex Composite": 23,
        "Hybrid Multimodal": 16,
    }
    expected_async = {
        "Single fixed point": 29,
        "Multi-attractor fixed-point": 25,
        "Cyclic / trap dominated": 28,
        "Mixed fixed + trap": 7,
    }
    if dict(sync_counts) != expected_sync:
        raise RuntimeError(f"Unexpected synchronous attractor type counts: {dict(sync_counts)}")
    if dict(async_counts) != expected_async:
        raise RuntimeError(f"Unexpected asynchronous attractor type counts: {dict(async_counts)}")
    async_n_attr_counts = Counter(async_df["Number of Attractors"])
    if dict(async_n_attr_counts) != {1: 57, 2: 25, 3: 7}:
        raise RuntimeError(f"Unexpected asynchronous number-of-attractors counts: {dict(async_n_attr_counts)}")


def main() -> None:
    app_rows = generate_final_network_catalogue()
    catalogue_df = pd.DataFrame(app_rows)
    catalogue_df.to_csv(OUT / "borna_final_89_network_catalogue.csv", index=False, encoding="utf-8-sig")

    sync_truth, async_truth = build_truth_tables(app_rows)
    sync_truth.to_csv(OUT / "borna_final_sync_truth_table_89.csv", index=False, encoding="utf-8-sig")
    async_truth.to_csv(OUT / "borna_final_async_truth_table_89.csv", index=False, encoding="utf-8-sig")
    build_sync_transition_rows(sync_truth).to_csv(OUT / "borna_final_sync_stg_transitions_89.csv", index=False, encoding="utf-8-sig")
    build_async_transition_rows(async_truth).to_csv(OUT / "borna_final_async_stg_transitions_89.csv", index=False, encoding="utf-8-sig")

    sync_df = sync_features(sync_truth)
    async_df = async_features(async_truth)
    validate_final_expected_outputs(sync_df, async_df)

    sync_df.to_csv(OUT / "borna_final_sync_features_89.csv", index=False, encoding="utf-8-sig")
    async_df.to_csv(OUT / "borna_final_async_features_89.csv", index=False, encoding="utf-8-sig")

    sync_assign, _, _, sync_features_used = cluster_and_plot(sync_df, "sync", OUT / "borna_final_figure4_sync_pca_tsne_umap.png")
    async_assign, _, _, async_features_used = cluster_and_plot(async_df, "async", OUT / "borna_final_figure5_async_pca_tsne_umap.png")
    sync_assign.to_csv(OUT / "borna_final_sync_cluster_assignments.csv", index=False, encoding="utf-8-sig")
    async_assign.to_csv(OUT / "borna_final_async_cluster_assignments.csv", index=False, encoding="utf-8-sig")

    sync_cols = ["Stable Fixed-point", "Pure Oscillatory", "Complex Composite", "Hybrid Multimodal"]
    async_cols = ["Single fixed point", "Multi-attractor fixed-point", "Cyclic / trap dominated", "Mixed fixed + trap"]
    sync_table = make_count_table(sync_assign, sync_cols)
    async_table = make_count_table(async_assign, async_cols)
    sync_table.to_csv(OUT / "borna_final_table2_sync_cluster_attractor_types.csv", index=False, encoding="utf-8-sig")
    async_table.to_csv(OUT / "borna_final_table3_async_cluster_attractor_types.csv", index=False, encoding="utf-8-sig")
    (OUT / "borna_final_tables_2_3_latex.tex").write_text(latex_tables(sync_table, async_table), encoding="utf-8")
    save_stg_catalogue_pdf(catalogue_df, sync_truth, async_truth, sync_df, async_df)

    summary = [
        "BORNA final data regeneration from the code-defined graph/rule catalogue",
        "======================================================================",
        f"Networks: {len(sync_df)}",
        f"Sync retained nonconstant numeric STG features: {len(sync_features_used)}",
        f"Async retained nonconstant numeric STG features: {len(async_features_used)}",
        "",
        "Figure 4: borna_final_figure4_sync_pca_tsne_umap.png",
        "Figure 5: borna_final_figure5_async_pca_tsne_umap.png",
        "",
        "Catalogue PDF: borna_final_sync_async_stg_catalogue_89.pdf",
        "",
        "Table 2:",
        sync_table.to_string(index=False),
        "",
        "Table 3:",
        async_table.to_string(index=False),
        "",
        "Sync cluster sizes:",
        sync_assign["Cluster"].value_counts().sort_index().to_string(),
        "",
        "Async cluster sizes:",
        async_assign["Cluster"].value_counts().sort_index().to_string(),
    ]
    text = "\n".join(summary)
    (OUT / "borna_final_generation_summary.txt").write_text(text, encoding="utf-8")
    print(text)


if __name__ == "__main__":
    main()
