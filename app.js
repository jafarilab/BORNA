const STATES = ["00", "01", "10", "11"];
const STATE_BITS = {
  "00": [0, 0],
  "01": [0, 1],
  "10": [1, 0],
  "11": [1, 1],
};

const FUNCTION_OPTIONS = [
  { expr: "0", table: [0, 0, 0, 0] },
  { expr: "1", table: [1, 1, 1, 1] },
  { expr: "A", table: [0, 0, 1, 1] },
  { expr: "!A", table: [1, 1, 0, 0] },
  { expr: "B", table: [0, 1, 0, 1] },
  { expr: "!B", table: [1, 0, 1, 0] },
  { expr: "A & B", table: [0, 0, 0, 1] },
  { expr: "A | B", table: [0, 1, 1, 1] },
  { expr: "A & !B", table: [0, 0, 1, 0] },
  { expr: "A | !B", table: [1, 0, 1, 1] },
  { expr: "!A & B", table: [0, 1, 0, 0] },
  { expr: "!A | B", table: [1, 1, 0, 1] },
  { expr: "!A & !B", table: [1, 0, 0, 0] },
  { expr: "!A | !B", table: [1, 1, 1, 0] },
  { expr: "A ^ B", table: [0, 1, 1, 0] },
  { expr: "!(A ^ B)", table: [1, 0, 0, 1] },
];

const EXPR_BY_TABLE = new Map(FUNCTION_OPTIONS.map((f) => [f.table.join(""), f.expr]));

const GRAPHS_RAW = [
  ["A->B", [["A", "B"]], []],
  ["A-|B", [], [["A", "B"]]],
  ["A->B, B->A", [["A", "B"], ["B", "A"]], []],
  ["A->B, B-|A", [["A", "B"]], [["B", "A"]]],
  ["A-|B, B-|A", [], [["A", "B"], ["B", "A"]]],
  ["A->A, A->B", [["A", "A"], ["A", "B"]], []],
  ["A->A, A-|B", [["A", "A"]], [["A", "B"]]],
  ["A-|A, A->B", [["A", "B"]], [["A", "A"]]],
  ["A-|A, A-|B", [], [["A", "A"], ["A", "B"]]],
  ["A->A, B->A", [["A", "A"], ["B", "A"]], []],
  ["A->A, B-|A", [["A", "A"]], [["B", "A"]]],
  ["A-|A, B->A", [["B", "A"]], [["A", "A"]]],
  ["A-|A, B-|A", [], [["A", "A"], ["B", "A"]]],
  ["A->A, A->B, B->A", [["A", "A"], ["A", "B"], ["B", "A"]], []],
  ["A->A, A->B, B-|A", [["A", "A"], ["A", "B"]], [["B", "A"]]],
  ["A->A, A-|B, B->A", [["A", "A"], ["B", "A"]], [["A", "B"]]],
  ["A->A, A-|B, B-|A", [["A", "A"]], [["A", "B"], ["B", "A"]]],
  ["A-|A, A->B, B->A", [["A", "B"], ["B", "A"]], [["A", "A"]]],
  ["A-|A, A->B, B-|A", [["A", "B"]], [["A", "A"], ["B", "A"]]],
  ["A-|A, A-|B, B->A", [["B", "A"]], [["A", "A"], ["A", "B"]]],
  ["A-|A, A-|B, B-|A", [], [["A", "A"], ["A", "B"], ["B", "A"]]],
  ["A->A, B->B, B->A", [["A", "A"], ["B", "B"], ["B", "A"]], []],
  ["A->A, B->B, B-|A", [["A", "A"], ["B", "B"]], [["B", "A"]]],
  ["A->A, B-|B, B->A", [["A", "A"], ["B", "A"]], [["B", "B"]]],
  ["A-|A, B->B, B-|A", [["B", "B"]], [["A", "A"], ["B", "A"]]],
  ["A-|A, B->B, B->A", [["B", "A"], ["B", "B"]], [["A", "A"]]],
  ["A->A, B-|B, B-|A", [["A", "A"]], [["B", "B"], ["B", "A"]]],
  ["A-|A, B-|B, B->A", [["B", "A"]], [["A", "A"], ["B", "B"]]],
  ["A-|A, B-|B, B-|A", [], [["A", "A"], ["B", "B"], ["B", "A"]]],
  ["A->A, B->B, A->B, B->A", [["A", "A"], ["B", "B"], ["A", "B"], ["B", "A"]], []],
  ["A->A, B->B, A->B, B-|A", [["A", "A"], ["B", "B"], ["A", "B"]], [["B", "A"]]],
  ["A->A, B->B, A-|B, B-|A", [["A", "A"], ["B", "B"]], [["A", "B"], ["B", "A"]]],
  ["A->A, B-|B, A->B, B-|A", [["A", "A"], ["A", "B"]], [["B", "B"], ["B", "A"]]],
  ["A->A, B-|B, A-|B, B->A", [["A", "A"], ["B", "A"]], [["B", "B"], ["A", "B"]]],
  ["A->A, B-|B, A-|B, B-|A", [["A", "A"]], [["B", "B"], ["A", "B"], ["B", "A"]]],
  ["A-|A, B->B, A->B, B->A", [["A", "B"], ["B", "B"], ["B", "A"]], [["A", "A"]]],
  ["A-|A, B-|B, A->B, B->A", [["A", "B"], ["B", "A"]], [["A", "A"], ["B", "B"]]],
  ["A-|A, B-|B, A->B, B-|A", [["A", "B"]], [["A", "A"], ["B", "B"], ["B", "A"]]],
  ["A-|A, B-|B, A-|B, B-|A", [], [["A", "A"], ["B", "B"], ["A", "B"], ["B", "A"]]],
];

const CLASS_SIZES = [2, 3, 4, 4, 8, 8, 10];
const CLASS_RULE_SETS = {
  1: ["AA", "OA"],
  2: ["AA"],
  3: ["AA"],
  4: ["AA", "AO", "OA", "OO"],
  // Class 5 has two inputs into A and one input into B in this canonical
  // ordering, so the second realization must switch A from AND to OR.
  5: ["AA", "OA"],
  // Class 6 is displayed in the B-to-A orientation, so the two-input rule is A.
  6: ["AA", "OA"],
  7: ["AA", "OA", "OO"],
};
const LOGIC_TYPES = {
  AA: ["AND", "AND"],
  AO: ["AND", "OR"],
  OA: ["OR", "AND"],
  OO: ["OR", "OR"],
};

const $ = (id) => document.getElementById(id);
const stateIndex = (s) => STATES.indexOf(s);
const stateFromBits = (a, b) => `${a}${b}`;
const edgeKey = (e) => `${e.from}->${e.to}`;

let currentMode = "preset";

function buildGraphs() {
  const graphs = [];
  let cursor = 0;
  CLASS_SIZES.forEach((size, classOffset) => {
    const classId = classOffset + 1;
    for (let i = 0; i < size; i += 1) {
      const raw = GRAPHS_RAW[cursor];
      graphs.push({
        id: cursor + 1,
        classId,
        graphInClass: i + 1,
        desc: raw[0],
        pos: raw[1].map((e) => e.join("")),
        neg: raw[2].map((e) => e.join("")),
      });
      cursor += 1;
    }
  });
  return graphs;
}

function buildNetworks() {
  const networks = [];
  buildGraphs().forEach((graph) => {
    CLASS_RULE_SETS[graph.classId].forEach((code) => {
      const gates = LOGIC_TYPES[code];
      const net = {
        id: networks.length + 1,
        graph,
        logicCode: code,
        gateA: gates[0],
        gateB: gates[1],
      };
      const tableA = tableFromPresetRule(net, "A");
      const tableB = tableFromPresetRule(net, "B");
      net.tableA = tableA;
      net.tableB = tableB;
      net.ruleA = `A' = ${ruleRhsText(graph, "A", net.gateA)}`;
      net.ruleB = `B' = ${ruleRhsText(graph, "B", net.gateB)}`;
      networks.push(net);
    });
  });
  return networks;
}

const NETWORKS = buildNetworks();

function hasEdge(graph, src, dst, kind) {
  return graph[kind].includes(`${src}${dst}`);
}

function incoming(graph, target) {
  return ["A", "B"].flatMap((src) => {
    if (hasEdge(graph, src, target, "pos")) return [{ src, neg: false }];
    if (hasEdge(graph, src, target, "neg")) return [{ src, neg: true }];
    return [];
  });
}

function literalValue(src, neg, state) {
  const bits = STATE_BITS[state];
  const value = src === "A" ? bits[0] : bits[1];
  return neg ? 1 - value : value;
}

function evalGate(values, gate) {
  if (gate === "AND") return values.every(Boolean) ? 1 : 0;
  return values.some(Boolean) ? 1 : 0;
}

function ruleRhsText(graph, target, gate) {
  const lits = incoming(graph, target);
  if (!lits.length) return gate === "AND" ? "1" : "0";
  const parts = lits.map((lit) => `${lit.neg ? "!" : ""}${lit.src}`);
  return parts.join(gate === "AND" ? " & " : " | ");
}

function tableFromPresetRule(net, target) {
  const gate = target === "A" ? net.gateA : net.gateB;
  return STATES.map((state) => {
    const values = incoming(net.graph, target).map((lit) => literalValue(lit.src, lit.neg, state));
    return evalGate(values, gate);
  });
}

function evalTable(table, state) {
  return table[stateIndex(state)];
}

function syncNext(model, state) {
  return stateFromBits(evalTable(model.tableA, state), evalTable(model.tableB, state));
}

function asyncANext(model, state) {
  return stateFromBits(evalTable(model.tableA, state), STATE_BITS[state][1]);
}

function asyncBNext(model, state) {
  return stateFromBits(STATE_BITS[state][0], evalTable(model.tableB, state));
}

function transitionRows(model) {
  return STATES.map((s) => ({
    start: s,
    sync: syncNext(model, s),
    a: asyncANext(model, s),
    b: asyncBNext(model, s),
  }));
}

function stgEdges(model, scheme, showSelfLoops) {
  const map = new Map();
  const add = (from, to, label, cls) => {
    if (!showSelfLoops && from === to) return;
    const key = `${from}->${to}`;
    if (map.has(key)) {
      const e = map.get(key);
      if (!e.labels.includes(label)) e.labels.push(label);
      e.cls = e.labels.length > 1 ? "ab-edge" : e.cls;
    } else {
      map.set(key, { from, to, labels: [label], cls });
    }
  };

  STATES.forEach((s) => {
    if (scheme === "sync") {
      add(s, syncNext(model, s), "sync", "sync");
    } else {
      add(s, asyncANext(model, s), "A", "a-edge");
      add(s, asyncBNext(model, s), "B", "b-edge");
    }
  });

  if (!showSelfLoops) {
    STATES.forEach((s) => {
      const hasOut = [...map.values()].some((e) => e.from === s);
      if (!hasOut) {
        if (scheme === "sync") add(s, syncNext(model, s), "sync", "sync");
        else {
          const a = asyncANext(model, s);
          const b = asyncBNext(model, s);
          if (a === s && b === s) add(s, s, "A/B", "ab-edge");
        }
      }
    });
  }

  return [...map.values()].map((e) => ({ ...e, label: e.labels.join("/") }));
}

function fullEdgesForAttractors(model, scheme) {
  return stgEdges(model, scheme, true);
}

function terminalSccs(model, scheme) {
  const edges = fullEdgesForAttractors(model, scheme);
  const adj = Object.fromEntries(STATES.map((s) => [s, []]));
  edges.forEach((e) => adj[e.from].push(e.to));

  const visited = new Set();
  const order = [];
  function dfs(v) {
    visited.add(v);
    adj[v].forEach((to) => {
      if (!visited.has(to)) dfs(to);
    });
    order.push(v);
  }
  STATES.forEach((s) => {
    if (!visited.has(s)) dfs(s);
  });

  const radj = Object.fromEntries(STATES.map((s) => [s, []]));
  edges.forEach((e) => radj[e.to].push(e.from));
  const comps = [];
  visited.clear();
  function rdfs(v, comp) {
    visited.add(v);
    comp.push(v);
    radj[v].forEach((to) => {
      if (!visited.has(to)) rdfs(to, comp);
    });
  }
  order.reverse().forEach((s) => {
    if (!visited.has(s)) {
      const comp = [];
      rdfs(s, comp);
      comps.push(comp.sort((a, b) => stateIndex(a) - stateIndex(b)));
    }
  });

  return comps.filter((comp) => {
    const set = new Set(comp);
    return comp.every((v) => adj[v].every((to) => set.has(to)));
  });
}

function inferInfluence(table, variable) {
  const pairs = variable === "A" ? [[0, 2], [1, 3]] : [[0, 1], [2, 3]];
  const diffs = pairs.map(([lo, hi]) => table[hi] - table[lo]);
  if (diffs.every((d) => d === 0)) return "none";
  if (diffs.every((d) => d >= 0) && diffs.some((d) => d > 0)) return "pos";
  if (diffs.every((d) => d <= 0) && diffs.some((d) => d < 0)) return "neg";
  return "mixed";
}

function graphFromTables(tableA, tableB) {
  const graph = { id: 0, classId: "-", graphInClass: "-", desc: "custom inferred graph", pos: [], neg: [], mixed: [] };
  [["A", tableA], ["B", tableB]].forEach(([target, table]) => {
    ["A", "B"].forEach((src) => {
      const influence = inferInfluence(table, src);
      const key = `${src}${target}`;
      if (influence === "pos") graph.pos.push(key);
      if (influence === "neg") graph.neg.push(key);
      if (influence === "mixed") graph.mixed.push(key);
    });
  });
  return graph;
}

function currentModel() {
  if (currentMode === "preset") {
    return NETWORKS[Number($("networkSelect").value) - 1];
  }
  const optA = FUNCTION_OPTIONS[Number($("ruleASelect").value)];
  const optB = FUNCTION_OPTIONS[Number($("ruleBSelect").value)];
  const graph = graphFromTables(optA.table, optB.table);
  return {
    id: "custom",
    graph,
    logicCode: "custom",
    tableA: optA.table,
    tableB: optB.table,
    ruleA: `A' = ${optA.expr}`,
    ruleB: `B' = ${optB.expr}`,
  };
}

function svgEl(name, attrs = {}) {
  const el = document.createElementNS("http://www.w3.org/2000/svg", name);
  Object.entries(attrs).forEach(([key, value]) => el.setAttribute(key, value));
  return el;
}

function clearSvg(svg) {
  while (svg.firstChild) svg.removeChild(svg.firstChild);
}

function ensureMarkers(svg) {
  const defs = svgEl("defs");
  [["arrow", "#344054"], ["arrow-blue", "#4285f4"], ["arrow-red", "#db4437"], ["arrow-purple", "#8b5cf6"], ["arrow-green", "#0f9d58"]].forEach(([id, color]) => {
    const marker = svgEl("marker", {
      id,
      markerWidth: "12",
      markerHeight: "12",
      refX: "10",
      refY: "6",
      orient: "auto",
      markerUnits: "strokeWidth",
    });
    marker.appendChild(svgEl("path", { d: "M 1 1 L 11 6 L 1 11 z", fill: color }));
    defs.appendChild(marker);
  });
  svg.appendChild(defs);
}

function drawNetwork(model) {
  const svg = $("networkSvg");
  clearSvg(svg);
  ensureMarkers(svg);
  const pos = { A: [170, 160], B: [350, 160] };
  const edges = [];
  model.graph.pos.forEach((key) => edges.push({ key, type: "pos" }));
  model.graph.neg.forEach((key) => edges.push({ key, type: "neg" }));
  (model.graph.mixed || []).forEach((key) => edges.push({ key, type: "mixed" }));

  edges.forEach((edge) => {
    const [src, dst] = edge.key.split("");
    const cls = edge.type === "pos" ? "reg-pos" : edge.type === "neg" ? "reg-neg" : "reg-mixed";
    const colorMarker = edge.type === "pos" ? "arrow-green" : edge.type === "neg" ? "" : "arrow-purple";

    if (src === dst) {
      const [x, y] = pos[src];
      const up = src === "A" ? -1 : 1;
      const path = svgEl("path", {
        d: `M ${x - 42} ${y + up * 4} C ${x - 105} ${y + up * 105}, ${x + 105} ${y + up * 105}, ${x + 42} ${y + up * 4}`,
        class: `edge ${cls}`,
        "marker-end": edge.type === "neg" ? "" : `url(#${colorMarker})`,
      });
      svg.appendChild(path);
      if (edge.type === "neg") drawInhibitionBar(svg, x + 40, y + up * 10, 1, 0);
      return;
    }

    const [x1, y1] = pos[src];
    const [x2, y2] = pos[dst];
    const reverse = edges.some((e) => e.key === `${dst}${src}`);
    const lift = reverse ? (src === "A" ? -56 : 56) : 0;
    const path = svgEl("path", {
      d: `M ${x1 + (src === "A" ? 46 : -46)} ${y1} C ${(x1 + x2) / 2} ${y1 + lift}, ${(x1 + x2) / 2} ${y2 + lift}, ${x2 + (dst === "A" ? 46 : -46)} ${y2}`,
      class: `edge ${cls}`,
      "marker-end": edge.type === "neg" ? "" : `url(#${colorMarker})`,
    });
    svg.appendChild(path);
    if (edge.type === "neg") {
      const direction = dst === "B" ? 1 : -1;
      drawInhibitionBar(svg, x2 + (dst === "A" ? 50 : -50), y2, direction, 0);
    }
  });

  ["A", "B"].forEach((node) => {
    const [x, y] = pos[node];
    svg.appendChild(svgEl("circle", { cx: x, cy: y, r: 39, class: "node" }));
    const label = svgEl("text", { x, y, class: "net-label", "font-size": "28" });
    label.textContent = node;
    svg.appendChild(label);
  });
}

function drawInhibitionBar(svg, x, y, dx, dy) {
  const g = svgEl("line", {
    x1: x + 14 * -dy,
    y1: y + 14 * dx,
    x2: x - 14 * -dy,
    y2: y - 14 * dx,
    stroke: "#db4437",
    "stroke-width": "5",
    "stroke-linecap": "butt",
  });
  svg.appendChild(g);
}

function drawStg(model) {
  const svg = $("stgSvg");
  clearSvg(svg);
  ensureMarkers(svg);
  const scheme = $("schemeSelect").value;
  const showSelfLoops = $("selfLoopToggle").checked;
  const edges = stgEdges(model, scheme, showSelfLoops);
  const traps = terminalSccs(model, scheme);
  const attractorNodes = new Set(traps.flat());
  const pos = {
    "00": [185, 125],
    "01": [535, 125],
    "10": [185, 395],
    "11": [535, 395],
  };

  const pairCounts = {};
  edges.forEach((e) => {
    const a = [e.from, e.to].sort().join("-");
    pairCounts[a] = (pairCounts[a] || 0) + 1;
  });

  edges.forEach((e) => {
    const [x1, y1] = pos[e.from];
    const [x2, y2] = pos[e.to];
    const marker = e.cls === "a-edge" ? "arrow-red" : e.cls === "b-edge" ? "arrow-blue" : e.cls === "ab-edge" ? "arrow-purple" : "arrow-blue";

    if (e.from === e.to) {
      const anchor = selfLoopAnchor(e.from, x1, y1);
      const path = svgEl("path", {
        d: anchor.d,
        class: `edge ${e.cls} self`,
        "marker-end": `url(#${marker})`,
      });
      svg.appendChild(path);
      addEdgeLabel(svg, anchor.lx, anchor.ly, e.label);
      return;
    }

    const dx = x2 - x1;
    const dy = y2 - y1;
    const len = Math.hypot(dx, dy);
    const ux = dx / len;
    const uy = dy / len;
    const start = [x1 + ux * 44, y1 + uy * 44];
    const end = [x2 - ux * 52, y2 - uy * 52];
    const reverseExists = edges.some((other) => other.from === e.to && other.to === e.from);
    // Use the same bend sign for both directions. Because the direction vector
    // reverses, this places reciprocal arrows on opposite sides of the nodes.
    const bend = reverseExists ? 58 : 0;
    const mx = (start[0] + end[0]) / 2 - uy * bend;
    const my = (start[1] + end[1]) / 2 + ux * bend;
    const path = svgEl("path", {
      d: `M ${start[0]} ${start[1]} Q ${mx} ${my} ${end[0]} ${end[1]}`,
      class: `edge ${e.cls}`,
      "marker-end": `url(#${marker})`,
    });
    svg.appendChild(path);
    addEdgeLabel(svg, mx, my - 8, e.label);
  });

  STATES.forEach((s) => {
    const [x, y] = pos[s];
    svg.appendChild(svgEl("rect", {
      x: x - 43,
      y: y - 34,
      width: 86,
      height: 68,
      rx: 7,
      class: `state-node ${attractorNodes.has(s) ? "attractor" : ""}`,
    }));
    const label = svgEl("text", { x, y, class: "state-label", "font-size": "25" });
    label.textContent = s;
    svg.appendChild(label);
  });

  $("edgeCountText").textContent = `${edges.length} visible edge${edges.length === 1 ? "" : "s"}`;
}

function selfLoopAnchor(state, x, y) {
  if (state === "00") return { d: `M ${x - 38} ${y - 30} C ${x - 110} ${y - 106}, ${x + 64} ${y - 108}, ${x + 40} ${y - 35}`, lx: x - 8, ly: y - 86 };
  if (state === "01") return { d: `M ${x + 38} ${y - 30} C ${x + 110} ${y - 106}, ${x - 64} ${y - 108}, ${x - 40} ${y - 35}`, lx: x + 8, ly: y - 86 };
  if (state === "10") return { d: `M ${x - 38} ${y + 30} C ${x - 110} ${y + 106}, ${x + 64} ${y + 108}, ${x + 40} ${y + 35}`, lx: x - 8, ly: y + 96 };
  return { d: `M ${x + 38} ${y + 30} C ${x + 110} ${y + 106}, ${x - 64} ${y + 108}, ${x - 40} ${y + 35}`, lx: x + 8, ly: y + 96 };
}

function addEdgeLabel(svg, x, y, text) {
  const label = svgEl("text", { x, y, class: "edge-label", "text-anchor": "middle" });
  label.textContent = text;
  svg.appendChild(label);
}

function updateTable(model) {
  const body = $("truthBody");
  body.innerHTML = "";
  transitionRows(model).forEach((row) => {
    const tr = document.createElement("tr");
    [row.start, row.sync, row.a, row.b].forEach((value) => {
      const td = document.createElement("td");
      td.textContent = value;
      tr.appendChild(td);
    });
    body.appendChild(tr);
  });
}

function updateMeta(model) {
  const scheme = $("schemeSelect").value;
  const traps = terminalSccs(model, scheme);
  $("topologyText").textContent = currentMode === "preset"
    ? `C${model.graph.classId}-G${model.graph.graphInClass}, ${model.graph.desc}, ${model.logicCode}`
    : model.graph.desc;
  $("ruleAText").textContent = model.ruleA;
  $("ruleBText").textContent = model.ruleB;
  $("attractorText").textContent = traps.map((comp) => `{${comp.join(", ")}}`).join("; ") || "None";
}

function render() {
  const model = currentModel();
  drawNetwork(model);
  drawStg(model);
  updateTable(model);
  updateMeta(model);
}

function setMode(mode) {
  currentMode = mode;
  $("presetMode").classList.toggle("active", mode === "preset");
  $("customMode").classList.toggle("active", mode === "custom");
  document.querySelectorAll(".preset-only").forEach((el) => el.classList.toggle("hidden", mode !== "preset"));
  document.querySelectorAll(".custom-only").forEach((el) => el.classList.toggle("hidden", mode !== "custom"));
  render();
}

function populateControls() {
  const networkSelect = $("networkSelect");
  NETWORKS.forEach((net) => {
    const option = document.createElement("option");
    option.value = String(net.id);
    option.textContent = `N${net.id} | C${net.graph.classId}-G${net.graph.graphInClass} ${net.logicCode} | ${net.ruleA}; ${net.ruleB}`;
    networkSelect.appendChild(option);
  });

  [$("ruleASelect"), $("ruleBSelect")].forEach((select, selectIndex) => {
    FUNCTION_OPTIONS.forEach((fn, i) => {
      const option = document.createElement("option");
      option.value = String(i);
      option.textContent = `${selectIndex === 0 ? "A" : "B"}' = ${fn.expr}`;
      select.appendChild(option);
    });
  });
  $("ruleASelect").value = String(FUNCTION_OPTIONS.findIndex((f) => f.expr === "A & B"));
  $("ruleBSelect").value = String(FUNCTION_OPTIONS.findIndex((f) => f.expr === "A"));
}

populateControls();
$("presetMode").addEventListener("click", () => setMode("preset"));
$("customMode").addEventListener("click", () => setMode("custom"));
["networkSelect", "ruleASelect", "ruleBSelect", "schemeSelect", "selfLoopToggle"].forEach((id) => {
  $(id).addEventListener("change", render);
});
render();
