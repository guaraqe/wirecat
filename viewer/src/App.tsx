import { useCallback, useEffect, useMemo, useState } from "react";
import {
  Background,
  Controls,
  ReactFlow,
  type Edge as RFEdge,
  type Node as RFNode,
} from "@xyflow/react";
import "@xyflow/react/dist/style.css";
import { ProcNode } from "./ProcNode";
import { SplineEdge } from "./SplineEdge";
import {
  computeLayout,
  extractEdgeRoutings,
  handleId,
  nodeHeight,
  nodeWidth,
} from "./layout";
import type { Graph, GraphFile, GraphNode } from "./types";

const logoUrl = new URL("./assets/wirecat-logo-only.svg", import.meta.url).href;
const wordmarkUrl = new URL("./assets/wirecat-text-only.svg", import.meta.url).href;
const nodeTypes = { proc: ProcNode };
const edgeTypes = { spline: SplineEdge };

function nodeByIdMap(nodes: GraphNode[]): Record<string, GraphNode> {
  return Object.fromEntries(nodes.map((n) => [n.nodeId, n]));
}

function inlineGraphNode(parent: Graph, nodeId: string, child: Graph): Graph {
  const target = parent.nodes.find((node) => node.nodeId === nodeId);
  if (!target) return parent;

  const prefix = `${nodeId}/`;
  const inputBoundary = child.nodes.find(
    (node) => node.boundary === "InputBoundary",
  );
  const outputBoundary = child.nodes.find(
    (node) => node.boundary === "OutputBoundary",
  );
  const boundaryIds = new Set(
    [inputBoundary?.nodeId, outputBoundary?.nodeId].filter(
      (id): id is string => Boolean(id),
    ),
  );
  const childNodes = child.nodes
    .filter((node) => !boundaryIds.has(node.nodeId))
    .map((node) => ({
    ...node,
    nodeId: `${prefix}${node.nodeId}`,
    }));
  const internalEdges = child.edges.filter(
    (edge) =>
      !boundaryIds.has(edge.source.node) && !boundaryIds.has(edge.target.node),
  );
  const childEdges = internalEdges.map((edge) => ({
    source: { ...edge.source, node: `${prefix}${edge.source.node}` },
    target: { ...edge.target, node: `${prefix}${edge.target.node}` },
  }));
  const inputTargets = (attr: string) =>
    child.edges
      .filter(
        (edge) =>
          edge.source.node === inputBoundary?.nodeId &&
          edge.source.attr === attr &&
          edge.target.node !== outputBoundary?.nodeId,
      )
      .map((edge) => ({ ...edge.target, node: `${prefix}${edge.target.node}` }));
  const outputSources = (attr: string) =>
    child.edges
      .filter(
        (edge) =>
          edge.target.node === outputBoundary?.nodeId &&
          edge.target.attr === attr &&
          edge.source.node !== inputBoundary?.nodeId,
      )
      .map((edge) => ({ ...edge.source, node: `${prefix}${edge.source.node}` }));

  const retainedEdges = parent.edges.filter(
    (edge) => edge.source.node !== nodeId && edge.target.node !== nodeId,
  );
  const incomingEdges = parent.edges
    .filter((edge) => edge.target.node === nodeId)
    .flatMap((edge) =>
      inputTargets(edge.target.attr).map((targetPlug) => ({
        source: edge.source,
        target: targetPlug,
      })),
    );
  const outgoingEdges = parent.edges
    .filter((edge) => edge.source.node === nodeId)
    .flatMap((edge) =>
      outputSources(edge.source.attr).map((sourcePlug) => ({
        source: sourcePlug,
        target: edge.target,
      })),
    );
  const passthroughEdges = child.edges
    .filter(
      (edge) =>
        edge.source.node === inputBoundary?.nodeId &&
        edge.target.node === outputBoundary?.nodeId,
    )
    .flatMap((childEdge) => {
      const parentInputs = parent.edges.filter(
        (edge) =>
          edge.target.node === nodeId &&
          edge.target.attr === childEdge.source.attr,
      );
      const parentOutputs = parent.edges.filter(
        (edge) =>
          edge.source.node === nodeId &&
          edge.source.attr === childEdge.target.attr,
      );
      return parentInputs.flatMap((inputEdge) =>
        parentOutputs.map((outputEdge) => ({
          source: inputEdge.source,
          target: outputEdge.target,
        })),
      );
    });

  return {
    nodes: [
      ...parent.nodes.filter((node) => node.nodeId !== nodeId),
      ...childNodes,
    ],
    edges: [
      ...retainedEdges,
      ...childEdges,
      ...incomingEdges,
      ...outgoingEdges,
      ...passthroughEdges,
    ],
    location: parent.location,
  };
}

async function toReactFlow(graph: Graph): Promise<{
  nodes: RFNode[];
  edges: RFEdge[];
}> {
  const byId = nodeByIdMap(graph.nodes);
  const edgeLabelFor = (i: number): string => {
    const e = graph.edges[i];
    const src = byId[e.source.node];
    return src?.output[e.source.attr] ?? "";
  };
  const laid = await computeLayout(graph, edgeLabelFor);
  const positions: Record<string, { x: number; y: number }> = {};
  for (const child of laid.children ?? []) {
    positions[child.id] = { x: child.x ?? 0, y: child.y ?? 0 };
  }

  const nodes: RFNode[] = graph.nodes.map((n) => {
    const pos = positions[n.nodeId] ?? { x: 0, y: 0 };
    return {
      id: n.nodeId,
      type: "proc",
      position: pos,
      width: nodeWidth(n),
      height: nodeHeight(n),
      data: {
        name: n.name,
        subgraph: n.subgraph,
        boundary: n.boundary,
        inputs: Object.entries(n.input).map(([attr, type]) => ({ attr, type })),
        outputs: Object.entries(n.output).map(([attr, type]) => ({ attr, type })),
      },
    };
  });

  const routings = extractEdgeRoutings(laid);
  const edges: RFEdge[] = graph.edges.map((e, i) => {
    const id = `e${i}`;
    const typeName = edgeLabelFor(i);
    const routing = routings.get(id);
    return {
      id,
      source: e.source.node,
      sourceHandle: handleId("out", e.source.attr),
      target: e.target.node,
      targetHandle: handleId("in", e.target.attr),
      type: routing ? "spline" : "smoothstep",
      label: routing ? undefined : typeName,
      data: routing ? { routing } : undefined,
    };
  });

  return { nodes, edges };
}

export default function App() {
  const [file, setFile] = useState<GraphFile | null>(null);
  const [fileName, setFileName] = useState<string>("");
  const [selected, setSelected] = useState<string>("");
  const [selectedNodeId, setSelectedNodeId] = useState<string>("");
  const [displayGraph, setDisplayGraph] = useState<Graph | null>(null);
  const [rfNodes, setRfNodes] = useState<RFNode[]>([]);
  const [rfEdges, setRfEdges] = useState<RFEdge[]>([]);
  const [error, setError] = useState<string>("");

  const graphNames = useMemo(
    () => (file ? Object.keys(file.graphs) : []),
    [file],
  );
  const selectedGraph = file && selected ? file.graphs[selected] : null;
  const selectedNode =
    displayGraph?.nodes.find((node) => node.nodeId === selectedNodeId) ?? null;

  const onPick = useCallback((ev: React.ChangeEvent<HTMLInputElement>) => {
    const f = ev.target.files?.[0];
    if (!f) return;
    setFileName(f.name);
    f.text()
      .then((text) => {
        const parsed = JSON.parse(text) as GraphFile;
        if (!parsed.graphs || typeof parsed.graphs !== "object") {
          throw new Error("missing 'graphs' object");
        }
        setFile(parsed);
        setError("");
        const first = Object.keys(parsed.graphs)[0] ?? "";
        setSelected(first);
      })
      .catch((e: unknown) => {
        setError(String(e));
        setFile(null);
        setSelected("");
      });
  }, []);

  useEffect(() => {
    setDisplayGraph(selectedGraph);
  }, [selectedGraph]);

  useEffect(() => {
    if (!displayGraph) {
      setRfNodes([]);
      setRfEdges([]);
      return;
    }
    let cancelled = false;
    toReactFlow(displayGraph).then((res) => {
      if (cancelled) return;
      setRfNodes(res.nodes);
      setRfEdges(res.edges);
    });
    return () => {
      cancelled = true;
    };
  }, [displayGraph]);

  useEffect(() => {
    setSelectedNodeId("");
  }, [file, selected]);

  return (
    <div className="app">
      <header className="topbar">
        <div className="topbar-left">
          <img className="topbar-logo" src={logoUrl} alt="" />
          <label className="file-picker topbar-file-picker">
            <input
              type="file"
              accept="application/json,.json"
              onChange={onPick}
            />
            <span>Open JSON</span>
          </label>
          {fileName && <div className="topbar-filename">{fileName}</div>}
        </div>
        <img className="topbar-wordmark" src={wordmarkUrl} alt="WireCat" />
      </header>
      <div className="body">
        <aside className="sidebar">
          <h2 className="panel-title">Graphs</h2>
          {error && <div className="error">{error}</div>}
          {graphNames.length > 0 && (
            <nav className="graph-menu">
              {graphNames.map((name) => (
                <button
                  key={name}
                  type="button"
                  className={`graph-item${name === selected ? " active" : ""}`}
                  onClick={() => setSelected(name)}
                >
                  {name}
                </button>
              ))}
            </nav>
          )}
        </aside>
        <div className="canvas">
          {rfNodes.length > 0 ? (
            <ReactFlow
              nodes={rfNodes}
              edges={rfEdges}
              nodeTypes={nodeTypes}
              edgeTypes={edgeTypes}
              onNodeClick={(_, node) => setSelectedNodeId(node.id)}
              onPaneClick={() => setSelectedNodeId("")}
              fitView
              proOptions={{ hideAttribution: true }}
            >
              <Background />
              <Controls />
            </ReactFlow>
          ) : (
            <div className="placeholder">
              {file ? "Select a graph." : "Open a wirecat JSON file to begin."}
            </div>
          )}
        </div>
        <aside className="details-panel">
          <h2 className="panel-title">Node Info</h2>
          {selectedNode ? (
            <>
              <h3 className="details-title">{selectedNode.name}</h3>
              <section className="details-section">
                <h3>Inputs</h3>
                <PortTable ports={selectedNode.input} emptyText="No inputs" />
              </section>
              <section className="details-section">
                <h3>Outputs</h3>
                <PortTable ports={selectedNode.output} emptyText="No outputs" />
              </section>
              {selectedNode.subgraph && file?.graphs[selectedNode.subgraph] && (
                <section className="details-section subpipeline-actions">
                  <h3>Subpipeline</h3>
                  {file.graphs[selectedNode.subgraph].location && (
                    <div className="subpipeline-location">
                      {file.graphs[selectedNode.subgraph].location}
                    </div>
                  )}
                  <button
                    type="button"
                    onClick={() => setSelected(selectedNode.subgraph ?? "")}
                  >
                    Open
                  </button>
                  <button
                    type="button"
                    onClick={() => {
                      const child = file.graphs[selectedNode.subgraph ?? ""];
                      if (!displayGraph || !child) return;
                      setDisplayGraph(
                        inlineGraphNode(displayGraph, selectedNode.nodeId, child),
                      );
                      setSelectedNodeId("");
                    }}
                  >
                    Inline
                  </button>
                </section>
              )}
            </>
          ) : (
            <div className="details-placeholder">Select a node.</div>
          )}
        </aside>
      </div>
    </div>
  );
}

function PortTable({
  ports,
  emptyText,
}: {
  ports: Record<string, string>;
  emptyText: string;
}) {
  const entries = Object.entries(ports);
  if (entries.length === 0) {
    return <div className="port-empty">{emptyText}</div>;
  }

  return (
    <div className="port-table">
      {entries.map(([name, type]) => (
        <div className="port-detail" key={name}>
          <div className="port-detail-name">{name}</div>
          <div className="port-detail-type">{type}</div>
        </div>
      ))}
    </div>
  );
}
