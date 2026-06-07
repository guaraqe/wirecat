import ELK, { type ElkNode } from "elkjs/lib/elk.bundled.js";
import type { Graph, GraphNode } from "./types";

export interface Point {
  x: number;
  y: number;
}

export interface EdgeRouting {
  start: Point;
  end: Point;
  bendPoints: Point[];
  label?: { text: string; x: number; y: number };
}

const LABEL_CHAR_WIDTH = 6.5;
const LABEL_HEIGHT = 14;
const LABEL_H_PADDING = 8;

export function labelSize(text: string): { width: number; height: number } {
  return {
    width: Math.max(12, Math.ceil(text.length * LABEL_CHAR_WIDTH) + LABEL_H_PADDING),
    height: LABEL_HEIGHT,
  };
}

const elk = new ELK();

const PORT_WIDTH = 70;
const MIN_NODE_WIDTH = 140;
const NODE_HEIGHT = 90;

export function portCount(n: GraphNode): { inputs: number; outputs: number } {
  return {
    inputs: Object.keys(n.input).length,
    outputs: Object.keys(n.output).length,
  };
}

export function nodeWidth(n: GraphNode): number {
  const { inputs, outputs } = portCount(n);
  return Math.max(MIN_NODE_WIDTH, Math.max(inputs, outputs, 1) * PORT_WIDTH);
}

export function nodeHeight(node: GraphNode): number {
  return node.boundary ? 64 : NODE_HEIGHT;
}

export async function computeLayout(
  graph: Graph,
  edgeLabelFor: (edgeIndex: number) => string,
): Promise<ElkNode> {
  const elkGraph = {
    id: "root",
    layoutOptions: {
      "elk.algorithm": "layered",
      "elk.direction": "DOWN",
      "elk.edgeRouting": "SPLINES",
      "elk.layered.spacing.nodeNodeBetweenLayers": "40",
      "elk.spacing.nodeNode": "40",
      "elk.layered.nodePlacement.strategy": "NETWORK_SIMPLEX",
      "elk.edgeLabels.placement": "CENTER",
      "elk.edgeLabels.inline": "true",
      "elk.spacing.edgeLabel": "6",
    },
    children: graph.nodes.map((n) => {
      const inputs = Object.keys(n.input);
      const outputs = Object.keys(n.output);
      const w = nodeWidth(n);
      const h = nodeHeight(n);
      const inCount = Math.max(1, inputs.length);
      const outCount = Math.max(1, outputs.length);
      return {
        id: n.nodeId,
        width: w,
        height: h,
        properties: {
          "org.eclipse.elk.portConstraints": "FIXED_POS",
        },
        ports: [
          ...inputs.map((attr, i) => ({
            id: portId(n.nodeId, "in", attr),
            x: ((i + 0.5) * w) / inCount,
            y: 0,
            width: 1,
            height: 1,
            properties: { "org.eclipse.elk.port.side": "NORTH" },
          })),
          ...outputs.map((attr, i) => ({
            id: portId(n.nodeId, "out", attr),
            x: ((i + 0.5) * w) / outCount,
            y: h,
            width: 1,
            height: 1,
            properties: { "org.eclipse.elk.port.side": "SOUTH" },
          })),
        ],
      };
    }),
    edges: graph.edges.map((e, i) => {
      const text = edgeLabelFor(i);
      const size = labelSize(text);
      return {
        id: `e${i}`,
        sources: [portId(e.source.node, "out", e.source.attr)],
        targets: [portId(e.target.node, "in", e.target.attr)],
        labels: text
          ? [
              {
                text,
                width: size.width,
                height: size.height,
              },
            ]
          : [],
      };
    }),
  };
  return elk.layout(elkGraph);
}

export function portId(
  nodeId: string,
  dir: "in" | "out",
  attr: string,
): string {
  return `${nodeId}__${dir}__${attr}`;
}

export function handleId(dir: "in" | "out", attr: string): string {
  return `${dir}__${attr}`;
}

export function extractEdgeRoutings(laid: ElkNode): Map<string, EdgeRouting> {
  const out = new Map<string, EdgeRouting>();
  const edges = (laid as unknown as { edges?: Array<Record<string, unknown>> })
    .edges;
  if (!edges) return out;
  for (const e of edges) {
    const id = e.id as string;
    const sections = e.sections as
      | Array<{
          startPoint: Point;
          endPoint: Point;
          bendPoints?: Point[];
        }>
      | undefined;
    if (!sections || sections.length === 0) continue;
    const s = sections[0];
    const labels = e.labels as
      | Array<{
          text?: string;
          x?: number;
          y?: number;
          width?: number;
          height?: number;
        }>
      | undefined;
    let label: EdgeRouting["label"] = undefined;
    if (labels && labels.length > 0) {
      const l = labels[0];
      if (
        l.text &&
        typeof l.x === "number" &&
        typeof l.y === "number" &&
        typeof l.width === "number" &&
        typeof l.height === "number"
      ) {
        label = {
          text: l.text,
          x: l.x + l.width / 2,
          y: l.y + l.height / 2,
        };
      }
    }
    out.set(id, {
      start: s.startPoint,
      end: s.endPoint,
      bendPoints: s.bendPoints ?? [],
      label,
    });
  }
  return out;
}

export function routingToSvgPath(r: EdgeRouting): string {
  // Full control polygon: [start, ...bendPoints, end]. Each cubic Bezier takes
  // 4 consecutive points and consecutive segments share endpoints, so we walk
  // the list reading 3 new points per step. Expected length is 3n+1.
  const pts = [r.start, ...r.bendPoints, r.end];
  if (pts.length < 2) return `M ${r.start.x} ${r.start.y}`;
  const parts: string[] = [`M ${pts[0].x} ${pts[0].y}`];
  let i = 1;
  while (i + 2 < pts.length) {
    const c1 = pts[i];
    const c2 = pts[i + 1];
    const p = pts[i + 2];
    parts.push(`C ${c1.x} ${c1.y}, ${c2.x} ${c2.y}, ${p.x} ${p.y}`);
    i += 3;
  }
  // Fallback for unexpected lengths: line to any leftover points.
  for (; i < pts.length; i++) {
    parts.push(`L ${pts[i].x} ${pts[i].y}`);
  }
  return parts.join(" ");
}
