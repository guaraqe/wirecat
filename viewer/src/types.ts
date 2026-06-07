export interface Plug {
  node: string;
  attr: string;
}

export interface GraphEdge {
  source: Plug;
  target: Plug;
}

export interface GraphNode {
  nodeId: string;
  name: string;
  input: Record<string, string>;
  output: Record<string, string>;
  subgraph?: string | null;
  boundary?: "InputBoundary" | "OutputBoundary" | null;
}

export interface Graph {
  nodes: GraphNode[];
  edges: GraphEdge[];
  location?: string | null;
}

export interface GraphFile {
  graphs: Record<string, Graph>;
}
