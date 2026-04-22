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
}

export interface Graph {
  nodes: GraphNode[];
  edges: GraphEdge[];
}

export interface GraphFile {
  graphs: Record<string, Graph>;
}
