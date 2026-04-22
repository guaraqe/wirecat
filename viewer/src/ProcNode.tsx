import { Handle, Position, type NodeProps } from "@xyflow/react";
import { handleId } from "./layout";

export interface ProcNodeData {
  name: string;
  inputs: Array<{ attr: string; type: string }>;
  outputs: Array<{ attr: string; type: string }>;
  [key: string]: unknown;
}

export function ProcNode({ data }: NodeProps) {
  const d = data as ProcNodeData;
  return (
    <div className="proc-node">
      <div className="port-row port-row-top">
        {d.inputs.map((p) => (
          <div key={p.attr} className="port">
            <Handle
              type="target"
              position={Position.Top}
              id={handleId("in", p.attr)}
              className="port-handle"
            />
            <span className="port-label">{p.attr}</span>
          </div>
        ))}
      </div>
      <div className="node-title">{d.name}</div>
      <div className="port-row port-row-bottom">
        {d.outputs.map((p) => (
          <div key={p.attr} className="port">
            <span className="port-label">{p.attr}</span>
            <Handle
              type="source"
              position={Position.Bottom}
              id={handleId("out", p.attr)}
              className="port-handle"
            />
          </div>
        ))}
      </div>
    </div>
  );
}
