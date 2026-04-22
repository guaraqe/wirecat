import { useMemo } from "react";
import { BaseEdge, EdgeLabelRenderer, type EdgeProps } from "@xyflow/react";
import { routingToSvgPath, type EdgeRouting } from "./layout";

export interface SplineEdgeData {
  routing: EdgeRouting;
  [key: string]: unknown;
}

export function SplineEdge(props: EdgeProps) {
  const data = props.data as SplineEdgeData | undefined;
  const path = useMemo(
    () => (data?.routing ? routingToSvgPath(data.routing) : ""),
    [data?.routing],
  );
  if (!data?.routing) return null;
  const label = data.routing.label;
  return (
    <>
      <BaseEdge id={props.id} path={path} markerEnd={props.markerEnd} />
      {label && (
        <EdgeLabelRenderer>
          <div
            className="spline-edge-label"
            style={{
              position: "absolute",
              transform: `translate(-50%, -50%) translate(${label.x}px, ${label.y}px)`,
            }}
          >
            {label.text}
          </div>
        </EdgeLabelRenderer>
      )}
    </>
  );
}
