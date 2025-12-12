import { writable, get } from "svelte/store";

type WorkflowData = {
  [nodeId: string]: any;
};

// Store all node outputs
export const workflowData = writable<WorkflowData>({});

// Update a node's output data
export function updateNodeData(nodeId: string, data: any) {
  workflowData.update((store) => ({
    ...store,
    [nodeId]: data,
  }));
  console.log(`Node ${nodeId} updated:`, data);
}

// Get input data for a node (from connected source nodes)
export function getNodeInputData(nodeId: string, edges: any[]) {
  // Find all edges that connect TO this node
  const incomingEdges = edges.filter((edge) => edge.target === nodeId);

  // Get data from all source nodes
  const inputData = incomingEdges.map((edge) => {
    const sourceData = get(workflowData)[edge.source];
    return {
      sourceNodeId: edge.source,
      data: sourceData,
    };
  });

  // Return the first one for now (you can merge multiple inputs later)
  return inputData[0]?.data || null;
}

// Execute workflow from a starting node
export function executeWorkflow(
  startNodeId: string,
  nodes: any[],
  edges: any[],
) {
  console.log("Executing workflow from node:", startNodeId);

  // Find all nodes connected from this one
  const outgoingEdges = edges.filter((edge) => edge.source === startNodeId);

  // Trigger execution on connected nodes
  outgoingEdges.forEach((edge) => {
    const targetNode = nodes.find((n) => n.id === edge.target);
    console.log(`Data flowing to node: ${edge.target} (${targetNode?.type})`);
  });
}
