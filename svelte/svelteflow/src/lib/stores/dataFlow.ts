import { writable } from "svelte/store";

type NodeData = {
  [nodeId: string]: any;
};

export const nodeDataStore = writable<NodeData>({});

export function setNodeOutput(nodeId: string, data: any) {
  nodeDataStore.update((store) => ({
    ...store,
    [nodeId]: data,
  }));
}

export function getNodeInput(nodeId: string) {
  let data;
  nodeDataStore.subscribe((store) => {
    data = store[nodeId];
  })();
  return data;
}
