<script lang="ts">
    import {
      SvelteFlow,
      Controls,
      Background,
      BackgroundVariant,
      MiniMap
    } from '@xyflow/svelte';
    import '@xyflow/svelte/dist/style.css';

    import InputNode from '$lib/nodes/InputNode.svelte';
    import ProcessorNode from '$lib/nodes/ProcessorNode.svelte';
    import OutputNode from '$lib/nodes/OutputNode.svelte';

    const nodeTypes = {
      input: InputNode,
      processor: ProcessorNode,
      output: OutputNode
    };

    let nodes = $state.raw([
      {
        id: '1',
        type: 'input',
        data: {},
        position: { x: 50, y: 100 }
      },
      {
        id: '2',
        type: 'processor',
        data: { sourceNodeId: '1' },
        position: { x: 350, y: 100 }
      },
      {
        id: '3',
        type: 'output',
        data: { sourceNodeId: '2' },
        position: { x: 650, y: 100 }
      }
    ]);

    let edges = $state.raw([
      { id: 'e1-2', source: '1', target: '2' },
      { id: 'e2-3', source: '2', target: '3' }
    ]);

    // Handle new connections
    function handleConnect(event: any) {
      const { source, target } = event.detail.connection;
      console.log(`🔗 Connected: ${source} -> ${target}`);

      // Update target node to know its source
      nodes = nodes.map(node => {
        if (node.id === target) {
          return {
            ...node,
            data: { ...node.data, sourceNodeId: source }
          };
        }
        return node;
      });
    }

    // Handle disconnections
    function handleDisconnect(event: any) {
      console.log('🔌 Disconnected:', event.detail);
    }
  </script>

<div style="height: 100vh;">
    <SvelteFlow
      bind:nodes
      bind:edges
      {nodeTypes}
      onconnect={handleConnect}
      ondisconnect={handleDisconnect}
    >
      <Controls />
      <Background variant={BackgroundVariant.Dots} />
      <MiniMap />
    </SvelteFlow>
  </div>
