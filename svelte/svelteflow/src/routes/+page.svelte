<script lang="ts">
    import {
      SvelteFlow,
      Controls,
      Background,
      BackgroundVariant,
      MiniMap,
      type Node
    } from '@xyflow/svelte';
    import '@xyflow/svelte/dist/style.css';

    import InputNode from '$lib/nodes/InputNode.svelte';
    import ProcessorNode from '$lib/nodes/ProcessorNode.svelte';
    import OutputNode from '$lib/nodes/OutputNode.svelte';
    import Toolbar from '$lib/components/Toolbar.svelte';

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

    // Track selected nodes
    let selectedNodes = $state<Node[]>([]);

    // Counter for unique IDs
    let nodeIdCounter = $state(4); // Start after existing nodes

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

    // Track node selection
    function handleNodeClick(event: any) {
      const clickedNode = event.detail.node;

      // Toggle selection
      const isSelected = selectedNodes.some(n => n.id === clickedNode.id);

      if (isSelected) {
        selectedNodes = selectedNodes.filter(n => n.id !== clickedNode.id);
      } else {
        selectedNodes = [...selectedNodes, clickedNode];
      }

      console.log('Selected nodes:', selectedNodes.map(n => n.id));
    }

    // Clear selection when clicking canvas
    function handlePaneClick() {
      selectedNodes = [];
    }

    // Add a new node
    function addNode(type: 'input' | 'processor' | 'output') {
      const newNode = {
        id: String(nodeIdCounter),
        type,
        data: {},
        position: {
          // Stagger new nodes so they don't overlap
          x: 100 + (nodeIdCounter * 30),
          y: 100 + (nodeIdCounter * 30)
        }
      };

      nodes = [...nodes, newNode];
      nodeIdCounter++;

      console.log(`✅ Added ${type} node:`, newNode.id);
    }

    // Delete selected nodes
    function deleteSelectedNodes() {
      if (selectedNodes.length === 0) return;

      const selectedIds = selectedNodes.map(n => n.id);

      // Remove nodes
      nodes = nodes.filter(n => !selectedIds.includes(n.id));

      // Remove edges connected to deleted nodes
      edges = edges.filter(edge =>
        !selectedIds.includes(edge.source) &&
        !selectedIds.includes(edge.target)
      );

      console.log(`🗑️ Deleted ${selectedNodes.length} node(s)`);
      selectedNodes = [];
    }

    // Keyboard shortcuts
    $effect(() => {
      function handleKeyPress(e: KeyboardEvent) {
        // Delete key or Backspace
        if ((e.key === 'Delete' || e.key === 'Backspace') && selectedNodes.length > 0) {
          // Only if not typing in an input
          if (!(e.target instanceof HTMLInputElement) && !(e.target instanceof HTMLTextAreaElement)) {
            e.preventDefault();
            deleteSelectedNodes();
          }
        }

        // Escape to clear selection
        if (e.key === 'Escape') {
          selectedNodes = [];
        }
      }

      window.addEventListener('keydown', handleKeyPress);

      return () => {
        window.removeEventListener('keydown', handleKeyPress);
      };
    });
  </script>

<div style="height: 100vh; position: relative;">
    <Toolbar
      onAddNode={addNode}
      onDeleteSelected={deleteSelectedNodes}
      {selectedNodes}
    />

    <SvelteFlow
      bind:nodes
      bind:edges
      {nodeTypes}
      onconnect={handleConnect}
      ondisconnect={handleDisconnect}
      onnodeclick={handleNodeClick}
      onpaneclick={handlePaneClick}
    >
      <Controls />
      <Background variant={BackgroundVariant.Dots} />
      <MiniMap />
    </SvelteFlow>
  </div>
