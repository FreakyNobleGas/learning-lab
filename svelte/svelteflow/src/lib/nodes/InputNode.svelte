<script lang="ts">
    import { Handle, Position } from '@xyflow/svelte';
    import { updateNodeData } from '$lib/stores/workflowEngine';

    let { data, id } = $props();

    let pokemonName = $state('');

    function sendData() {
      if (!pokemonName.trim()) {
        alert('Please enter a Pokemon name!');
        return;
      }

      const inputData = {
        pokemonName: pokemonName.toLowerCase().trim(),
        timestamp: new Date().toISOString()
      };

      updateNodeData(id, inputData);
      console.log('✅ Input Node sent Pokemon:', pokemonName);
    }
  </script>

  <div class="input-node">
    <div class="node-header">
      🎮 Pokemon Input
    </div>

    <div class="node-body">
      <input
        type="text"
        placeholder="Pokemon name (e.g., pikachu)"
        bind:value={pokemonName}
        onkeydown={(e) => e.key === 'Enter' && sendData()}
      />

      <button onclick={sendData}>
        Fetch Pokemon →
      </button>
    </div>

    <Handle
      type="source"
      position={Position.Right}
      id="output"
    />
  </div>

  <style>
    .input-node {
      background: white;
      border: 2px solid #ef4444;
      border-radius: 8px;
      min-width: 220px;
      box-shadow: 0 4px 6px rgba(0,0,0,0.1);
    }

    .node-header {
      background: #ef4444;
      color: white;
      padding: 8px 12px;
      font-weight: bold;
      border-radius: 6px 6px 0 0;
    }

    .node-body {
      padding: 12px;
      display: flex;
      flex-direction: column;
      gap: 8px;
    }

    input {
      padding: 8px;
      border: 1px solid #ddd;
      border-radius: 4px;
      font-size: 14px;
    }

    button {
      background: #ef4444;
      color: white;
      border: none;
      padding: 8px;
      border-radius: 4px;
      cursor: pointer;
      font-weight: 500;
    }

    button:hover {
      background: #dc2626;
    }
  </style>
