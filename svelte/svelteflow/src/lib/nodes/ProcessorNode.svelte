<script lang="ts">
    import { Handle, Position } from '@xyflow/svelte';
    import { workflowData, updateNodeData } from '$lib/stores/workflowEngine';

    let { data, id } = $props();

    let processedData = $state(null);
    let isProcessing = $state(false);
    let error = $state(null);

    let inputData = $derived($workflowData[data.sourceNodeId]);

    // AUTOMATIC PROCESSING: Watch for input data changes
    $effect(() => {
      if (inputData && inputData.pokemonName) {
        // Auto-trigger processing when new input arrives
        processPokemon(inputData.pokemonName);
      }
    });

    async function processPokemon(pokemonName: string) {
      isProcessing = true;
      error = null;
      processedData = null;

      try {
        console.log(`🔍 Fetching Pokemon: ${pokemonName}...`);

        const response = await fetch(`https://pokeapi.co/api/v2/pokemon/${pokemonName}`);

        if (!response.ok) {
          throw new Error('Pokemon not found!');
        }

        const pokemon = await response.json();

        processedData = {
          name: pokemon.name,
          id: pokemon.id,
          types: pokemon.types.map((t: any) => t.type.name),
          stats: {
            hp: pokemon.stats[0].base_stat,
            attack: pokemon.stats[1].base_stat,
            defense: pokemon.stats[2].base_stat,
            speed: pokemon.stats[5].base_stat
          },
          sprite: pokemon.sprites.front_default,
          height: pokemon.height,
          weight: pokemon.weight,
          processedAt: new Date().toLocaleTimeString()
        };

        // Send to next node automatically
        updateNodeData(id, processedData);
        console.log('✅ Processor enriched Pokemon data');

      } catch (err) {
        error = err.message;
        console.error('❌ Error fetching Pokemon:', err);
      } finally {
        isProcessing = false;
      }
    }
  </script>

  <div class="processor-node">
    <div class="node-header">
      ⚙️ Pokemon API
    </div>

    <div class="node-body">
      {#if inputData}
        <div class="input-preview">
          📨 Searching: {inputData.pokemonName}
        </div>
      {/if}

      {#if isProcessing}
        <div class="status processing">
          <div class="spinner"></div>
          Fetching data...
        </div>
      {:else if error}
        <div class="status error">
          ❌ {error}
        </div>
      {:else if processedData}
        <div class="result">
          <strong>✅ Found!</strong>
          <div>#{processedData.id} - {processedData.name}</div>
          <div class="types">
            {#each processedData.types as type}
              <span class="type-badge">{type}</span>
            {/each}
          </div>
        </div>
      {:else}
        <div class="status waiting">
          Waiting for input...
        </div>
      {/if}
    </div>

    <Handle type="target" position={Position.Left} id="input" />
    <Handle type="source" position={Position.Right} id="output" />
  </div>

  <style>
    .processor-node {
      background: white;
      border: 2px solid #3b82f6;
      border-radius: 8px;
      min-width: 200px;
      box-shadow: 0 4px 6px rgba(0,0,0,0.1);
    }

    .node-header {
      background: #3b82f6;
      color: white;
      padding: 8px 12px;
      font-weight: bold;
      border-radius: 6px 6px 0 0;
    }

    .node-body {
      padding: 12px;
    }

    .input-preview {
      font-size: 12px;
      color: #3b82f6;
      margin-bottom: 8px;
      padding: 4px 8px;
      background: #dbeafe;
      border-radius: 4px;
    }

    .status {
      text-align: center;
      padding: 12px;
      font-size: 14px;
    }

    .status.processing {
      color: #3b82f6;
    }

    .status.error {
      color: #ef4444;
    }

    .status.waiting {
      color: #999;
      font-style: italic;
    }

    .spinner {
      border: 2px solid #f3f3f3;
      border-top: 2px solid #3b82f6;
      border-radius: 50%;
      width: 20px;
      height: 20px;
      animation: spin 1s linear infinite;
      margin: 0 auto 8px;
    }

    @keyframes spin {
      0% { transform: rotate(0deg); }
      100% { transform: rotate(360deg); }
    }

    .result {
      font-size: 14px;
    }

    .types {
      display: flex;
      gap: 4px;
      margin-top: 8px;
    }

    .type-badge {
      background: #3b82f6;
      color: white;
      padding: 2px 8px;
      border-radius: 12px;
      font-size: 11px;
      text-transform: uppercase;
    }
  </style>
