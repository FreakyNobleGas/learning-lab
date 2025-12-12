<script lang="ts">
    import { Handle, Position } from '@xyflow/svelte';
    import { workflowData } from '$lib/stores/workflowEngine';

    let { data, id } = $props();

    let displayData = $derived($workflowData[data.sourceNodeId]);
  </script>

  <div class="output-node">
    <div class="node-header">
      📊 Pokemon Display
    </div>

    <div class="node-body">
      {#if displayData}
        <div class="pokemon-card">
          {#if displayData.sprite}
            <img src={displayData.sprite} alt={displayData.name} class="sprite" />
          {/if}

          <div class="pokemon-name">
            #{displayData.id} {displayData.name.toUpperCase()}
          </div>

          <div class="types">
            {#each displayData.types as type}
              <span class="type-badge {type}">{type}</span>
            {/each}
          </div>

          <div class="stats">
            <div class="stat">
              <span class="label">HP</span>
              <span class="value">{displayData.stats.hp}</span>
            </div>
            <div class="stat">
              <span class="label">Attack</span>
              <span class="value">{displayData.stats.attack}</span>
            </div>
            <div class="stat">
              <span class="label">Defense</span>
              <span class="value">{displayData.stats.defense}</span>
            </div>
            <div class="stat">
              <span class="label">Speed</span>
              <span class="value">{displayData.stats.speed}</span>
            </div>
          </div>

          <div class="metadata">
            Height: {displayData.height/10}m | Weight: {displayData.weight/10}kg
          </div>

          <div class="timestamp">
            Fetched: {displayData.processedAt}
          </div>
        </div>
      {:else}
        <div class="waiting">
          Waiting for Pokemon data...
        </div>
      {/if}
    </div>

    <Handle
      type="target"
      position={Position.Left}
      id="input"
    />
  </div>

  <style>
    .output-node {
      background: white;
      border: 2px solid #8b5cf6;
      border-radius: 8px;
      min-width: 280px;
      box-shadow: 0 4px 6px rgba(0,0,0,0.1);
    }

    .node-header {
      background: #8b5cf6;
      color: white;
      padding: 8px 12px;
      font-weight: bold;
      border-radius: 6px 6px 0 0;
    }

    .node-body {
      padding: 12px;
    }

    .pokemon-card {
      text-align: center;
    }

    .sprite {
      width: 96px;
      height: 96px;
      image-rendering: pixelated;
      margin: 0 auto;
    }

    .pokemon-name {
      font-size: 18px;
      font-weight: bold;
      color: #333;
      margin: 8px 0;
    }

    .types {
      display: flex;
      gap: 6px;
      justify-content: center;
      margin: 12px 0;
    }

    .type-badge {
      padding: 4px 12px;
      border-radius: 16px;
      font-size: 12px;
      font-weight: 600;
      text-transform: uppercase;
      color: white;
    }

    .type-badge.fire { background: #f08030; }
    .type-badge.water { background: #6890f0; }
    .type-badge.grass { background: #78c850; }
    .type-badge.electric { background: #f8d030; color: #333; }
    .type-badge.psychic { background: #f85888; }
    .type-badge.ice { background: #98d8d8; }
    .type-badge.dragon { background: #7038f8; }
    .type-badge.dark { background: #705848; }
    .type-badge.fairy { background: #ee99ac; }
    .type-badge.normal { background: #a8a878; }
    .type-badge.fighting { background: #c03028; }
    .type-badge.flying { background: #a890f0; }
    .type-badge.poison { background: #a040a0; }
    .type-badge.ground { background: #e0c068; }
    .type-badge.rock { background: #b8a038; }
    .type-badge.bug { background: #a8b820; }
    .type-badge.ghost { background: #705898; }
    .type-badge.steel { background: #b8b8d0; }

    .stats {
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 8px;
      margin: 12px 0;
    }

    .stat {
      background: #f3f4f6;
      padding: 8px;
      border-radius: 6px;
      display: flex;
      justify-content: space-between;
      align-items: center;
    }

    .stat .label {
      font-size: 12px;
      color: #666;
      font-weight: 500;
    }

    .stat .value {
      font-size: 16px;
      font-weight: bold;
      color: #8b5cf6;
    }

    .metadata {
      font-size: 12px;
      color: #666;
      margin-top: 8px;
    }

    .timestamp {
      font-size: 11px;
      color: #999;
      margin-top: 12px;
      padding-top: 8px;
      border-top: 1px solid #eee;
    }

    .waiting {
      color: #999;
      font-style: italic;
      text-align: center;
      padding: 32px 0;
    }
  </style>
