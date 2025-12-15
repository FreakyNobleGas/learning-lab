<script lang="ts">
  // Props passed from parent
  let { onAddNode, onDeleteSelected, selectedNodes = [] } = $props();

  // Define available node types for each category
  const inputNodeTypes = [
    { id: 'input', label: 'Pokemon Input', icon: '🎮', description: 'Enter Pokemon name' }
  ];

  const processorNodeTypes = [
    { id: 'processor', label: 'API Processor', icon: '⚙️', description: 'Fetch Pokemon data' }
  ];

  const outputNodeTypes = [
    { id: 'output', label: 'Display Output', icon: '📊', description: 'Show Pokemon card' }
  ];

  // Track selected node type for each category
  let selectedInputType = $state(inputNodeTypes[0].id);
  let selectedProcessorType = $state(processorNodeTypes[0].id);
  let selectedOutputType = $state(outputNodeTypes[0].id);
</script>

<div class="toolbar">
  <!-- Input Nodes Section -->
  <div class="toolbar-section input">
    <div class="section-header">
      <span class="icon">📥</span>
      <h3>Input Nodes</h3>
    </div>
    <div class="node-controls">
      <select bind:value={selectedInputType} class="node-select">
        {#each inputNodeTypes as nodeType}
          <option value={nodeType.id}>
            {nodeType.icon} {nodeType.label}
          </option>
        {/each}
      </select>
      <button
        class="add-button input"
        onclick={() => onAddNode(selectedInputType)}
        title="Add input node"
      >
        <span class="icon">+</span>
        Add
      </button>
    </div>
  </div>

  <!-- Processor Nodes Section -->
  <div class="toolbar-section processor">
    <div class="section-header">
      <span class="icon">⚙️</span>
      <h3>Processor Nodes</h3>
    </div>
    <div class="node-controls">
      <select bind:value={selectedProcessorType} class="node-select">
        {#each processorNodeTypes as nodeType}
          <option value={nodeType.id}>
            {nodeType.icon} {nodeType.label}
          </option>
        {/each}
      </select>
      <button
        class="add-button processor"
        onclick={() => onAddNode(selectedProcessorType)}
        title="Add processor node"
      >
        <span class="icon">+</span>
        Add
      </button>
    </div>
  </div>

  <!-- Output Nodes Section -->
  <div class="toolbar-section output">
    <div class="section-header">
      <span class="icon">📤</span>
      <h3>Output Nodes</h3>
    </div>
    <div class="node-controls">
      <select bind:value={selectedOutputType} class="node-select">
        {#each outputNodeTypes as nodeType}
          <option value={nodeType.id}>
            {nodeType.icon} {nodeType.label}
          </option>
        {/each}
      </select>
      <button
        class="add-button output"
        onclick={() => onAddNode(selectedOutputType)}
        title="Add output node"
      >
        <span class="icon">+</span>
        Add
      </button>
    </div>
  </div>

  <!-- Delete Section -->
  <div class="toolbar-section delete">
    <div class="section-header">
      <span class="icon">🗑️</span>
      <h3>Actions</h3>
    </div>
    <button
      class="delete-button"
      onclick={onDeleteSelected}
      disabled={selectedNodes.length === 0}
      title="Delete selected nodes"
    >
      <span class="icon">🗑️</span>
      Delete {selectedNodes.length > 0 ? `(${selectedNodes.length})` : ''}
    </button>
  </div>
</div>

<style>
  .toolbar {
    position: absolute;
    bottom: 0;
    left: 0;
    right: 0;
    background: white;
    border-top: 2px solid #e5e7eb;
    box-shadow: 0 -4px 12px rgba(0, 0, 0, 0.1);
    z-index: 10;
    display: flex;
    gap: 1px;
    background: #e5e7eb;
  }

  .toolbar-section {
    flex: 1;
    background: white;
    padding: 16px 20px;
    display: flex;
    flex-direction: column;
    gap: 12px;
  }

  .toolbar-section.delete {
    flex: 0.5;
    background: #fef2f2;
  }

  .section-header {
    display: flex;
    align-items: center;
    gap: 8px;
  }

  .section-header .icon {
    font-size: 20px;
  }

  h3 {
    margin: 0;
    font-size: 13px;
    font-weight: 600;
    color: #374151;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .node-controls {
    display: flex;
    gap: 8px;
    align-items: center;
  }

  .node-select {
    flex: 1;
    padding: 8px 12px;
    border: 2px solid #d1d5db;
    border-radius: 6px;
    font-size: 14px;
    background: white;
    cursor: pointer;
    transition: all 0.2s;
  }

  .node-select:hover {
    border-color: #9ca3af;
  }

  .node-select:focus {
    outline: none;
    border-color: #3b82f6;
    box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1);
  }

  .add-button {
    display: flex;
    align-items: center;
    gap: 6px;
    padding: 8px 16px;
    border-radius: 6px;
    cursor: pointer;
    transition: all 0.2s;
    font-size: 14px;
    font-weight: 600;
    border: none;
    white-space: nowrap;
  }

  .add-button.input {
    background: #ef4444;
    color: white;
  }

  .add-button.input:hover {
    background: #dc2626;
    transform: translateY(-1px);
  }

  .add-button.processor {
    background: #3b82f6;
    color: white;
  }

  .add-button.processor:hover {
    background: #2563eb;
    transform: translateY(-1px);
  }

  .add-button.output {
    background: #8b5cf6;
    color: white;
  }

  .add-button.output:hover {
    background: #7c3aed;
    transform: translateY(-1px);
  }

  .add-button .icon {
    font-size: 18px;
  }

  .delete-button {
    display: flex;
    align-items: center;
    justify-content: center;
    gap: 8px;
    padding: 10px 16px;
    background: white;
    border: 2px solid #ef4444;
    border-radius: 6px;
    cursor: pointer;
    transition: all 0.2s;
    font-size: 14px;
    font-weight: 600;
    color: #ef4444;
    width: 100%;
  }

  .delete-button:hover:not(:disabled) {
    background: #ef4444;
    color: white;
    transform: translateY(-1px);
  }

  .delete-button:disabled {
    opacity: 0.4;
    cursor: not-allowed;
    border-color: #d1d5db;
    color: #9ca3af;
  }

  .delete-button .icon {
    font-size: 18px;
  }
</style>
