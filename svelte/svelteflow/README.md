# Pokemon Workflow Builder

A learning project built with **Svelte 5** and **Svelte Flow** to understand node-based workflow systems. This demo fetches Pokemon data through a visual pipeline of connected nodes.

## Project Overview

This application demonstrates how to build interactive, node-based workflows where data flows automatically through a pipeline. The example uses Pokemon data from the PokeAPI, but the concepts apply to any workflow system (CRM integrations, data processing pipelines, automation tools, etc.).

**Live Demo Flow:**
```
[Pokemon Input] → [API Processor] → [Display Output]
     Type name      Fetch from API     Show Pokemon card
```

## Core Concepts Learned

### 1. Svelte Flow Fundamentals

**What is Svelte Flow?**
- A customizable library for building node-based editors and interactive diagrams
- Nodes are draggable, connectable components that represent steps in a workflow
- Edges are the visual connections between nodes showing data flow
- The viewport handles zooming, panning, and canvas management

**Key Components:**
- `<SvelteFlow>` - Main canvas component
- `<Controls>` - Zoom in/out buttons
- `<Background>` - Grid or dot pattern background
- `<MiniMap>` - Bird's eye view of the canvas
- `<Handle>` - Connection points on nodes

### 2. Svelte 5 Runes (State Management)

**Critical for Performance:**
```svelte
// Use $state.raw() for nodes and edges (better performance)
let nodes = $state.raw([...]);
let edges = $state.raw([...]);

// Use $state() for regular reactive state
let pokemonName = $state('');

// Use $derived() for computed values
let inputData = $derived($workflowData[sourceNodeId]);

// Use $effect() for side effects and auto-triggering
$effect(() => {
  if (inputData) {
    processPokemon(inputData);
  }
});
```

### 3. Custom Node Types

**Three Node Patterns:**

**Input Node** (Data Source)
- No input handles, only output handle
- Collects user input (forms, file uploads, etc.)
- Sends data to connected nodes
- Example: Pokemon name input

**Processor Node** (Data Transformation)
- Both input and output handles
- Receives data, transforms it, sends it forward
- Can call APIs, validate, enrich, or modify data
- Example: Fetch Pokemon data from API

**Output Node** (Data Destination)
- Only input handle, no outputs
- Final destination for data
- Displays results or sends to external systems
- Example: Pokemon card display

### 4. Handle Types and Positions

```svelte
<!-- Output handle (data flows OUT) -->
<Handle
  type="source"
  position={Position.Right}
  id="output"
/>

<!-- Input handle (data flows IN) -->
<Handle
  type="target"
  position={Position.Left}
  id="input"
/>
```

**Rules:**
- `type="source"` = output connection point
- `type="target"` = input connection point
- `position` = Top, Right, Bottom, or Left
- `id` = unique identifier for the handle

### 5. Automatic Data Flow

**Workflow Engine Pattern:**

The workflow uses a centralized store to manage data flow:

```typescript
// workflowEngine.ts
export const workflowData = writable<WorkflowData>({});

// When a node outputs data
updateNodeData(nodeId, data);

// When a node needs input data
let inputData = $derived($workflowData[sourceNodeId]);
```

**Automatic Processing:**
```svelte
// Watches for input changes and auto-processes
$effect(() => {
  if (inputData && inputData.pokemonName) {
    processPokemon(inputData.pokemonName);
  }
});
```

### 6. Real API Integration

**Fetching External Data:**
```typescript
async function processPokemon(pokemonName: string) {
  const response = await fetch(`https://pokeapi.co/api/v2/pokemon/${pokemonName}`);
  const pokemon = await response.json();

  // Transform and send to next node
  updateNodeData(id, processedData);
}
```

**Key Patterns:**
- Use async/await for API calls
- Handle loading states (`isProcessing`)
- Handle errors gracefully
- Display feedback to users

### 7. Edge Event Handling

**Connecting Nodes:**
```svelte
<SvelteFlow
  onconnect={handleConnect}
  ondisconnect={handleDisconnect}
>
```

**Track Connections:**
```typescript
function handleConnect(event) {
  const { source, target } = event.detail.connection;

  // Update target node to know its data source
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
```

## Project Structure

```
src/
├── lib/
│   ├── nodes/
│   │   ├── InputNode.svelte      # Pokemon name input
│   │   ├── ProcessorNode.svelte  # API data fetcher
│   │   └── OutputNode.svelte     # Pokemon card display
│   └── stores/
│       └── workflowEngine.ts     # Centralized data flow manager
└── routes/
    └── +page.svelte              # Main canvas and node registration
```

## How Data Flows

1. **User Input** → User types "pikachu" in Input Node
2. **Trigger** → Clicks "Fetch Pokemon" button
3. **Update Store** → `updateNodeData('1', { pokemonName: 'pikachu' })`
4. **Auto-Detect** → Processor Node's `$effect()` sees new data
5. **Process** → Fetches from PokeAPI automatically
6. **Update Store** → `updateNodeData('2', pokemonData)`
7. **Auto-Display** → Output Node's `$derived()` updates view

**Key Insight:** The `$effect()` and `$derived()` runes make data flow automatic!

## Hackathon-Ready Concepts

### For Your Next Project

**Multi-Node Types:**
```typescript
const nodeTypes = {
  // Inputs
  pokemonInput: PokemonInputNode,
  textInput: TextInputNode,
  fileUpload: FileUploadNode,

  // Processors
  apiCall: APINode,
  dataFilter: FilterNode,
  transformer: TransformNode,
  validator: ValidatorNode,

  // Outputs
  display: DisplayNode,
  webhook: WebhookNode,
  database: DatabaseNode,
  email: EmailNode
};
```

**Connection Validation:**
```typescript
function isValidConnection(connection) {
  const sourceNode = nodes.find(n => n.id === connection.source);
  const targetNode = nodes.find(n => n.id === connection.target);

  // Business rules
  if (sourceNode.type === 'input' && targetNode.type === 'output') {
    return false; // Can't skip processing
  }

  return true;
}
```

**Dynamic Node Creation:**
```typescript
function addNode(type: string, position: { x: number, y: number }) {
  const newNode = {
    id: `node-${Date.now()}`,
    type,
    data: {},
    position
  };
  nodes = [...nodes, newNode];
}
```

**Workflow Execution:**
```typescript
async function executeWorkflow(startNodeId: string) {
  // 1. Get data from input nodes
  // 2. Find connected processor nodes
  // 3. Execute in topological order
  // 4. Handle errors and retries
  // 5. Display results in output nodes
}
```

## Advanced Features (Coming Next)

- [ ] **Dynamic Toolbar** - Drag & drop nodes onto canvas
- [ ] **Save/Load Workflows** - Persist workflow configurations
- [ ] **Multiple Inputs** - Merge data from multiple sources
- [ ] **Conditional Branching** - Different paths based on data
- [ ] **Loop Nodes** - Process arrays of items
- [ ] **Error Handling Nodes** - Catch and handle failures
- [ ] **Real-time Collaboration** - Multiple users editing same workflow

## Development

**Install dependencies:**
```bash
npm install
```

**Start dev server:**
```bash
npm run dev
```

**Build for production:**
```bash
npm run build
```

## Key Dependencies

- **Svelte 5** - Reactive UI framework with runes
- **SvelteKit** - Full-stack framework for Svelte
- **@xyflow/svelte** - Node-based workflow library
- **PokeAPI** - Free Pokemon data API (no auth required)

## Tips for Building Similar Apps

### 1. Start Simple
- Build one flow end-to-end first
- Add complexity incrementally
- Test data flow at each step

### 2. Use Console Logs
- Log when data enters nodes: `console.log('📥 Received:', data)`
- Log when data exits nodes: `console.log('✅ Sent:', data)`
- Track edge connections: `console.log('🔗 Connected:', source, target)`

### 3. Handle Edge Cases
- What if API fails?
- What if node is disconnected?
- What if user enters invalid data?
- What if multiple inputs connect?

### 4. Visual Feedback
- Show loading states during processing
- Display errors clearly
- Highlight active data flow
- Use colors to indicate node types

### 5. Performance
- Use `$state.raw()` for large arrays
- Debounce API calls
- Cache API responses
- Limit number of nodes on canvas

## Common Patterns

### Sharing Data Between Nodes
```typescript
// In workflow engine store
export const workflowData = writable({});

// Node A: Send data
updateNodeData(nodeId, myData);

// Node B: Receive data
let data = $derived($workflowData[sourceNodeId]);
```

### Auto-Processing on Data Change
```svelte
$effect(() => {
  if (inputData) {
    processData(inputData);
  }
});
```

### Handling Multiple Inputs
```typescript
function getNodeInputData(nodeId: string, edges: Edge[]) {
  const incomingEdges = edges.filter(e => e.target === nodeId);
  return incomingEdges.map(edge => ({
    sourceId: edge.source,
    data: $workflowData[edge.source]
  }));
}
```

## Resources

- [Svelte Flow Docs](https://svelteflow.dev/)
- [Svelte 5 Docs](https://svelte.dev/docs/svelte/overview)
- [PokeAPI Docs](https://pokeapi.co/docs/v2)
- [SvelteKit Docs](https://kit.svelte.dev/)

## Example Use Cases

This pattern works for:
- **Data pipelines** - ETL, data transformation
- **Automation workflows** - Zapier-style automation
- **Business process modeling** - BPMN diagrams
- **Visual programming** - Low-code/no-code tools
- **AI agent chains** - LangChain-style workflows
- **Game logic** - Quest systems, skill trees
- **Music production** - Audio effect chains

## License

MIT - Built for learning purposes

---

**Ready for your hackathon?** You now understand the core concepts of building node-based workflow systems!
