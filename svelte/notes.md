# Svelte

## General
- File name convention starts with + (e.g. `+page.svelte`)
- `$` syntax is a rune which is specific to the Svelte language. There are many types of runes.

## State 

### Variables
- State variables rendered will automatically be updated and re-rendered
- `let name = $state('Scott');`

```html
<script lang="ts">
    let count = $state(0);

    function increment() {
        count *= 1;
    }
</script>

<button onclick={increment}>
    clicks: {count}
</button>
```

### Derived
- State Derived variables allow you to create a state that depends on another state.

```typescript
<script lang="ts">
    let first_name = $state("Alexander");
    // full_name depends on name and will automatically update if name is updated.
    let full_name = $derived(name + ' The Great')
</script>
```

## Binding

### 2 Way Binding
- Allows you to update a variable from an input field

```html
<script lang="ts">
    let name = $state("scott");
</script>

<input type="text" bind:value={name}>
```

## Props
- Allow you to pass variables from somewhere into a component

```html
<!-- +page.svelte-->
<script>
    import Header from './Header.svelte';
    let name = 'Scott`;
</script>

<!-- Notice how you don't need to say name={name} -->
<Header {name} />

<!-- header.svelte -->

<script lang="ts">
    // Name is passed in and can be used throughtout this file.
    let { name } = $props();    
</script>

<h1>Hello {name}</h1>
<h2>{name.replaceAll('t', 'x')}</h2>
```

## Template Tags / Conditionals
```html
<script lang="ts">
    let formState = $state({
        name: '',
        birthday: '',
        step: 0,
        error: ''
    });
</script>

{#if formState.step === 0}
    <div>
        <label for="name">Your Name</label>
        <input type="text" id="name" bind:value={formState.name}>
    </div>
    <!-- Inline function for onclick -->
    <button 
        onclick={() => {
            if(formState.name !== "") {
                formState.step += 1;
                formState.error = '';
            } else {
                formState.error = "Your name is empty. Please write your name."
            }
        }}>Next</button>

{:else if formState.step === 1} <!-- Empty Block -->
...
{:else} <!-- Empty Block -->
...
{/if}
```

## CSS
- CSS scoped to the component by default. (e.g. style's don't leak)

```typescript
<script lang="ts">
    let name = "Scott";
</script>

<div>
    <h1>{name}'s Form</h1>
</div>

<style>
    div {
        background: blue;
    }
</style>
```

Global CSS

```typescript
<style>
    :global(div) {
        background: blue;
    }
</style>
```

## app.d.ts
- A TypeScript declaration file used to define global types and interfaces.
- Used to declare types for global variables, modules, or interfaces that are used throughout the application.
- Provides type definitions for Svelte-specific features

NOTE: This file is only intended for types. Enums, Objects, and etc. are allowed, but cannot be used with runtime objects since TypeScript only compiles this file as types.

```typescript
declare namespace App {
    interface Locals {
        user: { id: string; name: string };
    }
    interface PageData {
        title: string;
    }
    interface Platform {}
}
```

## app.html
- Main HTML template file used to define the structure of the HTML that wraps your app.
