# Svelte

## General
- File name convention starts with + (e.g. `+page.svelte`)
- `$` syntax is a rune which is specific to the Svelte language. There are many types of runes.

## State Variables
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

## 2 Way Binding
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