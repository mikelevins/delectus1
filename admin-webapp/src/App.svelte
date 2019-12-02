<script>
 import { authorization } from "./auth.js";
 import Login from "./Login.svelte";
</script>

<h1>Delectus 2.0 Admin</h1>
<p><em>version 2.0d1</em></p>

{#if $authorization}
    <p>App goes here</p>
{:else}
    <Login/>
{/if}


