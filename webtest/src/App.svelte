<script>
 import { auth_token } from "./auth.js";
 import Login from "./Login.svelte";
</script>

<h1>Delectus Test Client</h1>
<p><em>version 2.0d1</em></p>

{#if $auth_token}
    <p>Logged in</p>
{:else}
    <p>Not logged in</p>
{/if}

<Login/>

