<script>
 import { authorization } from "./auth.js";
 import Endpoints from "./Endpoints.svelte";
 import Login from "./Login.svelte";
</script>

<h1>Delectus Test Client</h1>
<p><em>version 2.0d1</em></p>

{#if $authorization}
    <p>Logged in as: <strong>{$authorization.email}</strong></p>
    <Endpoints/>
{:else}
    <Login/>
{/if}


