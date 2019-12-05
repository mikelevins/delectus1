<script>
 import { authorization } from "./auth.js";
 import CollectionEndpoints from "./CollectionEndpoints.svelte";
 import ListEndpoints from "./ListEndpoints.svelte";
 import Session from "./Session.svelte";
 import UserEndpoints from "./UserEndpoints.svelte";
 import Login from "./Login.svelte";
</script>

<h1>Delectus Test Client</h1>
<p><em>version 2.0d1</em></p>

{#if $authorization}
    <Session/>
    <UserEndpoints/>
    <CollectionEndpoints/>
    <ListEndpoints/>
{:else}
    <Login/>
{/if}


