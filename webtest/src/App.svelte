<script>
 import { auth_token } from "./Auth.js";
 import Login from "./Login.svelte";
</script>

<h1>Delectus Test Client</h1>
<p><em>version 2.0d1</em></p>

<p>auth_token = {$auth_token}</p>

<Login/>
