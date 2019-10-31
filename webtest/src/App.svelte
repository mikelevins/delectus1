<script>
 let users = [];
 function getUsers() {
     fetch('http://mars.local:9000/delectus/users').then((resp) => resp.json()).then((data) => users=data);
 }
</script>

<h1>Delectus Test Client</h1>
<p><em>version 2.0d1</em></p>

<p><button on:click={getUsers}>delectus/users</button> [{users}]</p>
