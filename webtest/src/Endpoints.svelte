<script>
 import { authorization } from "./auth.js";

 function getUserID () {
     let uri = "http://mars.local:9000/delectus/userid?email="+$authorization["email"];
     let token = $authorization["token"];
     fetch(uri, {method: 'GET',
                headers: {"Authorization": " Token "+token}})
         .then(response => response.json())
         .then(data => document.getElementById("userid_response").innerHTML=data);
 }
</script>

<table>
    <tr>
        <th>Method</th>
        <th>Endpoint</th>
        <th>Response</th>
    </tr>
    <tr>
        <td>GET</td>
        <td><button on:click={getUserID}>/delectus/userid</button></td><td id="userid_response"></td>
    </tr>
</table>
