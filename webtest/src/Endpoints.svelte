<script>
 import { authorization } from "./auth.js";

 function callDelectusAPI (apiName, displayElementID) {
     let uri = "http://mars.local:9000/delectus/"+apiName+"?email="+$authorization["email"];
     let token = $authorization["token"];
     fetch(uri, {method: 'GET',
                headers: {"Authorization": " Token "+token}})
         .then((response) => {
             if (!response.ok) { throw Error(response.statusText) }
             return response.json();
         })
         .then(data => document.getElementById(displayElementID).innerHTML=JSON.stringify(data));
 }
 
 function getUserID () {
     callDelectusAPI("userid","userid_response");
 }
 
 function getUserCollections () {
     callDelectusAPI("collections","collections_response");
 }
 
 function getUserLists () {
     callDelectusAPI("lists","lists_response");
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
        <td><button on:click={getUserID}>/delectus/userid</button></td>
        <td id="userid_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td><button on:click={getUserCollections}>/delectus/collections</button></td>
        <td id="collections_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td><button on:click={getUserLists}>/delectus/lists</button></td>
        <td id="lists_response"></td>
    </tr>
</table>
