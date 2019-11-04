<script>
 import { authorization } from "./auth.js";

 function handleErrorResponse(response) {
     if (response.ok) {
         return response;
     } else {
         throw Error(response.statusText);
     }
 }
 
 function displayResponseData (responseData, displayElementID) {
     document.getElementById(displayElementID).innerHTML=JSON.stringify(responseData)
 }
 
 function callDelectusAPI (apiName, displayElementID) {
     let uri = "http://mars.local:9000/delectus/"+apiName+"?email="+$authorization["email"];
     let token = $authorization["token"];
     fetch(uri, {method: 'GET',
                headers: {"Authorization": " Token "+token}})
         .then(handleErrorResponse)
         .then(response => response.json())
         .then(data => displayResponseData(data,displayElementID));
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
 
 function getCollectionByName () {
     let collectionName = document.getElementById("collection_name").value;
     let uri = "http://mars.local:9000/delectus/collection_named";
     let query = "?email="+$authorization["email"]+"&name="+collectionName;
     let token = $authorization["token"];
     fetch(uri+query, {method: 'GET',
                      headers: {"Authorization": " Token "+token}})
         .then((response) => {
             if (!response.ok) { throw Error(response.statusText) }
             return response.json();
         })
         .then(data => document.getElementById("collection_named_response").innerHTML=JSON.stringify(data));
 }

</script>

<style>
 .endpoint { text-align: right; }
 
 table {
     border-collapse: collapse;
 }

 table, th, td {
     border: 1px solid black;
 }
 
 th, td {
     padding: 8px;
     text-align: left;
 } 
</style>

<table>
    <tr>
        <th>Method</th>
        <th>Endpoint</th>
        <th>Parameters</th>
        <th>Response</th>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={getUserID}>/delectus/userid</button></td>
        <td></td>
        <td id="userid_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={getUserCollections}>/delectus/collections</button></td>
        <td></td>
        <td id="collections_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={getCollectionByName}>/delectus/collection_named</button></td>
        <td><input type="text" id="collection_name" placeholder="Collection name"/></td>
        <td id="collection_named_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={getUserLists}>/delectus/lists</button></td>
        <td></td>
        <td id="lists_response"></td>
    </tr>
</table>
