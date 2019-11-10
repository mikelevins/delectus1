<script>
 // script
 // -----------------------------------------

 import { authorization } from "./auth.js";

 // general utilities
 // -----------------------------------------
 
 function discardAuthorization() {
     authorization.set(null)
 }
 
 function handleErrorResponse(response) {
     if (response.ok) {
         return response;
     } else {
         discardAuthorization();
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

 
 // user data
 // -----------------------------------------

 function getUserID () {
     callDelectusAPI("userid","getUserID_response");
 }

</script>

<!------------ style definitions ------------>

<style>
 .endpoint { text-align: right; }
 
 table {
     border-collapse: collapse;
 }

 table, th, td {
     border: 1px solid black;
 }
 
 th, td {
     padding: 6px;
     text-align: left;
 } 
</style>


<!------------ component markup ------------>


<p>
    Logged in as: <strong>{$authorization.email}</strong>&nbsp;
    <button on:click={discardAuthorization}>discard authorization</button>
</p>

<h3>Users</h3>

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
        <td id="getUserID_response"></td>
    </tr>

</table>

