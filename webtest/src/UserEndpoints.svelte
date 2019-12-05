<script>
 // script
 // -----------------------------------------

 import { authorization, discardAuthorization } from "./auth.js";
 import { apiEndpoint, encodedValue, getAPI, makeQuery } from "./api.js";

 // general utilities
 // -----------------------------------------
 
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
 
 // user data
 // -----------------------------------------

 function getUserID () {
     let uri = apiEndpoint("userid");
     let pMap = {"email": $authorization["email"]};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "getUserID_response");
 }

 function getUser () {
     let uri = apiEndpoint("user");
     let pMap = {"id": encodedValue(document, "getUser_user_id")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "getUser_response");
 }
 

</script>

<!------------ style definitions ------------>

<style>
 .authdata {
     font-family: monospace;
 }

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

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={getUser}>/delectus/user</button></td>
        <td><input type="text" id="getUser_user_id" placeholder="User ID"/></td>
        <td id="getUser_response"></td>
    </tr>

</table>

