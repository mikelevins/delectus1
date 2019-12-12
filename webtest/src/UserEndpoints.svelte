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
     document.getElementById(displayElementID).innerHTML=JSON.stringify(responseData, undefined, 2)
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

 .method-column { width: 5%; }
 .endpoint-column { width: 23%; }
 .parameters-column { width: 12%; }
 
 input { font-size: 1rem; }

 pre { font-size: 1rem; }

 table {
     border: 1px solid black;
     border-collapse: collapse;
     table-layout: fixed;
 }
 
 th, td {
     border: 1px solid black;
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
        <td><pre id="getUserID_response"></pre></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={getUser}>/delectus/user</button></td>
        <td><input size="36" type="text" id="getUser_user_id" placeholder="User ID"/></td>
        <td><pre id="getUser_response"></pre></td>
    </tr>

</table>

