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
 
 // collections
 // -----------------------------------------
 
 function getUserCollections () {
     callDelectusAPI("collections","getUserCollections_response");
 }
 
 function newCollection () {
     let collectionName = document.getElementById("newCollection_collection_name").value;
     let uri = "http://mars.local:9000/delectus/new_collection";
     let query = "?email="+$authorization["email"]+"&name="+collectionName;
     let token = $authorization["token"];
     fetch(uri+query, {method: 'GET',
                      headers: {"Authorization": " Token "+token}})
         .then((response) => {
             if (!response.ok) { throw Error(response.statusText) }
             return response.json();
         })
         .then(data => document.getElementById("newCollection_response").innerHTML=JSON.stringify(data));
 }
 
 function getCollectionByName () {
     let collectionName = document.getElementById("getCollectionByName_collection_name").value;
     let uri = "http://mars.local:9000/delectus/collection_named";
     let query = "?email="+$authorization["email"]+"&name="+collectionName;
     let token = $authorization["token"];
     fetch(uri+query, {method: 'GET',
                      headers: {"Authorization": " Token "+token}})
         .then((response) => {
             if (!response.ok) { throw Error(response.statusText) }
             return response.json();
         })
         .then(data => document.getElementById("getCollectionByName_response").innerHTML=JSON.stringify(data));
 }
 
 function getCollectionByID () {
     let collectionID = document.getElementById("getCollectionByID_collection_id").value;
     let uri = "http://mars.local:9000/delectus/collection_with_id";
     let query = "?email="+$authorization["email"]+"&id="+collectionID;
     let token = $authorization["token"];
     fetch(uri+query, {method: 'GET',
                      headers: {"Authorization": " Token "+token}})
         .then((response) => {
             if (!response.ok) { throw Error(response.statusText) }
             return response.json();
         })
         .then(data => document.getElementById("getCollectionByID_response").innerHTML=JSON.stringify(data));
 }
 
 function collectionAddList () {
     let collectionID = document.getElementById("collectionAddList_collection_id").value;
     let listID = document.getElementById("collectionAddList_list_id").value;
     let uri = "http://mars.local:9000/delectus/collection_add_list";
     let query = "?email="+$authorization["email"]+"&collectionid="+collectionID+"&listid="+listID;
     let token = $authorization["token"];
     fetch(uri+query, {method: 'GET',
                      headers: {"Authorization": " Token "+token}})
         .then((response) => {
             if (!response.ok) { throw Error(response.statusText) }
             return response.json();
         })
         .then(data => document.getElementById("collectionAddList_response").innerHTML=JSON.stringify(data));
 }

 function collectionRemoveList () {
     let collectionID = document.getElementById("collectionRemoveList_collection_id").value;
     let listID = document.getElementById("collectionRemoveList_list_id").value;
     let uri = "http://mars.local:9000/delectus/collection_remove_list";
     let query = "?email="+$authorization["email"]+"&collectionid="+collectionID+"&listid="+listID;
     let token = $authorization["token"];
     fetch(uri+query, {method: 'GET',
                      headers: {"Authorization": " Token "+token}})
         .then((response) => {
             if (!response.ok) { throw Error(response.statusText) }
             return response.json();
         })
         .then(data => document.getElementById("collectionRemoveList_response").innerHTML=JSON.stringify(data));
 }

 
 // lists
 // -----------------------------------------
 
 function getUserLists () {
     callDelectusAPI("lists","getUserLists_response");
 }

 
 function newList () {
     let listName = document.getElementById("newList_list_name").value;
     let uri = "http://mars.local:9000/delectus/new_list";
     let query = "?email="+$authorization["email"]+"&name="+listName;
     let token = $authorization["token"];
     fetch(uri+query, {method: 'GET',
                      headers: {"Authorization": " Token "+token}})
         .then((response) => {
             if (!response.ok) { throw Error(response.statusText) }
             return response.json();
         })
         .then(data => document.getElementById("newList_response").innerHTML=JSON.stringify(data));
 }

 function getListByName () {
     let listName = document.getElementById("getListByName_list_name").value;
     let uri = "http://mars.local:9000/delectus/list_named";
     let query = "?email="+$authorization["email"]+"&name="+listName;
     let token = $authorization["token"];
     fetch(uri+query, {method: 'GET',
                      headers: {"Authorization": " Token "+token}})
         .then((response) => {
             if (!response.ok) { throw Error(response.statusText) }
             return response.json();
         })
         .then(data => document.getElementById("getListByName_response").innerHTML=JSON.stringify(data));
 }
 
 function getListByID () {
     let listID = document.getElementById("getListByID_list_id").value;
     let uri = "http://mars.local:9000/delectus/list_with_id";
     let query = "?email="+$authorization["email"]+"&id="+listID;
     let token = $authorization["token"];
     fetch(uri+query, {method: 'GET',
                      headers: {"Authorization": " Token "+token}})
         .then((response) => {
             if (!response.ok) { throw Error(response.statusText) }
             return response.json();
         })
         .then(data => document.getElementById("getListByID_response").innerHTML=JSON.stringify(data));
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
     padding: 8px;
     text-align: left;
 } 
</style>


<!------------ component markup ------------>

<p>
    Logged in as: <strong>{$authorization.email}</strong>&nbsp;
    <button on:click={discardAuthorization}>discard authorization</button>
</p>

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
        <td class="endpoint"><button on:click={getUserCollections}>/delectus/collections</button></td>
        <td></td>
        <td id="getUserCollections_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={newCollection}>/delectus/new_collection</button></td>
        <td><input type="text" id="newCollection_collection_name" placeholder="Collection name"/></td>
        <td id="newCollection_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={getCollectionByName}>/delectus/collection_named</button></td>
        <td><input type="text" id="getCollectionByName_collection_name" placeholder="Collection name"/></td>
        <td id="getCollectionByName_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={getCollectionByID}>/delectus/collection_with_id</button></td>
        <td><input type="text" id="getCollectionByID_collection_id" placeholder="Collection ID"/></td>
        <td id="getCollectionByID_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={collectionAddList}>/delectus/collection_add_list</button></td>
        <td>
            <input type="text" id="collectionAddList_collection_id" placeholder="Collection ID"/>
            <input type="text" id="collectionAddList_list_id" placeholder="List ID"/>
        </td>
        <td id="collectionAddList_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={collectionRemoveList}>/delectus/collection_remove_list</button></td>
        <td>
            <input type="text" id="collectionRemoveList_collection_id" placeholder="Collection ID"/>
            <input type="text" id="collectionRemoveList_list_id" placeholder="List ID"/>
        </td>
        <td id="collectionRemoveList_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={getUserLists}>/delectus/lists</button></td>
        <td></td>
        <td id="getUserLists_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={newList}>/delectus/new_list</button></td>
        <td><input type="text" id="newList_list_name" placeholder="List name"/></td>
        <td id="newList_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={getListByName}>/delectus/list_named</button></td>
        <td><input type="text" id="getListByName_list_name" placeholder="List name"/></td>
        <td id="getListByName_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={getListByID}>/delectus/list_with_id</button></td>
        <td><input type="text" id="getListByID_list_id" placeholder="List ID"/></td>
        <td id="getListByID_response"></td>
    </tr>
</table>


