<script>
 // script
 // -----------------------------------------

 import { authorization } from "./auth.js";

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
 
 function callDelectusAPI (apiName, displayElementID) {
     let uri = "http://mars.local:9000/delectus/"+apiName+"?email="+$authorization["email"];
     let token = $authorization["token"];
     fetch(uri, {method: 'GET',
                headers: {"Authorization": " Token "+token}})
         .then(handleErrorResponse)
         .then(response => response.json())
         .then(data => displayResponseData(data,displayElementID));
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
 
 function deleteCollection () {
     let collectionID = document.getElementById("deleteCollection_collection_id").value;
     let uri = "http://mars.local:9000/delectus/delete_collection";
     let query = "?email="+$authorization["email"]+"&collectionid="+collectionID;
     let token = $authorization["token"];
     fetch(uri+query, {method: 'GET',
                      headers: {"Authorization": " Token "+token}})
         .then((response) => {
             if (!response.ok) { throw Error(response.statusText) }
             return response.json();
         })
         .then(data => document.getElementById("deleteCollection_response").innerHTML=JSON.stringify(data));
 }
 
 function undeleteCollection () {
     let collectionID = document.getElementById("undeleteCollection_collection_id").value;
     let uri = "http://mars.local:9000/delectus/undelete_collection";
     let query = "?email="+$authorization["email"]+"&collectionid="+collectionID;
     let token = $authorization["token"];
     fetch(uri+query, {method: 'GET',
                      headers: {"Authorization": " Token "+token}})
         .then((response) => {
             if (!response.ok) { throw Error(response.statusText) }
             return response.json();
         })
         .then(data => document.getElementById("undeleteCollection_response").innerHTML=JSON.stringify(data));
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
 
 function renameCollection () {
     let collectionID = document.getElementById("renameCollection_collection_id").value;
     let newName = document.getElementById("renameCollection_new_name").value;
     let uri = "http://mars.local:9000/delectus/rename_collection";
     let query = "?email="+$authorization["email"]+"&collectionid="+collectionID+"&newname="+newName;
     let token = $authorization["token"];
     fetch(uri+query, {method: 'GET',
                      headers: {"Authorization": " Token "+token}})
         .then((response) => {
             if (!response.ok) { throw Error(response.statusText) }
             return response.json();
         })
         .then(data => document.getElementById("renameCollection_response").innerHTML=JSON.stringify(data));
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

<h3>Collections</h3>

<table>
    <tr>
        <th>Method</th>
        <th>Endpoint</th>
        <th>Parameters</th>
        <th>Response</th>
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
        <td class="endpoint"><button on:click={deleteCollection}>/delectus/delete_collection</button></td>
        <td><input type="text" id="deleteCollection_collection_id" placeholder="Collection ID"/></td>
        <td id="deleteCollection_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={undeleteCollection}>/delectus/undelete_collection</button></td>
        <td><input type="text" id="undeleteCollection_collection_id" placeholder="Collection ID"/></td>
        <td id="undeleteCollection_response"></td>
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
        <td class="endpoint"><button on:click={renameCollection}>/delectus/rename_collection</button></td>
        <td>
            <input type="text" id="renameCollection_collection_id" placeholder="Collection ID"/>
            <input type="text" id="renameCollection_new_name" placeholder="New name"/>
        </td>
        <td id="renameCollection_response"></td>
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

</table>

