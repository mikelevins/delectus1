<script>
 // script
 // -----------------------------------------

 import { authorization } from "./auth.js";
 import { apiEndpoint, encodedValue, getAPI, makeQuery } from "./api.js";
 
 // collections
 // -----------------------------------------
 
 function getUserCollections () {
     let uri = apiEndpoint("collections");
     let pMap = {"email": $authorization["email"]};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "getUserCollections_response");
 }
 
 function newCollection () {
     let uri = apiEndpoint("new_collection");
     let pMap = {"email": $authorization["email"], 
                "name": encodedValue(document, "newCollection_collection_name")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "newCollection_response");
 }
 
 function deleteCollection () {
     let uri = apiEndpoint("delete_collection");
     let pMap = {"email": $authorization["email"], 
                "collectionid": encodedValue(document, "deleteCollection_collection_id")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "deleteCollection_response");
 }
 
 function undeleteCollection () {
     let uri = apiEndpoint("undelete_collection");
     let pMap = {"email": $authorization["email"], 
                "collectionid": encodedValue(document, "undeleteCollection_collection_id")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "undeleteCollection_response");
 }
 
 function collectionDeleted () {
     let uri = apiEndpoint("collection_deleted");
     let pMap = {"email": $authorization["email"], 
                "collectionid": encodedValue(document, "collectionDeleted_collection_id")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "collectionDeleted_response");
 }

 function collectionName () {
     let uri = apiEndpoint("collection_name");
     let pMap = {"email": $authorization["email"], 
                "id": encodedValue(document, "collectionName_collection_id")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "collectionName_response");
 }

 function getCollectionByName () {
     let uri = apiEndpoint("collection_named");
     let pMap = {"email": $authorization["email"], 
                "name": encodedValue(document, "getCollectionByName_collection_name")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "getCollectionByName_response");
 }
 
 function getCollectionByID () {
     let uri = apiEndpoint("collection_with_id");
     let pMap = {"email": $authorization["email"], 
                "id": encodedValue(document, "getCollectionByID_collection_id")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "getCollectionByID_response");
 }
 
 function renameCollection () {
     let uri = apiEndpoint("rename_collection");
     let pMap = {"email": $authorization["email"], 
                "collectionid": encodedValue(document, "renameCollection_collection_id"),
                "newname": encodedValue(document, "renameCollection_new_name")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "renameCollection_response");
 }
 
 function collectionLists () {
     let uri = apiEndpoint("collection_lists");
     let pMap = {"email": $authorization["email"], 
                "id": encodedValue(document, "collectionLists_collection_id")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "collectionLists_response");
 }

 function collectionAddList () {
     let uri = apiEndpoint("collection_add_list");
     let pMap = {"email": $authorization["email"], 
                "collectionid": encodedValue(document, "collectionAddList_collection_id"),
                "listid": encodedValue(document, "collectionAddList_list_id")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "collectionAddList_response");
 }

 function collectionRemoveList () {
     let uri = apiEndpoint("collection_remove_list");
     let pMap = {"email": $authorization["email"], 
                "collectionid": encodedValue(document, "collectionRemoveList_collection_id"),
                "listid": encodedValue(document, "collectionRemoveList_list_id")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "collectionRemoveList_response");
 }

</script>

<!------------ style definitions ------------>

<style>
 .endpoint { text-align: right; }

 .method-column { width: 5%; }
 .parameters-column { width: 25%; }
 .response-column { width: 40%; }

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

<h3>Collections</h3>

<table>
    <tr>
        <th class="method-column">Method</th>
        <th class="endpoint-column">Endpoint</th>
        <th class="parameters-column">Parameters</th>
        <th class="response-column">Response</th>
    </tr>
    
    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={getUserCollections}>/delectus/collections</button></td>
        <td></td>
        <td id="getUserCollections_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={getCollectionByID}>/delectus/collection_with_id</button></td>
        <td><input type="text" id="getCollectionByID_collection_id" placeholder="Collection ID"/></td>
        <td id="getCollectionByID_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={collectionName}>/delectus/collection_name</button></td>
        <td><input type="text" id="collectionName_collection_id" placeholder="Collection ID"/></td>
        <td id="collectionName_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={getCollectionByName}>/delectus/collection_named</button></td>
        <td><input type="text" id="getCollectionByName_collection_name" placeholder="Collection name"/></td>
        <td id="getCollectionByName_response"></td>
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
        <td class="endpoint"><button on:click={collectionDeleted}>/delectus/collection_deleted</button></td>
        <td><input type="text" id="collectionDeleted_collection_id" placeholder="Collection ID"/></td>
        <td id="collectionDeleted_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={collectionLists}>/delectus/collection_lists</button></td>
        <td><input type="text" id="collectionLists_collection_id" placeholder="Collection ID"/></td>
        <td id="collectionLists_response"></td>
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

