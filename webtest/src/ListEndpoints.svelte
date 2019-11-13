<script>
 // script
 // -----------------------------------------

 import { authorization } from "./auth.js";
 import { getAPI } from "./api.js";
 
 // lists
 // -----------------------------------------
 
 function getUserLists () {
     let uri = "http://mars.local:9000/delectus/lists?email="+$authorization["email"];
     let token = $authorization["token"];
     getAPI(uri, token, "getUserLists_response");
 }
 
 function getListByID () {
     let listID = document.getElementById("getListByID_list_id").value;
     let uri = "http://mars.local:9000/delectus/list_with_id";
     let query = "?email="+$authorization["email"]+"&id="+listID;
     let token = $authorization["token"];
     getAPI(uri+query, token, "getListByID_response");
 }
 
 function listName () {
     let listID = document.getElementById("listName_list_id").value;
     let uri = "http://mars.local:9000/delectus/list_name";
     let query = "?email="+$authorization["email"]+"&listid="+listID;
     let token = $authorization["token"];
     getAPI(uri+query, token, "listName_response");
 }
 
 function renameList () {
     let listID = document.getElementById("renameList_list_id").value;
     let newName = document.getElementById("renameList_new_name").value;
     let uri = "http://mars.local:9000/delectus/rename_list";
     let query = "?email="+$authorization["email"]+"&listid="+listID+"&newname="+newName;
     let token = $authorization["token"];
     getAPI(uri+query, token, "renameList_response");
 }

 function newList () {
     let listName = document.getElementById("newList_list_name").value;
     let uri = "http://mars.local:9000/delectus/new_list";
     let query = "?email="+$authorization["email"]+"&name="+listName;
     let token = $authorization["token"];
     getAPI(uri+query, token, "newList_response");
 }
 
 function deleteList () {
     let listID = document.getElementById("deleteList_list_id").value;
     let uri = "http://mars.local:9000/delectus/delete_list";
     let query = "?email="+$authorization["email"]+"&listid="+listID;
     let token = $authorization["token"];
     getAPI(uri+query, token, "deleteList_response");
 }
 
 function undeleteList () {
     let listID = document.getElementById("undeleteList_list_id").value;
     let uri = "http://mars.local:9000/delectus/undelete_list";
     let query = "?email="+$authorization["email"]+"&listid="+listID;
     let token = $authorization["token"];
     getAPI(uri+query, token, "undeleteList_response");
 }
 
 function listDeleted () {
     let listID = document.getElementById("listDeleted_list_id").value;
     let uri = "http://mars.local:9000/delectus/list_deleted";
     let query = "?email="+$authorization["email"]+"&listid="+listID;
     let token = $authorization["token"];
     getAPI(uri+query, token, "listDeleted_response");
 }

 function getListByName () {
     let listName = document.getElementById("getListByName_list_name").value;
     let uri = "http://mars.local:9000/delectus/list_named";
     let query = "?email="+$authorization["email"]+"&name="+listName;
     let token = $authorization["token"];
     getAPI(uri+query, token, "getListByName_response");
 }
 
 function listColumns () {
     let listID = document.getElementById("listColumns_list_id").value;
     let uri = "http://mars.local:9000/delectus/list_columns";
     let query = "?email="+$authorization["email"]+"&listid="+listID;
     let token = $authorization["token"];
     getAPI(uri+query, token, "listColumns_response");
 }

 function newColumn () {
     let listID = document.getElementById("newColumn_list_id").value;
     let columnName = document.getElementById("newColumn_column_name").value;
     let uri = "http://mars.local:9000/delectus/new_column";
     let query = "?email="+$authorization["email"]+"&listid="+listID+"&columnname="+columnName;
     let token = $authorization["token"];
     getAPI(uri+query, token, "newList_response");
 }

 function columnWithID () {
     let listID = document.getElementById("columnWithID_list_id").value;
     let columnID = document.getElementById("columnWithID_column_id").value;
     let uri = "http://mars.local:9000/delectus/column_with_id";
     let query = "?email="+$authorization["email"]+"&listid="+listID+"&columnid="+columnID;
     let token = $authorization["token"];
     getAPI(uri+query, token, "columnWithID_response");
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
     table-layout: fixed;
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


<h3>Lists</h3>

<table>
    <tr>
        <th class="method-column">Method</th>
        <th class="endpoint-column">Endpoint</th>
        <th class="parameters-column">Parameters</th>
        <th class="response-column">Response</th>
    </tr>


    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={getUserLists}>/delectus/lists</button></td>
        <td></td>
        <td id="getUserLists_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={getListByID}>/delectus/list_with_id</button></td>
        <td><input type="text" id="getListByID_list_id" placeholder="List ID"/></td>
        <td id="getListByID_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={listName}>/delectus/list_name</button></td>
        <td><input type="text" id="listName_list_id" placeholder="List ID"/></td>
        <td id="listName_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={getListByName}>/delectus/list_named</button></td>
        <td><input type="text" id="getListByName_list_name" placeholder="List name"/></td>
        <td id="getListByName_response"></td>
    </tr>
    
    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={renameList}>/delectus/rename_list</button></td>
        <td>
            <input type="text" id="renameList_list_id" placeholder="List ID"/>
            <input type="text" id="renameList_new_name" placeholder="New name"/>
        </td>
        <td id="renameList_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={newList}>/delectus/new_list</button></td>
        <td><input type="text" id="newList_list_name" placeholder="List name"/></td>
        <td id="newList_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={deleteList}>/delectus/delete_list</button></td>
        <td><input type="text" id="deleteList_list_id" placeholder="List ID"/></td>
        <td id="deleteList_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={undeleteList}>/delectus/undelete_list</button></td>
        <td><input type="text" id="undeleteList_list_id" placeholder="List ID"/></td>
        <td id="undeleteList_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={listDeleted}>/delectus/list_deleted</button></td>
        <td><input type="text" id="listDeleted_list_id" placeholder="List ID"/></td>
        <td id="listDeleted_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={listColumns}>/delectus/list_columns</button></td>
        <td><input type="text" id="listColumns_list_id" placeholder="List ID"/></td>
        <td id="listColumns_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={newColumn}>/delectus/new_column</button></td>
        <td>
            <input type="text" id="newColumn_list_id" placeholder="List ID"/>
            <input type="text" id="newColumn_column_name" placeholder="Column name"/>
        </td>
        <td id="newColumn_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={columnWithID}>/delectus/column_with_id</button></td>
        <td>
            <input type="text" id="columnWithID_list_id" placeholder="List ID"/>
            <input type="text" id="columnWithID_column_id" placeholder="Column ID"/>
        </td>
        <td id="columnWithID_response"></td>
    </tr>
    
    
</table>

