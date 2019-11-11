<script>
 // script
 // -----------------------------------------

 import { authorization } from "./auth.js";

 // general utilities
 // -----------------------------------------
 
 function displayResponseData (responseData, displayElementID) {
     document.getElementById(displayElementID).innerHTML=JSON.stringify(responseData)
 }
 
 function callDelectusAPI (apiName, displayElementID) {
     let uri = "http://mars.local:9000/delectus/"+apiName+"?email="+$authorization["email"];
     let token = $authorization["token"];
     fetch(uri, {method: 'GET',
                headers: {"Authorization": " Token "+token}})
         .then(response => response.json())
         .then(data => displayResponseData(data,displayElementID));
 }
 
 // lists
 // -----------------------------------------
 
 function getUserLists () {
     callDelectusAPI("lists","getUserLists_response");
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
 
 function listName () {
     let listID = document.getElementById("listName_list_id").value;
     let uri = "http://mars.local:9000/delectus/list_name";
     let query = "?email="+$authorization["email"]+"&listid="+listID;
     let token = $authorization["token"];
     fetch(uri+query, {method: 'GET',
                      headers: {"Authorization": " Token "+token}})
         .then((response) => {
             if (!response.ok) { throw Error(response.statusText) }
             return response.json();
         })
         .then(data => document.getElementById("listName_response").innerHTML=JSON.stringify(data));
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

 
 function deleteList () {
     let listID = document.getElementById("deleteList_list_id").value;
     let uri = "http://mars.local:9000/delectus/delete_list";
     let query = "?email="+$authorization["email"]+"&listid="+listID;
     let token = $authorization["token"];
     fetch(uri+query, {method: 'GET',
                      headers: {"Authorization": " Token "+token}})
         .then((response) => {
             if (!response.ok) { throw Error(response.statusText) }
             return response.json();
         })
         .then(data => document.getElementById("deleteList_response").innerHTML=JSON.stringify(data));
 }
 
 function undeleteList () {
     let listID = document.getElementById("undeleteList_list_id").value;
     let uri = "http://mars.local:9000/delectus/undelete_list";
     let query = "?email="+$authorization["email"]+"&listid="+listID;
     let token = $authorization["token"];
     fetch(uri+query, {method: 'GET',
                      headers: {"Authorization": " Token "+token}})
         .then((response) => {
             if (!response.ok) { throw Error(response.statusText) }
             return response.json();
         })
         .then(data => document.getElementById("undeleteList_response").innerHTML=JSON.stringify(data));
 }
 
 function listDeleted () {
     let listID = document.getElementById("listDeleted_list_id").value;
     let uri = "http://mars.local:9000/delectus/list_deleted";
     let query = "?email="+$authorization["email"]+"&listid="+listID;
     let token = $authorization["token"];
     fetch(uri+query, {method: 'GET',
                      headers: {"Authorization": " Token "+token}})
         .then((response) => {
             if (!response.ok) { throw Error(response.statusText) }
             return response.json();
         })
         .then(data => document.getElementById("listDeleted_response").innerHTML=JSON.stringify(data));
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
 
 function renameList () {
     let listID = document.getElementById("renameList_list_id").value;
     let newName = document.getElementById("renameList_new_name").value;
     let uri = "http://mars.local:9000/delectus/rename_list";
     let query = "?email="+$authorization["email"]+"&listid="+listID+"&newname="+newName;
     let token = $authorization["token"];
     fetch(uri+query, {method: 'GET',
                      headers: {"Authorization": " Token "+token}})
         .then((response) => {
             if (!response.ok) { throw Error(response.statusText) }
             return response.json();
         })
         .then(data => document.getElementById("renameList_response").innerHTML=JSON.stringify(data));
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
    
    
</table>

