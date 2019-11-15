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
     let query = "?email="+$authorization["email"]+"&listid="+listID;
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

 function columnName () {
     let listID = document.getElementById("columnName_list_id").value;
     let columnID = document.getElementById("columnName_column_id").value;
     let uri = "http://mars.local:9000/delectus/column_name";
     let query = "?email="+$authorization["email"]+"&listid="+listID+"&columnid="+columnID;
     let token = $authorization["token"];
     getAPI(uri+query, token, "columnName_response");
 }

 function columnNamed () {
     let listID = document.getElementById("columnNamed_list_id").value;
     let columnName = document.getElementById("columnNamed_column_name").value;
     let uri = "http://mars.local:9000/delectus/column_named";
     let query = "?email="+$authorization["email"]+"&listid="+listID+"&columnname="+columnName;
     let token = $authorization["token"];
     getAPI(uri+query, token, "columnNamed_response");
 }

 function columnDeleted () {
     let listID = document.getElementById("columnDeleted_list_id").value;
     let columnID = document.getElementById("columnDeleted_column_id").value;
     let uri = "http://mars.local:9000/delectus/column_deleted";
     let query = "?email="+$authorization["email"]+"&listid="+listID+"&columnid="+columnID;
     let token = $authorization["token"];
     getAPI(uri+query, token, "columnDeleted_response");
 }

 function deleteColumn () {
     let listID = document.getElementById("deleteColumn_list_id").value;
     let columnID = document.getElementById("deleteColumn_column_id").value;
     let uri = "http://mars.local:9000/delectus/delete_column";
     let query = "?email="+$authorization["email"]+"&listid="+listID+"&columnid="+columnID;
     let token = $authorization["token"];
     getAPI(uri+query, token, "deleteColumn_response");
 }

 function undeleteColumn () {
     let listID = document.getElementById("undeleteColumn_list_id").value;
     let columnID = document.getElementById("undeleteColumn_column_id").value;
     let uri = "http://mars.local:9000/delectus/undelete_column";
     let query = "?email="+$authorization["email"]+"&listid="+listID+"&columnid="+columnID;
     let token = $authorization["token"];
     getAPI(uri+query, token, "undeleteColumn_response");
 }

 function renameColumn () {
     let listID = document.getElementById("renameColumn_list_id").value;
     let columnID = document.getElementById("renameColumn_column_id").value;
     let columnName = document.getElementById("renameColumn_column_name").value;
     let uri = "http://mars.local:9000/delectus/rename_column";
     let query = "?email="+$authorization["email"]+"&listid="+listID+"&columnid="+columnID+"&columnname="+columnName;;
     let token = $authorization["token"];
     getAPI(uri+query, token, "renameColumn_response");
 }
 
 function listItems () {
     let listID = document.getElementById("listItems_list_id").value;
     let uri = "http://mars.local:9000/delectus/list_items";
     let query = "?email="+$authorization["email"]+"&listid="+listID;
     let token = $authorization["token"];
     getAPI(uri+query, token, "listItems_response");
 }
 
 function itemWithID () {
     let listID = document.getElementById("itemWithID_list_id").value;
     let itemID = document.getElementById("itemWithID_item_id").value;
     let uri = "http://mars.local:9000/delectus/item_with_id";
     let query = "?email="+$authorization["email"]+"&listid="+listID+"&itemid="+itemID;
     let token = $authorization["token"];
     getAPI(uri+query, token, "itemWithID_response");
 }

 function newItem () {
     let listID = document.getElementById("newItem_list_id").value;
     let uri = "http://mars.local:9000/delectus/new_item";
     let query = "?email="+$authorization["email"]+"&listid="+listID;
     let token = $authorization["token"];
     getAPI(uri+query, token, "newItem_response");
 }

 function deleteItem () {
     let listID = document.getElementById("deleteItem_list_id").value;
     let itemID = document.getElementById("deleteItem_item_id").value;
     let uri = "http://mars.local:9000/delectus/delete_item";
     let query = "?email="+$authorization["email"]+"&listid="+listID+"&itemid="+itemID;
     let token = $authorization["token"];
     getAPI(uri+query, token, "deleteItem_response");
 }

 function undeleteItem () {
     let listID = document.getElementById("undeleteItem_list_id").value;
     let itemID = document.getElementById("undeleteItem_item_id").value;
     let uri = "http://mars.local:9000/delectus/undelete_item";
     let query = "?email="+$authorization["email"]+"&listid="+listID+"&itemid="+itemID;
     let token = $authorization["token"];
     getAPI(uri+query, token, "undeleteItem_response");
 }

 function itemDeleted () {
     let listID = document.getElementById("itemDeleted_list_id").value;
     let itemID = document.getElementById("itemDeleted_item_id").value;
     let uri = "http://mars.local:9000/delectus/item_deleted";
     let query = "?email="+$authorization["email"]+"&listid="+listID+"&itemid="+itemID;
     let token = $authorization["token"];
     getAPI(uri+query, token, "itemDeleted_response");
 }

 function itemColumnValue () {
     let listID = document.getElementById("itemColumnValue_list_id").value;
     let itemID = document.getElementById("itemColumnValue_item_id").value;
     let columnID = document.getElementById("itemColumnValue_column_id").value;
     let uri = "http://mars.local:9000/delectus/item_column_value";
     let query = "?email="+$authorization["email"]+"&listid="+listID+"&itemid="+itemID+"&columnid="+columnID;
     let token = $authorization["token"];
     getAPI(uri+query, token, "itemColumnValue_response");
 }


 // TODO;
 // encodeURIComponent is needed to ensure that input values are received properly;
 // browsers won't even send unencoded parameters that contain certain characters
 // Fix all query-string constructors to properly encode input values before calling getAPI

 function setItemColumnValue () {
     let listID = encodeURIComponent(document.getElementById("setItemColumnValue_list_id").value);
     let itemID = encodeURIComponent(document.getElementById("setItemColumnValue_item_id").value);
     let columnID = encodeURIComponent(document.getElementById("setItemColumnValue_item_id").value);
     let newValue = encodeURIComponent(document.getElementById("setItemColumnValue_new_value").value);
     let uri = "http://mars.local:9000/delectus/set_item_column_value";
     let query1 = "?email="+$authorization["email"]+"&listid="+listID
     let query2 = "&itemid="+itemID+"&columnid="+columnID+"&newvalue="+newValue;
     let token = $authorization["token"];
     getAPI(uri+query1+query2, token, "setItemColumnValue_response");
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

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={columnName}>/delectus/column_name</button></td>
        <td>
            <input type="text" id="columnName_list_id" placeholder="List ID"/>
            <input type="text" id="columnName_column_id" placeholder="Column ID"/>
        </td>
        <td id="columnName_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={columnNamed}>/delectus/column_named</button></td>
        <td>
            <input type="text" id="columnNamed_list_id" placeholder="List ID"/>
            <input type="text" id="columnNamed_column_name" placeholder="Column name"/>
        </td>
        <td id="columnNamed_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={columnDeleted}>/delectus/column_deleted</button></td>
        <td>
            <input type="text" id="columnDeleted_list_id" placeholder="List ID"/>
            <input type="text" id="columnDeleted_column_id" placeholder="Column ID"/>
        </td>
        <td id="columnDeleted_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={deleteColumn}>/delectus/delete_column</button></td>
        <td>
            <input type="text" id="deleteColumn_list_id" placeholder="List ID"/>
            <input type="text" id="deleteColumn_column_id" placeholder="Column ID"/>
        </td>
        <td id="deleteColumn_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={undeleteColumn}>/delectus/undelete_column</button></td>
        <td>
            <input type="text" id="undeleteColumn_list_id" placeholder="List ID"/>
            <input type="text" id="undeleteColumn_column_id" placeholder="Column ID"/>
        </td>
        <td id="undeleteColumn_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={renameColumn}>/delectus/rename_column</button></td>
        <td>
            <input type="text" id="renameColumn_list_id" placeholder="List ID"/>
            <input type="text" id="renameColumn_column_id" placeholder="Column ID"/>
            <input type="text" id="renameColumn_column_name" placeholder="New name"/>
        </td>
        <td id="renameColumn_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={listItems}>/delectus/list_items</button></td>
        <td><input type="text" id="listItems_list_id" placeholder="List ID"/></td>
        <td id="listItems_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={itemWithID}>/delectus/item_with_id</button></td>
        <td>
            <input type="text" id="itemWithID_list_id" placeholder="List ID"/>
            <input type="text" id="itemWithID_item_id" placeholder="Item ID"/>
        </td>
        <td id="itemWithID_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={newItem}>/delectus/new_item</button></td>
        <td><input type="text" id="newItem_list_id" placeholder="List ID"/></td>
        <td id="newItem_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={deleteItem}>/delectus/delete_item</button></td>
        <td>
            <input type="text" id="deleteItem_list_id" placeholder="List ID"/>
            <input type="text" id="deleteItem_item_id" placeholder="Item ID"/>
        </td>
        <td id="deleteItem_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={undeleteItem}>/delectus/undelete_item</button></td>
        <td>
            <input type="text" id="undeleteItem_list_id" placeholder="List ID"/>
            <input type="text" id="undeleteItem_item_id" placeholder="Item ID"/>
        </td>
        <td id="undeleteItem_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={itemDeleted}>/delectus/item_deleted</button></td>
        <td>
            <input type="text" id="itemDeleted_list_id" placeholder="List ID"/>
            <input type="text" id="itemDeleted_item_id" placeholder="Item ID"/>
        </td>
        <td id="itemDeleted_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={itemColumnValue}>/delectus/item_column_value</button></td>
        <td>
            <input type="text" id="itemColumnValue_list_id" placeholder="List ID"/>
            <input type="text" id="itemColumnValue_item_id" placeholder="Item ID"/>
            <input type="text" id="itemColumnValue_column_id" placeholder="Column ID"/>
        </td>
        <td id="itemColumnValue_response"></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={setItemColumnValue}>/delectus/set_item_column_value</button></td>
        <td>
            <input type="text" id="setItemColumnValue_list_id" placeholder="List ID"/>
            <input type="text" id="setItemColumnValue_item_id" placeholder="Item ID"/>
            <input type="text" id="setItemColumnValue_column_id" placeholder="Column ID"/>
            <input type="text" id="setItemColumnValue_new_value" placeholder="New value"/>
        </td>
        <td id="setItemColumnValue_response"></td>
    </tr>
    
</table>

