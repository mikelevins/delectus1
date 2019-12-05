<script>
 // script
 // -----------------------------------------

 import { authorization } from "./auth.js";
 import { apiEndpoint, encodedValue, getAPI, makeQuery } from "./api.js";

 // lists
 // -----------------------------------------
 
 function getUserLists () {
     let uri = apiEndpoint("lists");
     let pMap = {"email": $authorization["email"]};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "getUserLists_response");
 }
 
 function getListByID () {
     let uri = apiEndpoint("list_with_id");
     let pMap = {"email": $authorization["email"], 
                "listid": encodedValue(document, "getListByID_list_id")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "getListByID_response");
 }
 
 function listName () {
     let uri = apiEndpoint("list_name");
     let pMap = {"email": $authorization["email"], 
                "listid": encodedValue(document, "listName_list_id")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "listName_response");
 }
 
 function renameList () {
     let uri = apiEndpoint("rename_list");
     let pMap = {"email": $authorization["email"], 
                "listid": encodedValue(document, "renameList_list_id"),
                "newname": encodedValue(document, "renameList_new_name")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "renameList_response");
 }

 function newList () {
     let uri = apiEndpoint("new_list");
     let pMap = {"email": $authorization["email"], 
                "name": encodedValue(document, "newList_list_name")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "newList_response");
 }
 
 function deleteList () {
     let uri = apiEndpoint("delete_list");
     let pMap = {"email": $authorization["email"], 
                "listid": encodedValue(document, "deleteList_list_id")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "deleteList_response");
 }
 
 function undeleteList () {
     let uri = apiEndpoint("undelete_list");
     let pMap = {"email": $authorization["email"], 
                "listid": encodedValue(document, "undeleteList_list_id")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "undeleteList_response");
 }
 
 function listDeleted () {
     let uri = apiEndpoint("list_deleted");
     let pMap = {"email": $authorization["email"], 
                "listid": encodedValue(document, "listDeleted_list_id")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "listDeleted_response");
 }

 function getListByName () {
     let uri = apiEndpoint("list_named");
     let pMap = {"email": $authorization["email"], 
                "name": encodedValue(document, "getListByName_list_name")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "getListByName_response");
 }
 
 function listColumns () {
     let uri = apiEndpoint("list_columns");
     let pMap = {"email": $authorization["email"], 
                "listid": encodedValue(document, "listColumns_list_id")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "listColumns_response");
 }

 function newColumn () {
     let uri = apiEndpoint("new_column");
     let pMap = {"email": $authorization["email"], 
                "listid": encodedValue(document, "newColumn_list_id"),
                "columnname": "newColumn_column_name"};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "newList_response");
 }

 function columnWithID () {
     let uri = apiEndpoint("column_with_id");
     let pMap = {"email": $authorization["email"], 
                "listid": encodedValue(document, "columnWithID_list_id"),
                "columnid": encodedValue(document, "columnWithID_column_id")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "columnWithID_response");
 }

 function columnName () {
     let uri = apiEndpoint("column_name");
     let pMap = {"email": $authorization["email"], 
                "listid": encodedValue(document, "columnName_list_id"),
                "columnid": encodedValue(document, "columnName_column_id")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "columnName_response");
 }

 function columnNamed () {
     let uri = apiEndpoint("column_named");
     let pMap = {"email": $authorization["email"], 
                "listid": encodedValue(document, "columnNamed_list_id"),
                "columnname": encodedValue(document, "columnNamed_column_name")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "columnNamed_response");
 }

 function columnDeleted () {
     let uri = apiEndpoint("column_deleted");
     let pMap = {"email": $authorization["email"], 
                "listid": encodedValue(document, "columnDeleted_list_id"),
                "columnid": encodedValue(document, "columnDeleted_column_id")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "columnDeleted_response");
 }

 function deleteColumn () {
     let uri = apiEndpoint("delete_column");
     let pMap = {"email": $authorization["email"], 
                "listid": encodedValue(document, "deleteColumn_list_id"),
                "columnid": encodedValue(document, "deleteColumn_column_id")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "deleteColumn_response");
 }

 function undeleteColumn () {
     let uri = apiEndpoint("undelete_column");
     let pMap = {"email": $authorization["email"], 
                "listid": encodedValue(document, "undeleteColumn_list_id"),
                "columnid": encodedValue(document, "undeleteColumn_column_id")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "undeleteColumn_response");
 }

 function renameColumn () {
     let uri = apiEndpoint("rename_column");
     let pMap = {"email": $authorization["email"], 
                "listid": encodedValue(document, "renameColumn_list_id"),
                "columnid": encodedValue(document, "renameColumn_column_id"),
                "columnname": encodedValue(document, "renameColumn_column_name")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "renameColumn_response");
 }
 
 function listItems () {
     let uri = apiEndpoint("list_items");
     let pMap = {"email": $authorization["email"], 
                "listid": encodedValue(document, "listItems_list_id")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "listItems_response");
 }
 
 function itemWithID () {
     let uri = apiEndpoint("item_with_id");
     let pMap = {"email": $authorization["email"], 
                "listid": encodedValue(document, "itemWithID_list_id"),
                "itemid": encodedValue(document, "itemWithID_item_id")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "itemWithID_response");
 }

 function newItem () {
     let uri = apiEndpoint("new_item");
     let pMap = {"email": $authorization["email"], 
                "listid": encodedValue(document, "newItem_list_id")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "newItem_response");
 }

 function deleteItem () {
     let uri = apiEndpoint("delete_item");
     let pMap = {"email": $authorization["email"], 
                "listid": encodedValue(document, "deleteItem_list_id"),
                "itemid": encodedValue(document, "deleteItem_item_id")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "deleteItem_response");
 }

 function undeleteItem () {
     let uri = apiEndpoint("undelete_item");
     let pMap = {"email": $authorization["email"], 
                "listid": encodedValue(document, "undeleteItem_list_id"),
                "itemid": encodedValue(document, "undeleteItem_item_id")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "undeleteItem_response");
 }

 function itemDeleted () {
     let uri = apiEndpoint("item_deleted");
     let pMap = {"email": $authorization["email"], 
                "listid": encodedValue(document, "itemDeleted_list_id"),
                "itemid": encodedValue(document, "itemDeleted_item_id")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "itemDeleted_response");
 }

 function itemColumnValue () {
     let uri = apiEndpoint("item_column_value");
     let pMap = {"email": $authorization["email"], 
                "listid": encodedValue(document, "itemColumnValue_list_id"),
                "itemid": encodedValue(document, "itemColumnValue_item_id"),
                "columnid": encodedValue(document, "itemColumnValue_column_id")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "itemColumnValue_response");
 }

 function setItemColumnValue () {
     let uri = apiEndpoint("set_item_column_value");
     let pMap = {"email": $authorization["email"], 
                "listid": encodedValue(document, "setItemColumnValue_list_id"),
                "itemid": encodedValue(document, "setItemColumnValue_item_id"),
                "columnid": encodedValue(document, "setItemColumnValue_column_id"),
                "newvalue": encodedValue(document, "setItemColumnValue_new_value")};
     let query = makeQuery(pMap);
     let token = $authorization["token"];
     getAPI(uri+query, token, "setItemColumnValue_response");
 }

</script>

<!------------ style definitions ------------>

<style>
 .endpoint { text-align: right; }

 .method-column { width: 5%; }
 .endpoint-column { width: 23%; }
 .parameters-column { width: 12%; }

 input { font-size: 1rem; }

 pre { font-size: 1rem; }
 
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
        <td><pre id="getUserLists_response"></pre></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={getListByID}>/delectus/list_with_id</button></td>
        <td><input size="36" type="text" id="getListByID_list_id" placeholder="List ID"/></td>
        <td><pre id="getListByID_response"></pre></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={listName}>/delectus/list_name</button></td>
        <td><input size="36" type="text" id="listName_list_id" placeholder="List ID"/></td>
        <td><pre id="listName_response"></pre></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={getListByName}>/delectus/list_named</button></td>
        <td><input size="36" type="text" id="getListByName_list_name" placeholder="List name"/></td>
        <td><pre id="getListByName_response"></pre></td>
    </tr>
    
    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={renameList}>/delectus/rename_list</button></td>
        <td>
            <input size="36" type="text" id="renameList_list_id" placeholder="List ID"/>
            <input size="36" type="text" id="renameList_new_name" placeholder="New name"/>
        </td>
        <td><pre id="renameList_response"></pre></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={newList}>/delectus/new_list</button></td>
        <td><input size="36" type="text" id="newList_list_name" placeholder="List name"/></td>
        <td><pre id="newList_response"></pre></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={deleteList}>/delectus/delete_list</button></td>
        <td><input size="36" type="text" id="deleteList_list_id" placeholder="List ID"/></td>
        <td><pre id="deleteList_response"></pre></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={undeleteList}>/delectus/undelete_list</button></td>
        <td><input size="36" type="text" id="undeleteList_list_id" placeholder="List ID"/></td>
        <td><pre id="undeleteList_response"></pre></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={listDeleted}>/delectus/list_deleted</button></td>
        <td><input size="36" type="text" id="listDeleted_list_id" placeholder="List ID"/></td>
        <td><pre id="listDeleted_response"></pre></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={listColumns}>/delectus/list_columns</button></td>
        <td><input size="36" type="text" id="listColumns_list_id" placeholder="List ID"/></td>
        <td><pre id="listColumns_response"></pre></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={newColumn}>/delectus/new_column</button></td>
        <td>
            <input size="36" type="text" id="newColumn_list_id" placeholder="List ID"/>
            <input size="36" type="text" id="newColumn_column_name" placeholder="Column name"/>
        </td>
        <td><pre id="newColumn_response"></pre></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={columnWithID}>/delectus/column_with_id</button></td>
        <td>
            <input size="36" type="text" id="columnWithID_list_id" placeholder="List ID"/>
            <input size="36" type="text" id="columnWithID_column_id" placeholder="Column ID"/>
        </td>
        <td><pre id="columnWithID_response"></pre></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={columnName}>/delectus/column_name</button></td>
        <td>
            <input size="36" type="text" id="columnName_list_id" placeholder="List ID"/>
            <input size="36" type="text" id="columnName_column_id" placeholder="Column ID"/>
        </td>
        <td><pre id="columnName_response"></pre></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={columnNamed}>/delectus/column_named</button></td>
        <td>
            <input size="36" type="text" id="columnNamed_list_id" placeholder="List ID"/>
            <input size="36" type="text" id="columnNamed_column_name" placeholder="Column name"/>
        </td>
        <td><pre id="columnNamed_response"></pre></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={columnDeleted}>/delectus/column_deleted</button></td>
        <td>
            <input size="36" type="text" id="columnDeleted_list_id" placeholder="List ID"/>
            <input size="36" type="text" id="columnDeleted_column_id" placeholder="Column ID"/>
        </td>
        <td><pre id="columnDeleted_response"></pre></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={deleteColumn}>/delectus/delete_column</button></td>
        <td>
            <input size="36" type="text" id="deleteColumn_list_id" placeholder="List ID"/>
            <input size="36" type="text" id="deleteColumn_column_id" placeholder="Column ID"/>
        </td>
        <td><pre id="deleteColumn_response"></pre></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={undeleteColumn}>/delectus/undelete_column</button></td>
        <td>
            <input size="36" type="text" id="undeleteColumn_list_id" placeholder="List ID"/>
            <input size="36" type="text" id="undeleteColumn_column_id" placeholder="Column ID"/>
        </td>
        <td><pre id="undeleteColumn_response"></pre></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={renameColumn}>/delectus/rename_column</button></td>
        <td>
            <input size="36" type="text" id="renameColumn_list_id" placeholder="List ID"/>
            <input size="36" type="text" id="renameColumn_column_id" placeholder="Column ID"/>
            <input size="36" type="text" id="renameColumn_column_name" placeholder="New name"/>
        </td>
        <td><pre id="renameColumn_response"></pre></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={listItems}>/delectus/list_items</button></td>
        <td><input size="36" type="text" id="listItems_list_id" placeholder="List ID"/></td>
        <td><pre id="listItems_response"></pre></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={itemWithID}>/delectus/item_with_id</button></td>
        <td>
            <input size="36" type="text" id="itemWithID_list_id" placeholder="List ID"/>
            <input size="36" type="text" id="itemWithID_item_id" placeholder="Item ID"/>
        </td>
        <td><pre id="itemWithID_response"></pre></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={newItem}>/delectus/new_item</button></td>
        <td><input size="36" type="text" id="newItem_list_id" placeholder="List ID"/></td>
        <td><pre id="newItem_response"></pre></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={deleteItem}>/delectus/delete_item</button></td>
        <td>
            <input size="36" type="text" id="deleteItem_list_id" placeholder="List ID"/>
            <input size="36" type="text" id="deleteItem_item_id" placeholder="Item ID"/>
        </td>
        <td><pre id="deleteItem_response"></pre></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={undeleteItem}>/delectus/undelete_item</button></td>
        <td>
            <input size="36" type="text" id="undeleteItem_list_id" placeholder="List ID"/>
            <input size="36" type="text" id="undeleteItem_item_id" placeholder="Item ID"/>
        </td>
        <td><pre id="undeleteItem_response"></pre></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={itemDeleted}>/delectus/item_deleted</button></td>
        <td>
            <input size="36" type="text" id="itemDeleted_list_id" placeholder="List ID"/>
            <input size="36" type="text" id="itemDeleted_item_id" placeholder="Item ID"/>
        </td>
        <td><pre id="itemDeleted_response"></pre></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={itemColumnValue}>/delectus/item_column_value</button></td>
        <td>
            <input size="36" type="text" id="itemColumnValue_list_id" placeholder="List ID"/>
            <input size="36" type="text" id="itemColumnValue_item_id" placeholder="Item ID"/>
            <input size="36" type="text" id="itemColumnValue_column_id" placeholder="Column ID"/>
        </td>
        <td><pre id="itemColumnValue_response"></pre></td>
    </tr>

    <tr>
        <td>GET</td>
        <td class="endpoint"><button on:click={setItemColumnValue}>/delectus/set_item_column_value</button></td>
        <td>
            <input size="36" type="text" id="setItemColumnValue_list_id" placeholder="List ID"/>
            <input size="36" type="text" id="setItemColumnValue_item_id" placeholder="Item ID"/>
            <input size="36" type="text" id="setItemColumnValue_column_id" placeholder="Column ID"/>
            <input size="36" type="text" id="setItemColumnValue_new_value" placeholder="New value"/>
        </td>
        <td><pre id="setItemColumnValue_response"></pre></td>
    </tr>
    
</table>

