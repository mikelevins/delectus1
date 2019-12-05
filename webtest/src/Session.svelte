<script>
 // script
 // -----------------------------------------

 import { authorization, discardAuthorization } from "./auth.js";

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

 
 // user data
 // -----------------------------------------

 function getUserID () {
     callDelectusAPI("userid","getUserID_response");
 }

</script>

<!------------ style definitions ------------>

<style>
 .authdata {
     font-family: monospace;
 }
 .bg-light-gray {
     background-color: #efefef;
 }
 
 .box {
     border: 1px solid black;
     padding-top: 0.5rem;
     padding-bottom: 0.5rem;
     padding-left: 1rem;
     padding-right: 1rem;
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

<div>
    <p class="box bg-light-gray">
        Logged in as: <strong>{$authorization.email}</strong>&nbsp;
        <button on:click={discardAuthorization}>Log out</button>
    </p>

    <div  class="box bg-light-gray">
        <p>Authorization data:</p>
        <pre>{JSON.stringify($authorization, undefined, 2)}</pre>
    </div>
</div>
