<script>
 import { authorization } from "./auth.js";

 function handleSubmit(event) {
     let uri = "http://mars.local:9000/delectus/login";
     let email_input = document.getElementById("email").value
     let password_input = document.getElementById("password").value
     let query_string = "?email=" + email_input + "&password=" + password_input
     fetch(uri+query_string)
         .then(response => response.json())
         .then(data => authorization.set({email: email_input, token: data.token}));
 }
</script>
<form on:submit|preventDefault={handleSubmit}>
    <input type="text" id="email" name="email" placeholder="email address" required /><br/>
    <input type="password" id="password" name="password" placeholder="password" required /><br/>
    <button type="submit">Log In</button>
</form>

