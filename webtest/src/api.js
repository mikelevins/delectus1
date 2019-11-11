
function getAPI(uri,authToken, displayElementID){
    fetch(uri, {method: 'GET',
                headers: {"Authorization": " Token "+authToken}})
        .then(response => response.json())
        .then(data => document.getElementById(displayElementID).innerHTML=JSON.stringify(data));
}

export getAPI;
