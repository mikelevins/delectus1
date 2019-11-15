
export function apiEndpoint (endpoint) {
    return "http://mars.local:9000/delectus/"+endpoint;
}

export function getAPI(uri,authToken, displayElementID){
    fetch(uri, {method: 'GET',
                headers: {"Authorization": " Token "+authToken}})
        .then(response => response.json())
        .then(data => document.getElementById(displayElementID).innerHTML=JSON.stringify(data));
}

export function encodedValue(doc, elementName) {
    return encodeURIComponent(doc.getElementById(elementName).value);
}

export function makeQuery(parameterMap){
    let queryParts = [];
    for (const param in parameterMap) {
        queryParts.push(param+"="+parameterMap[param]);
    }
    if (queryParts.length > 0) {
        return "?"+queryParts.join('&');
    } else {
        return "";
    }
}
