import React from 'react';
import ReactDOM from 'react-dom';
import './tachyons.min.css';
import './index.css';
import App from './App';
import * as serviceWorker from './serviceWorker';

import PouchDB from 'pouchdb';
import PouchDBFind from 'pouchdb-find';

PouchDB.plugin(PouchDBFind);
const localPouchDB = new PouchDB('oppsdaily');
const remoteCouchDB = new PouchDB('http://mars:5984/oppsdaily');

ReactDOM.render(
    <App localPouchDB={localPouchDB} remoteCouchDB={remoteCouchDB}/>, 
    document.getElementById('root')
);

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: http://bit.ly/CRA-PWA
serviceWorker.unregister();
