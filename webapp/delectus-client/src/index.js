import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';
import App from './App';
import * as serviceWorker from './serviceWorker';

import PouchDB from 'pouchdb';
import PouchDBAuth from 'pouchdb-authentication';
import PouchDBFind from 'pouchdb-find';

PouchDB.plugin(PouchDBAuth);
PouchDB.plugin(PouchDBFind);

const preferencesDB = new PouchDB('DelectusPreferences');


ReactDOM.render(
    <App preferencesDB={preferencesDB}/>, 
    document.getElementById('root')
);

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: http://bit.ly/CRA-PWA
serviceWorker.unregister();
