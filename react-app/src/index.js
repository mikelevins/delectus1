// ---------------------------------------------------------------------
// React imports
// ---------------------------------------------------------------------

import React from 'react';
import ReactDOM from 'react-dom';

// ---------------------------------------------------------------------
// PouchDB imports
// ---------------------------------------------------------------------

import PouchDB from 'pouchdb';
import PouchDBFind from 'pouchdb-find';

// ---------------------------------------------------------------------
// IBM ShoppingList sample imports
// ---------------------------------------------------------------------

import { ShoppingListFactory, ShoppingListRepositoryPouchDB } from 'ibm-shopping-list-model';

// ---------------------------------------------------------------------
// Local imports
// ---------------------------------------------------------------------

import './index.css';
import App from './App';
import registerServiceWorker from './registerServiceWorker';

// ---------------------------------------------------------------------
// App Setup
// ---------------------------------------------------------------------

// set up the database
PouchDB.plugin(PouchDBFind); // install the pouchdb-find plugin
const localDB = new PouchDB('shopping_list_react');
let remoteDB = undefined;

// these are framework-independent interfaces for working with lists and items in the list
const shoppingListFactory = new ShoppingListFactory();
const shoppingListRepository = new ShoppingListRepositoryPouchDB(localDB);


// ---------------------------------------------------------------------
// App startup
// ---------------------------------------------------------------------

// key offline-first step - more info at
// https://developers.google.com/web/fundamentals/primers/service-workers/
registerServiceWorker(); 

// create the app with access to the helper interfaces, the local
// database store (PouchDB), and the remote one
shoppingListRepository.ensureIndexes()
    .then((response) => {
        ReactDOM.render(
                <App
            shoppingListFactory={shoppingListFactory}
            shoppingListRepository={shoppingListRepository}
            localDB={localDB}
            remoteDB={remoteDB}
                />,
            document.getElementById('root'));
    }).catch( reason => {
        console.log("error in shoppingListRepository.ensureIndexes():");
        console.log(reason) 
    });
