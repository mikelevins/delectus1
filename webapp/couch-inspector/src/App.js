import React, { Component } from 'react';
import './App.css';

import axios from 'axios';

import CouchInspector from './CouchInspector.js';
import EmptyInspector from './EmptyInspector.js';

// auxiliary constructors
// ---------------------------------------------------------

function MakeAllDatabasesRequest(url) {
  return (url + '/_all_dbs');
}

function MakeAllDocumentsRequest(url, dbName, limit, offset) {
  return (
    url + '/' + dbName + '/_all_docs?limit=' +
    String(limit) + '&skip=' + String(offset));
}

// App class
// ---------------------------------------------------------

class App extends Component {

  // constructor
  // ---------------------------------------------------------

  constructor(props) {
    super(props);

    this.state = {
      authErrorMessage: null,
      authRequested: false,
      couchURL: '',
      databases: null,
      databaseCount: 0,
      selectedDatabase: null,
      documents: [],
      documentsCount: 0,
      documentsPerPage: 10,
      documentsPageOffset: 0,
      selectedDocumentID: null,
      selectedDocument: null,
      sessionCookies: {}
    };
  }

  // state accessors
  // ---------------------------------------------------------

  getAuthErrorMessage = () => { return this.state.authErrorMessage; }
  getAuthRequested = () => { return this.state.authRequested; }
  getCouchURL = () => { return this.state.couchURL; }
  getDatabases = () => { return this.state.databases; }
  getDatabaseCount = () => { return this.state.databases.length; }
  getDocumentsPerPage = () => { return this.state.documentsPerPage; }
  getDocumentsPageOffset = () => { return this.state.documentsPageOffset; }
  getSelectedDatabase = () => { return this.state.selectedDatabase; }
  getDocuments = () => { return this.state.documents; }
  getDocumentsCount = () => { return this.state.documentsCount; }
  getSelectedDocument = () => { return this.state.selectedDocument; }
  getSelectedDocumentID = () => { return this.state.selectedDocumentID; }
  getSessionCookies = () => { return this.state.sessionCookies; }

  // view accessors
  // ---------------------------------------------------------

  getFormURL = () => { return (document.getElementById('CouchDB_URL').value); }

  getLoginUsername = () => {
    const elt = document.getElementById('username');
    if (elt) {
      return (elt.value);
    } else {
      return ('');
    }
  }

  getLoginPassword = () => {
    const elt = document.getElementById('password');
    if (elt) {
      return (elt.value);
    } else {
      return ('');
    }
  }

  // lifecycle events
  // ---------------------------------------------------------

  componentDidMount() { document.title = "Couch Inspector" }

  // logging
  // ---------------------------------------------------------

  logServerError = (error, message) => {
    console.log(message);
    console.log(error.response.status);
    console.log(error.response.data);
    console.log(error.response.headers);
  }

  logConnectionError = (error, message) => {
    console.log(message);
    console.log(error);
  }

  // authentication
  // ---------------------------------------------------------

  authenticateUser = (dbname, username, password) => {
    const url = this.getCouchURL();
    const arbitraryCookieStandin = (dbname+username+password);
    alert('this is where we ask CouchDB to authenticate us');
    return arbitraryCookieStandin;
  }

  handleLogin = (dbName, username, password) => {
    const loginCookie = this.authenticateUser(dbName, username, password);
    this.addLoginSession(dbName, loginCookie);
    this.updateSelectedDatabase({ dbName: dbName });
  }

  handleLoginCanceled = () => {
    const app = this;
    app.setState({
      authRequested: false,
      selectedDatabase: null,
    });
  }

  addLoginSession = (dbname, cookie) => {
    const app = this;
    const oldCookies = app.getSessionCookies();
    const newCookie = { [dbname]: cookie };
    const newCookies = Object.assign({}, oldCookies, newCookie);
    app.setState({
      sessionCookies: newCookies
    })
  }

  removeLoginSession = (dbname) => {
    const app = this;
    const oldSessions = app.getSessionCredentials();
    const newCredentials = { [dbname]: null };
    const newSessions = Object.assign({}, oldSessions, newCredentials);
    app.setState({
      sessionCredentials: newSessions
    })
  }

  // TODO: add removeLoginSession to handle logouts and failed authentications

  // update state with data from the server
  // ---------------------------------------------------------

  updateServerConnection = () => {
    const app = this;
    const new_couch_url = this.getFormURL();
    const requestStr = MakeAllDatabasesRequest(new_couch_url);

    axios.get(requestStr)
      .then(
        (response) => {
          app.setState({
            authRequested: false,
            couchURL: new_couch_url,
            databases: response.data,
            databaseCount: response.data.length,
            selectedDatabase: null,
            documents: [],
            documentsCount: 0,
            documentsPageOffset: 0,
            selectedDocumentID: null,
            selectedDocument: null,
          })
        }
      )
      .catch((error) => {
        app.setState({
          authRequested: false,
          couchURL: new_couch_url,
          databases: [],
          databaseCount: 0,
          selectedDatabase: null,
          documents: [],
          documentsCount: 0,
          documentsPageOffset: 0,
          selectedDocumentID: null,
          selectedDocument: null
        });

        if (error.response) {
          this.logServerError(error, "handleConnect: the server returned an error");
        } else {
          this.logConnectionError(error, "handleConnect: no response from the server");
        }
      })
  }

  updateSelectedDatabase = (props) => {
    const dbName = props.dbName;
    const app = this;
    const authRequested = app.getAuthRequested();
    const couchURL = app.getCouchURL();
    const limit = app.getDocumentsPerPage();
    const docsRequest = MakeAllDocumentsRequest(couchURL, dbName, limit, 0);


    // if there's a pending auth request then we need to clear it
    if (authRequested) {
      // TODO: if there is a pending authRequest in app.state then
      // updateSelectedDatabase needs to obtain auth and clear the pending request
      // before proceeding. Then it needs to update the app's state to display
      // the fetched db data. If there's no such pending request, then
      // it simply gets the dbdata and displays it.
      this.handleLoginCanceled();
    }

    axios.get(docsRequest)
      .then(response => {
        app.setState({
          authRequested: false,
          couchURL: couchURL,
          selectedDatabase: dbName,
          documents: response.data.rows,
          documentsCount: response.data.total_rows,
          documentsPageOffset: 0,
          selectedDocumentID: null,
          selectedDocument: null
        })
      })
      .catch((error) => {
        app.setState({
          authRequested: false,
          couchURL: couchURL,
          selectedDatabase: null,
          documents: [],
          documentsCount: 0,
          documentsPageOffset: 0,
          selectedDocumentID: null,
          selectedDocument: null
        });
        if (error.response) {
          if (error.response.status === 401) {
            // 'Unauthorized'
            app.setState({ authRequested: true, selectedDatabase: dbName });
          } else {
            this.logServerError(error, 'App.updateSelectedDatabase: the server returned an error');
          }
        } else {
          this.logConnectionError(error, "App.updateSelectedDatabase: no response from the server");
        }
      })
  }

  updateNextDatabasePage = () => {
    const app = this;
    const couchURL = app.getCouchURL();
    const dbName = app.getSelectedDatabase();
    const limit = app.getDocumentsPerPage();
    const offset = app.getDocumentsPageOffset() + limit;
    const docsRequest = MakeAllDocumentsRequest(couchURL, dbName, limit, offset);

    axios.get(docsRequest)
      .then(response => {
        app.setState({
          couchURL: couchURL,
          documents: response.data.rows,
          documentsPageOffset: offset,
          selectedDocumentID: null,
          selectedDocument: null
        })
      })
      .catch((error) => {
        if (error.response) {
          this.logServerError(error, 'App.updateNextDatabasePage: the server returned an error');
        } else {
          this.logConnectionError(error, "App.updateNextDatabasePage: no response from the server");
        }
      })
  }

  updatePreviousDatabasePage = () => {
    const app = this;
    const couchURL = app.getCouchURL();
    const dbName = app.getSelectedDatabase();
    const limit = app.getDocumentsPerPage();
    const oldOffset = app.getDocumentsPageOffset();
    const newOffset = (oldOffset - limit <= 0) ? 0 : (oldOffset - limit);
    const docsRequest = MakeAllDocumentsRequest(couchURL, dbName, limit, newOffset);

    axios.get(docsRequest)
      .then(response => {
        app.setState({
          couchURL: couchURL,
          documents: response.data.rows,
          documentsPageOffset: newOffset,
          selectedDocumentID: null,
          selectedDocument: null
        })
      })
      .catch((error) => {
        if (error.response) {
          this.logServerError(error, 'App.updateNextDatabasePage: the server returned an error');
        } else {
          this.logConnectionError(error, "App.updateNextDatabasePage: no response from the server");
        }
      })
  }

  updateSelectedDocument = (documentID) => {
    const app = this;
    const couchURL = app.getCouchURL();
    const dbName = app.getSelectedDatabase();
    const docRequest = ('/' + dbName + '/' + documentID);

    axios.get(couchURL + docRequest)
      .then(response => {
        app.setState({
          selectedDocumentID: documentID,
          selectedDocument: response.data,
        })
      })
      .catch((error) => {
        if (error.response) {
          // the server returned an error response
          this.logServerError(error, 'App.updateSelectedDocument: the server returned an error');
        } else {
          // the server never responded
          this.logConnectionError(error, "App.updateSelectedDocument: no response from the server");
        }
      })
  }

  // main render
  // ---------------------------------------------------------

  render() {
    const app = this;
    const dbList = app.getDatabases();

    if (dbList) {
      // we have databases; display them
      return (<CouchInspector app={app} />);

    } else {
      // we don't have databases; display a blank div
      return (<EmptyInspector app={app} />);
    }
  }
}

// exports
// ---------------------------------------------------------

export default App;

