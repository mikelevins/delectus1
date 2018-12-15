import React, { Component } from 'react';
import './App.css';

import CssBaseline from '@material-ui/core/CssBaseline';

import PouchDB from 'pouchdb';

import DelectusLogin from './DelectusLogin.js';

// ---------------------------------------------------------
// App component
// ---------------------------------------------------------

class App extends Component {

  // constructor
  // ---------------------------------------------------------

  constructor(props) {
    super(props);

    var remoteCouch = localStorage.getItem('delectus_couchURL');
    if (!remoteCouch) { remoteCouch = this.defaultCouchURL(); }

      this.state = {
        username: localStorage.getItem('delectus_username'),
        couchURL: remoteCouch
      }
  }

  // default values
  // ---------------------------------------------------------

  // TODO: change to public Delectus couch when it's available
  defaultCouchURL = () => 'http://mars.local:5984';

  // lifecycle methods
  // ---------------------------------------------------------

  initDatabases = () => {
    const url = this.state.couchURL;
    const username = this.state.username;
    const localDB = new PouchDB('Delectus');
    var remoteDB = null;

    if (username && url) { remoteDB = (url + '/' + username); }

    this.setState({
      localDB: localDB,
      remoteDB: remoteDB
    });
  }

  componentDidMount() {
    document.title = "Delectus";
    this.initDatabases();
  }

  // main render
  // ---------------------------------------------------------

  render() {


    return (
      <React.Fragment>
        <CssBaseline />
        <div className="App">
          <DelectusLogin />
        </div>
      </React.Fragment>
    );
  }

}

// ---------------------------------------------------------
// main export
// ---------------------------------------------------------

export default App;
