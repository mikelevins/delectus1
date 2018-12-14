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
    this.state = {
      username: localStorage.getItem('delectus_username')
    }
  }

  // accessor methods
  // ---------------------------------------------------------


  // lifecycle methods
  // ---------------------------------------------------------

  initDatabases = () => {
    const username = this.state.username;
    const localDB = new PouchDB('Delectus');
    const remoteDB = (username) ? username : null;

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
    const username = this.state.username;

    return (
      <React.Fragment>
        <CssBaseline />
        <div className="App">
          <DelectusLogin username={username} />
        </div>
      </React.Fragment>
    );
  }

}

// ---------------------------------------------------------
// main export
// ---------------------------------------------------------

export default App;
