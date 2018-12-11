import React, { Component } from 'react';
import './App.css';

import PouchDB from 'pouchdb';

// ---------------------------------------------------------
// helper functions
// ---------------------------------------------------------

// ---------------------------------------------------------
// App component
// ---------------------------------------------------------

class App extends Component {

  // constructor
  // ---------------------------------------------------------

  constructor(props) {
    super(props);

    this.state = {
      localDB: new PouchDB('Delectus'),
      remoteDB: new PouchDB('http://mars:5984/mikel.evins') // TODO: initialize the username properly
    }
  }

  // lifecycle methods
  // ---------------------------------------------------------

  componentDidMount() {
    document.title = "Delectus"
  }

  // main render
  // ---------------------------------------------------------

  render() {
    return (
      <div className="App">
        <h1>Delectus</h1>
      </div>
    );
  }

}

// ---------------------------------------------------------
// main export
// ---------------------------------------------------------

export default App;
