import DelectusAppBar from "./AppBar";
import PouchDB from "pouchdb";
import React, { Component } from "react";

import "./App.css";
import { AppState } from "./AppState";

class App extends Component {
  constructor(props) {
    super(props);

    let db = new PouchDB('http://mars.local:5984/delectulus');
    AppState.updateRemoteCouch(db);
  }

  render() {
    return (
      <div className="App">
        <DelectusAppBar />
      </div>
    );
  }
}

export default App;
