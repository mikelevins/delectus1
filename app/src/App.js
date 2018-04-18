import React, { Component } from 'react';
import PouchDB from 'pouchdb';
import './App.css';

const localDB = new PouchDB('delectus');


class App extends Component {
  render() {
    return (
      <div className="App">
        <header className="App-header">
          <h1 className="App-title">Delectus</h1>
        </header>

      </div>
    );
  }
}

export default App;
