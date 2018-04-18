import React, { Component } from 'react';
import PouchDB from 'pouchdb';
import './App.css';

const localDB = new PouchDB('delectus');

function ListBox(props){
  return <div className="ListBox"><span>{props.name}</span></div>
}

class App extends Component {
  render() {
    return (
      <div className="App">
        <header className="App-header">
          <h1 className="App-title">Delectus</h1>
        </header>
        <ListBox name="Listname" />
      </div>
    );
  }
}

export default App;
