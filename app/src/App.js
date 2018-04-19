import React, { Component } from 'react';
import ReactDOM from 'react-dom';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider'
import DelectusAppBar from './DelectusAppBar';
import PouchDB from 'pouchdb';
import './App.css';

const localDB = new PouchDB('delectus');

const mockList = ["Apples", "Bananas", "Cherries", "Waffles"];

function ListItems(props) {
  return (
    <div className="ListItems">
      <ul>
        {props.items.map(function (listValue) {
          return <li>{listValue}</li>;
        })}
      </ul>
    </div>
  )
}

function AddRowButton() {
  return (
    <div className="AddRowButton">+</div>
  )
}

function ListBox(props) {
  return <div className="ListBox">
    <span>{props.name}</span>
    <span className="AddColumnButton">+</span>
    <p></p>
    <ListItems items={mockList} />
    <AddRowButton />
  </div>
}

class App extends Component {

  render() {
    return (
      <div className="App">
        <MuiThemeProvider>
        <DelectusAppBar />
        </MuiThemeProvider>
        <ListBox name="Listname" />
      </div>
    );
  }
}

export default App;
