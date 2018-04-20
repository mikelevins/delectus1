import React, { Component } from 'react';
import { Helmet } from "react-helmet";
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import DelectusAppBar from './DelectusAppBar';
import ListBox from './ListBox';
import PouchDB from 'pouchdb';
import './App.css';

const localDB = new PouchDB('delectus');
const mockColumns = ["Name", "Color", "Distinction", "Count"];
const mockRows = [
  ["Apples", "red", "tangy", 3],
  ["Bananas", "yellow", "mushy", 4],
  ["Cherries", "red", "sweet", 35],
  ["Dates", "brown", "fibery", 22],
  ["Elephants", "gray", "awesome", 0],
  ["Waffles", "tan", "buttery", 2]
];

class App extends Component {

  render() {
    return (
      <div className="App">
        <Helmet>
          <title>Delectus</title>
        </Helmet>
        <MuiThemeProvider>
          <DelectusAppBar title="Delectus" />
          <ListBox
            name="Fruits"
            columns={mockColumns}
            items={mockRows}
          />
        </MuiThemeProvider>

      </div>
    );
  }
}

export default App;
