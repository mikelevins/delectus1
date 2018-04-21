import React, { Component } from 'react';
import { Helmet } from "react-helmet";
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import DelectusAppBar from './DelectusAppBar';
import ListBox from './ListBox';
import PouchDB from 'pouchdb';
import cuid from 'cuid';
import './App.css';

const localDB = new PouchDB('delectus');


function makeRow(elements) {
  var result = {
    '_id': 'row' + cuid(),
    'type': 'row',
    'list': null,
    'fields': elements.slice(),
    'createdAt': new Date().toISOString(),
    'updatedAt': ''
  }
  return result;
}

console.log('mockRow:');
console.log(makeRow(["Fred","Wilma","Barney","Betty"]));

function makeList(columns,rows) {
  var result = {
    '_id': 'list' + cuid(),
    'type': 'list',
    'title': 'A List',
    'columns': columns.slice(),
    'rows': rows.map(function(row){return row._id;}),
  }
  return result;
}

const mockColumns = ["Name", "Color", "Distinction", "Count"];

const mockFields = [
  ["Apples", "red", "tangy", 3],
  ["Bananas", "yellow", "mushy", 4],
  ["Cherries", "red", "sweet", 35],
  ["Dates", "brown", "fibery", 22],
  ["Elephants", "gray", "awesome", 0],
  ["Waffles", "tan", "buttery", 2]
];

const mockRows = mockFields.map(makeRow);
const mockList = makeList(mockColumns,mockRows);

console.log('mockList:');
console.log(mockList);

class App extends Component {
  constructor () {
    super();
    this.state = {'mockRows': mockRows};
  }

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
            items={mockFields}
          />
        </MuiThemeProvider>

      </div>
    );
  }
}

export default App;
