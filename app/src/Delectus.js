// react base
import React, { Component } from 'react';
// helmet for meta tags in the head
import { Helmet } from "react-helmet";

// material-ui stuff

import lightBaseTheme from 'material-ui/styles/baseThemes/lightBaseTheme';

// utilities
import cuid from 'cuid';

// delectus' own includes
import DelectusAppBar from './DelectusAppBar';
import ListBox from './ListBox';
import './Delectus.css';

const mockTitle = "Fruits";
const mockColumns = ["Name", "Color", "Distinction", "Count"];
const mockFields = [
  ["Apples", "red", "tangy", 3],
  ["Bananas", "yellow", "mushy", 4],
  ["Cherries", "red", "sweet", 35],
  ["Dates", "brown", "fibery", 22],
  ["Elephants", "gray", "awesome", 0],
  ["Waffles", "tan", "buttery", 2]
];

function makeRow(fields) {
  var result = {
    '_id': 'row' + cuid(),
    'type': 'row',
    'list': null,
    'fields': fields.slice(),
    'createdAt': new Date().toISOString(),
    'updatedAt': ''
  }
  return result;
}

const mockRows = mockFields.map(makeRow);

function makeList(title, columns, rows) {
  var result = {
    '_id': 'list' + cuid(),
    'type': 'list',
    'title': title,
    'columns': columns.slice(),
    'rows': rows.map(function (row) { return row._id; }),
  }
  return result;
}

class Delectus extends Component {
  constructor() {
    super();
    this.state = {
      'theme': lightBaseTheme,
      'list': makeList(mockTitle, mockColumns, mockRows),
    };
  }

  render() {
    return (
      <div>
        <Helmet>
          <title>Delectus</title>
        </Helmet>

        <DelectusAppBar title={this.state.list.title}/>

        <ListBox 
        theme={this.state.theme}
        title={mockTitle}
        columns={mockColumns}
        rows={mockFields}
        />
        
      </div>
    );
  }
}

export default Delectus;
