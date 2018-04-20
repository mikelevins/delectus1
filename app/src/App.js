import React, { Component } from 'react';
import {Helmet} from "react-helmet";
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import DelectusAppBar from './DelectusAppBar';
import ListBox from './ListBox';
import PouchDB from 'pouchdb';
import './App.css';

const localDB = new PouchDB('delectus');
const mockRows = [["Apples", "red", "tangy"], ["Bananas", "yellow", "mushy"], ["Cherries","red", "sweet"], 
["Dates","brown", "fibery"], ["Elephants","gray", "awesome"], ["Waffles","tan", "buttery"]];

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
            items={mockRows}
          />
        </MuiThemeProvider>

      </div>
    );
  }
}

export default App;
