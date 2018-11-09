import React, { Component } from 'react';
import './App.css';
import CssBaseline from '@material-ui/core/CssBaseline';
import DocEditor from './DocEditor.js';
import DocList from './DocList.js';

// ---------------------------------------------------------
// App component
// ---------------------------------------------------------

class App extends Component {

  // constructor
  // ---------------------------------------------------------

  constructor(props) {
    super(props);

    this.state = {
      allDocs: [],
      showEditor: false,
      selectedDoc: null,
      localDB: this.props.localPouchDB,
      remoteDB: this.props.remoteCouchDB
    }
  }

  // methods
  // ---------------------------------------------------------

  syncWithRemoteDB() {
    this.state.localDB.sync(this.state.remoteDB);
  } // syncWithRemoteDB

  refreshAllDocs() {
    var app = this;
    app.state.localDB.allDocs(
      { include_docs: true, descending: true },
      function (err, result) {
        if (err) {
          console.log("Error getting documents: ");
          console.log(err);
        } else {
          var the_rows = result.rows;
          app.setState({ allDocs: the_rows });
        }
      }
    );
  } // end refreshAllDocs

  componentDidMount() {
    document.title = "Opps Daily"
    this.syncWithRemoteDB();
    this.refreshAllDocs();
  } // end componentDidMount

  // use fat arrow function to ensure 'this' refers to App
  editSelectedDocument = (doc) => {
    this.setState({ selectedDoc: doc, showEditor: true });
  }

  // use fat arrow function to ensure 'this' refers to App
  cancelAndDismissDocumentEditor = () => {
    this.setState({ selectedDoc: null, showEditor: false });
  }

  // main render
  // ---------------------------------------------------------

  render() {
    if (this.state.showEditor) {
      // DocEditor active
      return (
        <React.Fragment>
          <CssBaseline />
          <div className="App">
            <DocEditor app={this} doc={this.state.selectedDoc} />
          </div>
        </React.Fragment>
      );

    } else {
      // DocList active
      return (
        <React.Fragment>
          <CssBaseline />
          <div className="App">
            <DocList app={this} docs={this.state.allDocs} />
          </div>
        </React.Fragment>
      );

    }
  } // end render
} // end App

// ---------------------------------------------------------
// exports
// ---------------------------------------------------------

export default App;
