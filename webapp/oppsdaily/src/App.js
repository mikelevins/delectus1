import React, { Component } from 'react';
import './App.css';
import DocEditor from './DocEditor.js';
import DocList from './DocList.js';

// ---------------------------------------------------------
// helper functions
// ---------------------------------------------------------

function syncWithRemoteDB(props) {
  props.localDB.sync(props.remoteDB);
}

function refreshAllDocs(props) {
  props.localDB.allDocs(
    { include_docs: true, descending: true },
    function (err, result) {
      if (err) {
        console.log("Error getting documents: ");
        console.log(err);
      } else {
        var the_rows = result.rows;
        props.app.setState({ allDocs: the_rows });
      }
    }
  );
}

// ---------------------------------------------------------
// App component
// ---------------------------------------------------------

class App extends Component {
  state = {
    allDocs: [],
    showEditor: false,
    selectedDoc: null,
    localDB: this.props.localPouchDB,
    remoteDB: this.props.remoteCouchDB
  }

  // methods
  // ---------------------------------------------------------

  componentDidMount() {
    document.title = "Opps Daily"

    syncWithRemoteDB({
      localDB: this.state.localDB,
      remoteDB: this.state.remoteDB
    });
    refreshAllDocs({
      app: this,
      localDB: this.state.localDB
    });
  }

  hideEditor() {
    this.setState({ showEditor: false })
  }

  showEditor(props) {
    this.setState({ showEditor: true })
    this.setState({ selectedDoc: props.doc });
  }

  // shorten very long fields for display
  formatForDisplay(props) {
    const val = props.value;
    if (val.length > 512) {
      return val.substring(82, 320) + '...';
    } else {
      return val;
    }
  }

  // main render
  // ---------------------------------------------------------

  render() {
    return (
      <div className="App">
        <h1 className="center">Opps Daily</h1>
        <h3 className="center">document count: {this.state.allDocs.length}</h3>
        <DocList app={this} documents={this.state.allDocs} />
        {this.state.showEditor && <DocEditor app={this} doc={this.state.selectedDoc} />}
      </div>
    );
  }

}

// ---------------------------------------------------------
// main export
// ---------------------------------------------------------

export default App;
