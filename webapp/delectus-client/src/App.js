import React, { Component } from 'react';
import './App.css';

// ---------------------------------------------------------
// Styles for the components (included by the App)
// ---------------------------------------------------------

const css = `
.Entry { 
  border: 1px solid black;
  border-collapse: collapse;
  margin: .5em; 
  padding: 0.25em; 
  text-align: left;
}

.EntryField { 
  border: 1px solid black;
  border-collapse: collapse;
}
`;

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

function formatForDisplay(props) {
  const val = props.value;
  if (val.length > 82) {
    return val.substring(82, 320);
  } else {
    return val;
  }
}

// ---------------------------------------------------------
// DocField component
// ---------------------------------------------------------

function DocField(props) {
  return (<div>
    <label>{props.key}: </label>
    <label>{formatForDisplay({ value: props.doc[props.key] })}: </label>
    <input type='submit' value='edit' />
  </div>
  );
}

// ---------------------------------------------------------
// DocForm component
// ---------------------------------------------------------

function DocForm(props) {
  var doc = props.doc;
  var doc_keys = Object.keys(doc).reverse();
  var docFields = doc_keys.map((k) =>
    <div key={k}>
    <label>{k}:</label>
    </div>
  );

  return (
    <form>
      {docFields}
      <input type='submit' value='Edit' />
    </form>
  );
}

// ---------------------------------------------------------
// DocEntry component
// ---------------------------------------------------------

function DocEntry(props) {
  var entry = props.entry;
  var entry_doc = entry.doc;
  var doc_keys = Object.keys(entry_doc);

  return (<li className="Entry">
    <DocForm doc={entry_doc} />
  </li>);
}

// ---------------------------------------------------------
// DocList component
// ---------------------------------------------------------

function DocList(props) {
  const entries = props.documents;
  const docEntries = entries.map((entry) =>
    <DocEntry
      key={entry.key}
      entry={entry}
    />);

  return (
    <div className="DocList">
      <ul>
        {docEntries}
      </ul>
    </div>
  );

}

// ---------------------------------------------------------
// App component
// ---------------------------------------------------------

class App extends Component {
  state = {
    allDocs: [],
    localDB: this.props.localPouchDB,
    remoteDB: this.props.remoteCouchDB
  }

  componentDidMount() {
    syncWithRemoteDB({
      localDB: this.state.localDB,
      remoteDB: this.state.remoteDB
    });
    refreshAllDocs({
      app: this,
      localDB: this.state.localDB
    });
  }

  render() {
    return (
      <div className="App">
        <style>{css}</style>
        <h1>Opps Daily</h1>
        <h3>document count: {this.state.allDocs.length}</h3>
        <DocList documents={this.state.allDocs} />
      </div>
    );
  }

}

// ---------------------------------------------------------
// main export
// ---------------------------------------------------------

export default App;
