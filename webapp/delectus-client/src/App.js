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
  margin: .25em; 
}

.FormLabel {
  font-weight: bold;
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

// the 'content' field is typically very long, long enough
// to affect display performance. here we shorten very long
// content to speed things up.
// 
// BUG: this function also strips off the first 82 characters in 
// order to remove an inconvenient header from the content field.
// it makes the bad assumption that a field whose content is long 
// enough to be shortened is necessarily the content field, and 
// that it therefore contains this header we want to strip.
// If some other field happens to be very long, then it will
// get truncated inconveniently.
// We should fix this to process these fields more intelligently.
function formatForDisplay(props) {
  const val = props.value;
  if (val.length > 512) {
    return val.substring(82, 320) + '...';
  } else {
    return val;
  }
}

function editDoc() {
  alert('Soon this will be the document editor');
}

// ---------------------------------------------------------
// DocField component
// ---------------------------------------------------------

function DocField(props) {
  return (
    <p className="EntryField">
      <span className="FormLabel">{props.docKey}:</span>
      &nbsp;
      <span>{formatForDisplay({ value: props.doc[props.docKey] })}</span>
    </p>
  );
}

// ---------------------------------------------------------
// DocEntry component
// ---------------------------------------------------------

function DocEntry(props) {
  var entry = props.entry;
  var entry_doc = entry.doc;
  var doc_keys = Object.keys(entry_doc).reverse();
  var doc_fields = doc_keys.map((k) =>
    <DocField
      key={k}
      docKey={k}
      doc={entry_doc} />);

  return (<li>
    <div className="Entry">
      {doc_fields}
      <button onClick={editDoc}>
        Edit
      </button>
    </div>
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
