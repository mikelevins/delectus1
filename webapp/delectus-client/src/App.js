import React, { Component } from 'react';
import './App.css';

// ---------------------------------------------------------
// Styles for the components (included by the App)
// ---------------------------------------------------------

const css = `

.Button {
  font-size: 11pt;
}

.Cell {
  display: table-cell;
  font-size: 12pt;
  padding: 4px 8px;
  text-align: left;
  width: 85%;
}

.Editor {
  background-color: #eeeeee;
  border: 1px solid black;
  display: table;
  height: 80%;
  left: 15%;
  padding: 1em;
  position: fixed;
  text-align: left;
  top: 10%;
  width: 70%;
  z-index: 1000;
}

.Entry {
  border: 1px solid black;
  margin: 1em;
  padding: 1em;
}

.Label {
  display: table-cell;
  font-weight: bold;
  text-align: right;
  width: 15%;
}

.Row { 
  display: table-row;
}

.Table {
  display: table;
  width: 100%%;
}

.TextAreaCell {
  display: table-cell;
  height: 12em;
  font-size: 12pt;
  padding: 4px 8px;
  text-align: left;
  width: 85%;
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

function showEditor(props) {
  props.app.setState({ showEditor: true });
  props.app.setState({ selectedDoc: props.doc });
}

function hideEditor(props) {
  props.app.setState({ showEditor: false })
}

// ---------------------------------------------------------
// EditorField component
// ---------------------------------------------------------

function EditorField(props) {
  var labelText = props.docKey;
  var valueText = props.doc[props.docKey];

  if (props.docKey === "content") {
    return (
      <div className="Row">
        <div className="Label">
          <label>{labelText}:</label>
        </div>
        <div className="EntryField">
          <textarea
            className="TextAreaCell"
            name={props.docKey}
            defaultValue={valueText} />
        </div>
      </div>);
  } else {
    return (
      <div className="Row">
        <div className="Label">
          <label>{labelText}:</label>
        </div>
        <div>
          <input
            className="Cell"
            name={props.docKey}
            type="text" defaultValue={valueText} />
        </div>
      </div>
    );
  }
}

// ---------------------------------------------------------
// DocCompactField component
// ---------------------------------------------------------

function DocCompactField(props) {
  return (
    <div className="Row">
      <div className="Label">{
        props.docKey}:
        </div>
      <div className="Cell">
        {formatForDisplay({ value: props.doc[props.docKey] })}
      </div>
    </div>
  );
}

// ---------------------------------------------------------
// DocEditor component
// ---------------------------------------------------------

function DocEditor(props) {
  var app = props.app;
  var doc = props.doc
  var doc_keys = Object.keys(doc).reverse();
  var doc_fields = doc_keys.map((k) =>
    <EditorField
      key={k}
      docKey={k}
      doc={doc} />);

  return (
    <div className="Editor">
      {doc_fields}
      <button className="Button" onClick={() => { hideEditor({ app: app }) }}>Close</button>
    </div>
  );
}


// ---------------------------------------------------------
// DocEntry component
// ---------------------------------------------------------

function DocEntry(props) {
  var app = props.app;
  var entry = props.entry;
  var entry_doc = entry.doc;
  var doc_keys = Object.keys(entry_doc).reverse();
  var doc_fields = doc_keys.map((k) =>
    <DocCompactField
      key={k}
      docKey={k}
      doc={entry_doc} />);

  return (
    <div className="Entry">
      {doc_fields}
      <div className="Row">
        <div className="EntryField">
          <button className="Button" onClick={() => {
            showEditor({ app: app, doc: entry_doc })
          }
          }>
            Edit
          </button>
        </div>
      </div>
    </div>
  );
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
      app={props.app}
    />);

  return (
    <div className="Table">
      {docEntries}
    </div>
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
