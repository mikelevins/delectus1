import React, { Component } from 'react';
import './App.css';

const css = `
.Entry { 
  border: 1px solid black;
  border-collapse: collapse;
  margin: 2em; 
  text-align: left;
}

.EntryField { 
  border: 1px solid black;
  border-collapse: collapse;
  padding: 0.5em; 
}
`;

class App extends Component {
  state = {
    users: [],
    allDocs: [],
    localDB: this.props.localPouchDB,
    remoteDB: this.props.remoteCouchDB
  }

  componentDidMount() {
    fetch('/users')
      .then(res => res.json())
      .then(users => this.setState({ users }));
    this.syncWithRemoteDB();
  }

  syncWithRemoteDB() {
    this.state.localDB.sync(this.state.remoteDB).on('complete', function () {
      // yay, we're in sync!
      console.log("syncing with remote CouchDB")
    }).on('error', function (err) {
      // boo, we hit an error!
      console.log("ERROR: sync failed")
    });
  }

  getAllDocs() {
    var app = this;
    this.state.localDB.allDocs(
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
  }

  formatFieldForDisplay(fName, fVal){
    var contentBanner = ' ** The Opportunity ------------------------------------------------------------  ';
    if (fName == 'content') {
      return (fVal.substring(contentBanner.length,320)+'...');
    } else {
      return (fVal);
    }
  }

  renderDoc(obj) {
    var doc = obj.doc;
    return (
      <table className="Entry">
        <tbody>
          {Object.keys(doc).reverse().map(k => {
            var val = doc[k];
            var valstr = this.formatFieldForDisplay(k,String(val));
            return (
              <tr><td className="EntryField"><b>{k}:</b> {valstr}</td></tr>
            );
          })}
        </tbody>
      </table>
    );
  }

  renderDocs() {
    return (
      <div className="OppsDaily">
        {this.state.allDocs.map(doc => this.renderDoc(doc))}
      </div>
    );
  }

  render() {
    this.getAllDocs();
    return (
      <div className="App">
        <style>{css}</style>
        <h1>Opps Daily</h1>
        {this.renderDocs()}}
      </div>
    );
  }


}

export default App;
