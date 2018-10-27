import React, { Component } from 'react';
import './App.css';

const css = `
.Entry { border: 1px solid; }
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

  makeEntry(row) {
    return <tr key={row.id}>
      <td align={'left'}>received: {row.doc.date_received}</td>
      <td align={'left'}>id: {row.id}</td>
    </tr>
  }

  renderDoc(item) {
    return (
      <tr key={item.key}>
        <td>
          <table className="Entry">
            <tbody>
            <tr>
                <td colSpan="2"  align={'left'}> Summary: &nbsp; {item.doc.summary}</td>
              </tr>
              <tr>
                <td align={'left'}>Received: &nbsp; {item.doc.date_received}</td>
                <td align={'left'}>ID: &nbsp; {item.id}</td>
              </tr>
              <tr>
                <td align={'left'}>Industry: &nbsp; {item.doc.industry}</td>
                <td align={'left'}>Job function: &nbsp; {item.doc.job_function}</td>
              </tr>
              <tr>
                <td align={'left'}>Will pay: &nbsp; {item.doc.willing_to_pay}</td>
                <td align={'left'}>per: &nbsp; {item.doc.pay_per_unit}</td>
              </tr>
              <tr>
                <td align={'left'} colSpan="2">{item.doc.content.substring(82, 384) + '...'}</td>
              </tr>
            </tbody>
          </table>
        </td>
      </tr>);
  }

  renderDocs() {
    return (
      <div className="OppsDaily">
        <table align="center" cellPadding="10px">
          <tbody>
            {this.state.allDocs.map(entry => this.renderDoc(entry))}
          </tbody>
        </table>
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
