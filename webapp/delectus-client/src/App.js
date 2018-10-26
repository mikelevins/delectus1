import React, { Component } from 'react';
import './App.css';

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

  renderUsers() {
    return (<div className="Usernames">
      {this.state.users.map(user =>
        <div key={user.id}>{user.username}</div>
      )}
    </div>);
  }

  renderDoc(item) {
    return (
      <tr key={item.key}>
        <td>
          <table border="1px">
            <tbody>
            <tr>
                <td>Summary: &nbsp; {item.doc.summary}</td>
                <td>Received: &nbsp; {item.doc.date_received}</td>
                <td>ID: &nbsp; {item.id}</td>
              </tr>
              <tr>
                <td colSpan="3">{item.doc.content.substring(82,172)+'...'}</td>
              </tr>
            </tbody>
          </table>
        </td>
      </tr>);
  }

  renderDocs() {
    return (
      <div className="OppsDaily">
        <table align="center" cellPadding="8px">
          <tbody>
            {this.state.allDocs.map(entry => this.renderDoc(entry))}
          </tbody>
        </table>
      </div>
    );
  }

  render() {
    var app = this;
    this.getAllDocs();
    return (
      <div className="App">
        <h1>Delectus Users</h1>
        {this.renderUsers()}

        <h1>Opps Daily</h1>
        {this.renderDocs()}}
      </div>
    );
  }


}

export default App;
