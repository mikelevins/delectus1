import React, { Component } from 'react';
import './App.css';

class App extends Component {
  state = {
    users: [],
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
      console.log("yay, syncing with remote CouchDB")
    }).on('error', function (err) {
      // boo, we hit an error!
      console.log("boo, sync failed")
    });
  }

  getAllDocs(){
    var app = this;
    this.state.localDB.allDocs(
      {include_docs: true, descending: true}, 
      function(err, result) {
        if (err) {
          console.log("Error getting documents: ");  
          console.log(err);  
        } else {
          app.setState({docs: result.rows});
          console.log("row count: "+ result.rows.length);  
        }
      }
      );
      return app.docs;
  }

  render() {
    this.getAllDocs();
    return (
      <div className="App">
        <h1>Delectus Users</h1>
        {this.state.users.map(user =>
          <div key={user.id}>{user.username}</div>
        )}

        <h1>Opps Daily</h1>
        <div>docs: {String(this.state.docs)}</div>
      </div>
    );
  }


}

export default App;
