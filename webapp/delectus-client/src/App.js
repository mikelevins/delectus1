import React, { Component } from 'react';
import './App.css';

class App extends Component {
  state = {
    users: [],
    localDB: this.props.localPouchDB,
    remoteDB: this.props.remoteCouchDB
  }

  docs = ["nothing"];

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

  render() {

    return (
      <div className="App">
        <h1>Delectus Users</h1>
        {this.state.users.map(user =>
          <div key={user.id}>{user.username}</div>
        )}

        <h1>Opps Daily</h1>
      </div>
    );
  }


}

export default App;
