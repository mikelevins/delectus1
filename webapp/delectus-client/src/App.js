import React, { Component } from 'react';
import './App.css';

class App extends Component {
  state = {
    users: [],
    localDB: this.props.localDB,
    remoteDB: this.props.remoteDB,
    remoteDBInfo: this.props.remoteDB.info()
  }

  componentDidMount() {
    fetch('/users')
      .then(res => res.json())
      .then(users => this.setState({ users }));
  }

  syncWithRemoteDB() {
    this.state.localDB.sync(this.state.remoteDB).on('complete', function () {
      // yay, we're in sync!
      console.log("yay, we are syncing")
    }).on('error', function (err) {
      // boo, we hit an error!
      console.log("boo, we are NOT syncing")
    });
  }

  render() {
    this.syncWithRemoteDB();
    return (
      <div className="App">
        <h1>Delectus Users</h1>
        {this.state.users.map(user =>
          <div key={user.id}>{user.username}</div>
        )}

        <h1>Delectus Databases</h1>
        <div>localDB: {this.state.localDB.toString()}</div>
        <div> &nbsp; </div>
        <div>remoteDB: {this.state.remoteDBInfo.toString()}</div>

      </div>
    );
  }


}

export default App;
