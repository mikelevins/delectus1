import React, { Component } from 'react';
import './App.css';

class App extends Component {
  state = {
    users: [],
    localDB: this.props.localDB,
    remoteDB: this.props.remoteDB
  }

  componentDidMount() {
    fetch('/users')
      .then(res => res.json())
      .then(users => this.setState({ users }));
  }

  render() {
    return (
      <div className="App">
        <h1>Delectus Users</h1>
        {this.state.users.map(user =>
          <div key={user.id}>{user.username}</div>
        )}

        <h1>Delectus Databases</h1>
        <div>localDB</div>
        <div>remoteDB</div>
        
      </div>
    );
  }


}

export default App;
