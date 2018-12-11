import React, { Component } from 'react';
import './App.css';

// ---------------------------------------------------------
// helper functions
// ---------------------------------------------------------

// ---------------------------------------------------------
// App component
// ---------------------------------------------------------

class App extends Component {
  state = {
  }

  componentDidMount() {
    document.title = "Delectus"
  }

  render() {
    return (
      <div className="App">
        <h1>Delectus</h1>
      </div>
    );
  }

}

// ---------------------------------------------------------
// main export
// ---------------------------------------------------------

export default App;
