import React, { Component } from 'react';
import './App.css';

import axios from 'axios';

class App extends Component {
  componentDidMount() {
    document.title = "Couch Inspector"
  } // end componentDidMount

  handleClick() {
    axios.get('http://mars.local:5984')
      .then(response => console.log(response));
    console.log('Sucess!');
  }

  render() {
    const app = this;

    return (
      <div className='button__container'>
        <button className='button' onClick={app.handleClick}>
          Click Me
          </button>
      </div>
    );
  }
}

export default App;
