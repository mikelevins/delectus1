import React, { Component } from 'react';
import './App.css';


const styles = {
  propertyName: {
    fontWeight: 'bold',
  },
  object: {
    border: '1px solid black',
  },
};

class PresenterObjectTable extends Component {

  // constructor
  // ---------------------------------------------------------

  constructor(props) {
    super(props);

    this.state = {
    };
  }

  // main render
  // ---------------------------------------------------------

  render() {
    const presenter = this;
    const object = presenter.props.object;
    const object_properties = Object.keys(object);
    const property_rows = object_properties.map(
      (prop) => {
        const val = object[prop];

        return (
          <tr key={prop}>
            <td style={styles.propertyName}>{prop + ':'}</td>
            <td>{typeof val === 'object' ? (
              <PresenterObjectTable object={val} />
            ) : (
                String(val)
              )}</td>
          </tr>
        );
      }
    );

    return (
      <React.Fragment>
        <table style={styles.object}>
          <tbody>
            {property_rows}
          </tbody>
        </table>
      </React.Fragment>
    );
  }
}

// exports
// ---------------------------------------------------------

export default PresenterObjectTable;