import React, { Component } from 'react';
import './App.css';

const styles = {
    presenterTitle: {
        fontWeight: 'bold',
    },
    propertyName: {
        fontWeight: 'bold',
    },
    object: {
        border: '1px solid black',
    },
};

class Presenter extends Component {

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
                        <td>
                            {(typeof val === 'object') ? (
                                <Presenter object={val} />
                            ) : (String(val))
                            }
                        </td>
                    </tr>
                )
            });

        return (
            <React.Fragment>
                <p style={styles.presenterTitle}>Databases</p>
                <table style={styles.object}>
                    <tbody>{property_rows}</tbody>
                </table>
            </React.Fragment>
        );
    }
}

// exports
// ---------------------------------------------------------

export default Presenter;