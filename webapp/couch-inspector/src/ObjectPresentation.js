import React, { Component } from 'react';
import './App.css';

import CssBaseline from '@material-ui/core/CssBaseline';

const styles = {
    objectKey: {
        fontWeight: 'bold',
    },
    objectTable: {
        marginLeft: '2rem',
        marginRight: '4rem',
    },
    objectValue: {
        border: '1px solid black',
    },
};

function ObjectKeyCell(props) {
    return (
        <td style={styles.objectKey}>
            {props.propertyName}:
        </td>
    );
};

function ObjectValueCell(props) {
    const val = props.propertyValue;
    const valType = typeof val;
    if (valType === 'object') {
        return (
            <td style={styles.objectValue}>
                <ObjectTable object={val} />
            </td>
        );
    } else {
        return (
            <td style={styles.objectValue}>
                {String(val)} />
            </td>
        );
    }
};

function ObjectRow(props) {
    const value = props.object;
    const key = props.property;
    return (
        <tr key={key}>
            <ObjectKeyCell propertyName={key} />
            <ObjectValueCell propertyValue={value[key]} />
        </tr>
    );
}

function ObjectTable(props) {
    const value = props.object;
    const objectRows = Object.keys(value).map(
        (key) => { return (<ObjectRow key={key} property={key} object={value} />) }
    );

    return (
        <table style={styles.objectTable}>
            <tbody>
                {objectRows}
            </tbody>
        </table >
    );
}

class ObjectPresentation extends Component {

    // main render
    // ---------------------------------------------------------

    render() {
        const { classes } = this.props;
        const value = this.props.object;

        return (
            <React.Fragment>
                <CssBaseline />
                <ObjectTable object={value} />
            </React.Fragment>
        );
    }
}

// exports
// ---------------------------------------------------------

export default ObjectPresentation;