import React, { Component } from 'react';
import './App.css';

const styles = {
    browserPane: {
        border: '1px solid black',
        padding: '6px',
        width: '100%',
    },
    browserPaneTitle: {
        fontWeight: 'bold',
        height: '1rem',
    }
};

class BrowserPane extends Component {

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
        const pane = this;
        const list = pane.props.list;
        var listRows = [];

        if (list) {
            listRows = list.map(
                (item) => <tr><td>{item}</td></tr>
            );
        }

        return (
            <div>
                <p style={styles.browserPaneTitle}>{pane.props.title}</p>
                <table style={styles.browserPane}>
                    <tbody>
                        {listRows}
                    </tbody>
                </table>
            </div>
        );
    }
}

// exports
// ---------------------------------------------------------

export default BrowserPane;