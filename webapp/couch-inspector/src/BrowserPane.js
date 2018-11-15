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

    makeBrowserRow = (item) => <tr><td>{item}</td></tr>;

    // main render
    // ---------------------------------------------------------

    render() {
        const emptyList = [];
        const pane = this;
        const list = pane.props.list;
        var listRows = (list) ? (list.map(this.makeBrowserRow)) : (emptyList);

        if (list && list.length > 0) {
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
        } else {
            return (<div></div>);
        }

    }
}

// exports
// ---------------------------------------------------------

export default BrowserPane;