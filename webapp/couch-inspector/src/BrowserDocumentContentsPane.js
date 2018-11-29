import React, { Component } from 'react';
import './App.css';
import Button from '@material-ui/core/Button';

const styles = {
    browserPane: {
        border: '1px solid black',
        height: '20rem',
        overflow: 'auto',
        padding: '6px',
        width: '100%',
    },
    browserPaneTitle: {
        fontWeight: 'bold',
        height: '1rem',
    },
    button: {
        textTransform: 'none'
    }
};


class BrowserDocumentContentsPane extends Component {

    makeItemSelector = (item) =>
        <Button
            style={styles.button}
            variant='text' >
            {item}
        </Button>;

    makeBrowserRow = (item) => <tr><td>{this.makeItemSelector(item)}</td></tr>;

    // main render
    // ---------------------------------------------------------

    render() {
        const pane = this;
        const doc = pane.props.document;
        const paneTitle = doc;

        return (
            <div>
                <p style={styles.browserPaneTitle}>{paneTitle}</p>
            </div>
        );


    }
}

// exports
// ---------------------------------------------------------

export default BrowserDocumentContentsPane;

