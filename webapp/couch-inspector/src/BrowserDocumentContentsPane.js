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
            variant='text'
        >
            {item}
        </Button>;

    makeBrowserRow = (item) => <tr><td>{this.makeItemSelector(item)}</td></tr>;

    // main render
    // ---------------------------------------------------------

    render() {
        const pane = this;
        const app = pane.props.app;
        const selectedDoc = app.getSelectedDocument();
        const paneTitle = selectedDoc;
        const list = pane.props.list;
        var listRows = (list) ? (list.map(this.makeBrowserRow)) : ([]);

        if (list && list.length > 0) {
            return (
                <div>
                    <p style={styles.browserPaneTitle}>{paneTitle}</p>
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

export default BrowserDocumentContentsPane;

