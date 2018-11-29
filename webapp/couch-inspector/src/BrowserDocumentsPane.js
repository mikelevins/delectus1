import React, { Component } from 'react';
import './App.css';

import Button from '@material-ui/core/Button';
import List from '@material-ui/core/List';
import ListItem from '@material-ui/core/ListItem';
import ListItemText from '@material-ui/core/ListItemText';

// BrowserDocumentsPane styles
// ---------------------------------------------------------

const styles = {
    browserPane: {
        border: '1px solid black',
        height: '24rem',
        overflow: 'auto',
        padding: '6px',
        width: '100%',
    },
    browserPaneTitle: {
        fontWeight: 'bold',
        height: '1rem',
    },
    button: {
        marginRight: '1rem',
        marginTop: '0.5rem',
        textTransform: 'none'
    },
    controls: { textAlign: 'center' }
};

// BrowserDocumentsPane auxiliary components
// ---------------------------------------------------------

function PreviousButton(props) {
    const pane = props.pane;
    return (
            <Button
                style={styles.button}
                variant="contained"
                color="primary"
                onClick={pane.handlePagePrevious}>
                Previous
        </Button>
    );
}

function NextButton(props) {
    const pane = props.pane;
    return (
            <Button
                style={styles.button}
                variant="contained"
                color="primary"
                onClick={pane.handlePageNext}>
                Next
        </Button>
    );
}

// BrowserDocumentsPane class
// ---------------------------------------------------------

class BrowserDocumentsPane extends Component {

    handlePagePrevious = () => {
        const pane = this;
        const app = pane.props.app;
        app.updatePreviousDatabasePage();
    }

    handlePageNext = () => {
        const pane = this;
        const app = pane.props.app;
        app.updateNextDatabasePage();
    }

    setSelectedItem = (itemID) => {
        const app = this.props.app;
        app.updateSelectedDocument(itemID);
    }

    makeListItem = (item) => {
        const itemID = item.id;
        const pane = this;
        const app = this.props.app;
        const isSelected = itemID === app.getSelectedDocument();

        return (
            <ListItem
                key={itemID}
                button={true}
                selected={isSelected}
                onClick={(event) => { pane.setSelectedItem(itemID) }} >
                < ListItemText primary={itemID} />
            </ListItem>
        )
    };

    makeBrowserRow = (item) => <tr><td>{this.makeItemSelector(item)}</td></tr>;

    // main render
    // ---------------------------------------------------------

    render() {
        const pane = this;
        const app = pane.props.app;
        const selectedDB = app.getSelectedDatabase();
        const paneTitle = selectedDB;
        const doclist = app.state.selectedDBDocuments;
        var docRows = (doclist && doclist.length > 0) ? (doclist.map(this.makeListItem)) : ([]);

        if (doclist && doclist.length > 0) {
            return (
                <div>
                    <div>
                        <p style={styles.browserPaneTitle}>{paneTitle}</p>
                        <List component="nav" style={styles.browserPane}>{docRows}</List>
                    </div>
                    <div style={styles.controls}>
                        <PreviousButton pane={this} />
                        <NextButton pane={this} />
                    </div>
                </div>
            );
        } else {
            return (<div></div>);
        }
    }
}

// exports
// ---------------------------------------------------------

export default BrowserDocumentsPane;

