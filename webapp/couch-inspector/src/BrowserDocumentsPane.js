import React, { Component } from 'react';
import './App.css';

import Button from '@material-ui/core/Button';
import List from '@material-ui/core/List';
import ListItem from '@material-ui/core/ListItem';
import ListItemText from '@material-ui/core/ListItemText';

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
    }
};

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

class BrowserDocumentsPane extends Component {

    handlePagePrevious = () => {
        const pane = this;
        const app = pane.props.app;
        const pageLimit = app.state.databasesPerPage;
        const pageOffset = app.state.databasesPageOffset;
        var newOffset = pageOffset-pageLimit;

        if (newOffset < 0) { newOffset = 0; }

        app.setState({databasesPageOffset: newOffset});
    }

    handlePageNext = () => {
        const pane = this;
        const app = pane.props.app;
        const pageLimit = app.state.databasesPerPage;
        const pageOffset = app.state.databasesPageOffset;
        var newOffset = pageOffset+pageLimit;

        app.setState({databasesPageOffset: newOffset});
    }

    makeListItem = (item) => {
        const itemID = item.id;

        return (
            <ListItem
                key={itemID}
                button={true}  >
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
        const emptyList = [];
        const selectedDB = app.getSelectedDatabase();
        const paneTitle = '\'' + selectedDB + '\' documents';
        const doclist = app.state.selectedDocuments;
        var docRows = (doclist && doclist.length > 0) ? (doclist.map(this.makeListItem)) : (emptyList);

        if (doclist && doclist.length > 0) {
            return (
                <div>
                    <div>
                        <p style={styles.browserPaneTitle}>{paneTitle}</p>
                        <List component="nav" style={styles.browserPane}>{docRows}</List>
                    </div>
                    <div>
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

