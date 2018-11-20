import React, { Component } from 'react';
import './App.css';
import List from '@material-ui/core/List';
import ListItem from '@material-ui/core/ListItem';
import ListItemText from '@material-ui/core/ListItemText';

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


class BrowserDocumentsPane extends Component {

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
        const paneTitle = app.state.selectedDatabase + ' documents';
        const doclist = app.state.selectedDocuments;
        var docRows = (doclist && doclist.length > 0) ? (doclist.map(this.makeListItem)) : (emptyList);

        if (doclist && doclist.length > 0) {
            return (
                <div>
                    <p style={styles.browserPaneTitle}>{paneTitle}</p>
                    <List component="nav" style={styles.browserPane}>{docRows}</List>
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

