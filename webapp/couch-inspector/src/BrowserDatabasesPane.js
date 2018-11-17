import React, { Component } from 'react';
import './App.css';
import List from '@material-ui/core/List';
import ListItem from '@material-ui/core/ListItem';
import ListItemText from '@material-ui/core/ListItemText';

import axios from 'axios';

const styles = {
    browserPane: {
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


class BrowserDatabasesPane extends Component {

    setSelectedItem = (itemName) => {
        const pane = this;
        const app = pane.props.app;
        const couchURL = app.state.couchURL;
        const docsRequest = '/' + itemName + '/_all_docs';

        console.log(couchURL + docsRequest);
                
        axios.get(couchURL + docsRequest)
            .then(response => app.setState({
                selectedDatabase: itemName,
                selectedDocuments: response.data.rows
            }));
    }

    makeListItem = (item) => {
        const pane = this;
        const app = pane.props.app;
        const isSelected = item === app.state.selectedDatabase;

        return (
            <ListItem
                key={item}
                button={true}
                selected={isSelected}
                onClick={(event) => { pane.setSelectedItem(item) }} >
                < ListItemText primary={item} />
            </ListItem>
        )
    };

    // main render
    // ---------------------------------------------------------

    render() {
        const pane = this;
        const app = pane.props.app;
        const emptyList = [];
        const dblist = app.state.databases;
        var dbRows = (dblist) ? (dblist.map(this.makeListItem)) : (emptyList);

        if (dblist && dblist.length > 0) {
            return (
                <div>
                    <p style={styles.browserPaneTitle}>Databases</p>
                    <List component="nav">{dbRows}</List>
                </div>
            );
        } else {
            return (<div></div>);
        }

    }
}

// exports
// ---------------------------------------------------------

export default BrowserDatabasesPane;

