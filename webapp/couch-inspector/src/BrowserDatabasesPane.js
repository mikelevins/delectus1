import React, { Component } from 'react';
import './App.css';
import List from '@material-ui/core/List';
import ListItem from '@material-ui/core/ListItem';
import ListItemText from '@material-ui/core/ListItemText';

// BrowserDatabasesPane styles
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
        textTransform: 'none'
    }
};

// BrowserDatabasesPane class
// ---------------------------------------------------------

class BrowserDatabasesPane extends Component {

    setSelectedItem = (itemName) => {
        const app = this.props.app;
        app.updateSelectedDatabase(itemName);
    }

    makeListItem = (item) => {
        const pane = this;
        const app = this.props.app;
        const isSelected = item === app.getSelectedDatabase();

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
        const app = this.props.app;
        const dblist = app.getDatabases();
        var dbRows = (dblist) ? (dblist.map(this.makeListItem)) : ([]);

        if (dblist && dblist.length > 0) {
            return (
                <div>
                    <p style={styles.browserPaneTitle}>Databases</p>
                    <List style={styles.browserPane} component="nav">{dbRows}</List>
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
