import React, { Component } from 'react';
import './App.css';
import List from '@material-ui/core/List';
import ListItem from '@material-ui/core/ListItem';
import ListItemText from '@material-ui/core/ListItemText';

const styles = {
    browserPane: {
        border: '1px solid black',
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

        app.setState({selectedDatabase: itemName});
    }

    makeListItem = (item) => {
        const pane = this;
        const app = pane.props.app;

        return (
            <ListItem button onClick={(event) => pane.setSelectedItem(item)} >
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

