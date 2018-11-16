import React, { Component } from 'react';
import './App.css';
import Button from '@material-ui/core/Button';

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

    makeItemSelector = (item) => {
        const pane = this;
        const app = pane.props.app;

        return (
            <Button
                style={styles.button}
                variant='text'
                onClick={(event) => app.setState({selectedDatabase: item})} >
                {item}
            </Button>
        )
    };

    makeBrowserRow = (item) => {
        return (
            <tr>
                <td>{this.makeItemSelector(item)}</td>
            </tr>
        )
    };

    // main render
    // ---------------------------------------------------------

    render() {
        const pane = this;
        const app = pane.props.app;
        const emptyList = [];
        const dblist = app.state.databases;
        var dbRows = (dblist) ? (dblist.map(this.makeBrowserRow)) : (emptyList);

        if (dblist && dblist.length > 0) {
            return (
                <div>
                    <p style={styles.browserPaneTitle}>Databases</p>
                    <table style={styles.browserPane}>
                        <tbody>{dbRows}</tbody>
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

export default BrowserDatabasesPane;

