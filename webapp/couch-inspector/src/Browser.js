import React, { Component } from 'react';
import './App.css';

const styles = {
    browser: {
        border: '1px solid black',
    },
};

class Browser extends Component {

    // constructor
    // ---------------------------------------------------------

    constructor(props) {
        super(props);

        this.state = {
        };
    }

    // main render
    // ---------------------------------------------------------

    render() {
        const browser = this;
        const leftPaneTitle = browser.props.leftPaneTitle;
        const leftPaneList = browser.props.leftPaneList;

        return (
            <React.Fragment>
                <table style={styles.browser}>
                    <tbody>
                        <tr>
                            <td>{leftPaneList}</td>
                            <td></td>
                            <td></td>
                        </tr>
                    </tbody>
                </table>
            </React.Fragment>
        );
    }
}

// exports
// ---------------------------------------------------------

export default Browser;