import React, { Component } from 'react';
import './App.css';
import DocCompactField from './DocCompactField.js';

// ---------------------------------------------------------
// DocEntry component
// ---------------------------------------------------------
// Displays an individual document entry

class DocEntry extends Component {
    render() {
        const app = this.props.app;
        const entry = this.props.entry;
        const entry_id = entry.id;
        const doc = entry.doc;

        return (
            <div className="DocEntry">
                <table className="DocEntryTable">
                    <tbody>
                        <tr>
                            <td className="DocEntryCell" colSpan="3">
                                <span className='bold'>Summary:</span> &nbsp;
                            </td>
                        </tr>
                        <tr>
                            <td className="DocEntryButtonCell">
                                <button
                                    className="DocEntryButton"
                                    onClick={app.editSelectedDocument}>
                                    Edit
                                </button>
                            </td>
                            <td className="DocEntryCell">
                                <span className='bold'>date:</span>
                                {doc.date_received}, &nbsp;
                            </td>
                            <td className="DocEntryCell">
                                <span className='bold'>id: </span>
                                {entry_id}
                            </td>
                        </tr>
                    </tbody>
                </table>
            </div>
        );
    }
}

export default DocEntry;
