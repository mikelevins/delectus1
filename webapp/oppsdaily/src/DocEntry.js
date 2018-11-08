import React, { Component } from 'react';
import './App.css';

// ---------------------------------------------------------
// DocEntryCell component
// ---------------------------------------------------------
// Displays one cell of a document entry

class DocEntryCell extends Component {
    render() {
        return (
            <td className="Cell" colSpan={this.props.colSpan}>
                <span className="bold">{this.props.label}:</span>&nbsp;
                {this.props.value}
            </td>
        );
    }
}

// ---------------------------------------------------------
// DocEntryButtonCell component
// ---------------------------------------------------------
// Displays one cell of a document entry

class DocEntryButtonCell extends Component {
    render() {
        return (
            <td className="Cell" colSpan={this.props.colSpan}>
                <button
                    className="Button"
                    onClick={this.props.onClick}>
                    {this.props.label}
                </button>
            </td>
        );
    }
}

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
        const summary = entry.summary;

        return (
            <div className="DocEntry">
                <table className="DocEntryTable">
                    <tbody>
                        <tr>
                            <DocEntryCell
                                colSpan="3"
                                label="Summary"
                                value={summary}
                            />
                        </tr>
                        <tr>
                            <DocEntryButtonCell
                                colSpan="1"
                                label="Edit"
                                onClick={() => app.editSelectedDocument(doc)}
                            />
                            <DocEntryCell
                                colSpan="1"
                                label="date received"
                                value={doc.date_received}
                            />
                            <DocEntryCell
                                colSpan="1"
                                label="id"
                                value={entry_id}
                            />
                        </tr>
                    </tbody>
                </table>
            </div>
        );
    }
}

export default DocEntry;
