import React from 'react';

import {
    Table,
    TableBody,
    TableHeader,
    TableHeaderColumn,
    TableRow,
    TableRowColumn,
} from 'material-ui/Table';

import BottomBar from './BottomBar';
import './ListBox.css';

const tableStyle = {
    display: "block",
    overflowX: "auto",
    whiteSpace: "nowrap",
};

const tableHeaderStyle = {
    fontSize: "1rem",
    fontWeight: "bold",
};

const tableRowStyle = {
    fontSize: "1rem",
    fontWeight: "normal,"
};

const ListBox = (props) => (
    <div>
        <Table
            style={tableStyle}
            multiSelectable={true}
            fixedHeader={false}
        >
            <TableHeader>
                <TableRow>
                    {props.list.columns.map(function (field, index) {
                        return (
                            <TableHeaderColumn
                                style={tableHeaderStyle}
                                key={index}
                            >
                                {field}
                            </TableHeaderColumn>
                        )
                    })}
                </TableRow>
            </TableHeader>

            <TableBody stripedRows={true}>
                {props.list.rows.map(function (row, index) {
                    return (
                        <TableRow key={index}>
                            {row.fields.map(function (item, index) {
                                return (
                                    <TableRowColumn
                                        style={tableRowStyle}
                                        key={index}
                                    >
                                        {item}
                                    </TableRowColumn>
                                )
                            })}
                        </TableRow>)
                })}
            </TableBody>
        </Table>

        <BottomBar />
    </div>
);

export default ListBox;

