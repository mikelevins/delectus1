import React from 'react';

import {
    Table,
    TableBody,
    TableHeader,
    TableHeaderColumn,
    TableRow,
    TableRowColumn,
} from 'material-ui/Table';

import './ListBox.css';

const ListBox = (props) => (
    <Table>
        <TableHeader>
            <TableRow>
                {props.columns.map(function (field) {
                    return <TableHeaderColumn> {field} </TableHeaderColumn>
                })}
            </TableRow>
        </TableHeader>
        <TableBody>
            {props.rows.map(function (row) {
                return (
                    <TableRow>
                        {row.map(function (item) {
                            return (<TableRowColumn>{item}</TableRowColumn>)
                        })}
                    </TableRow>)
            })}
        </TableBody>
    </Table>
);

export default ListBox;

