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
                {props.columns.map(function (field, index) {
                    return (
                        <TableHeaderColumn key={index}>
                            {field}
                        </TableHeaderColumn>
                    )
                })}
            </TableRow>
        </TableHeader>
        <TableBody>
            {props.rows.map(function (row, index) {
                return (
                    <TableRow key={index}>
                        {row.map(function (item, index) {
                            return (
                                <TableRowColumn key={index}>
                                    {item}
                                </TableRowColumn>
                            )
                        })}
                    </TableRow>)
            })}
        </TableBody>
    </Table>
);

export default ListBox;

