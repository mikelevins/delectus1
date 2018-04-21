import React from 'react';

import {
    Table,
    TableBody,
    TableHeader,
    TableHeaderColumn,
    TableRow,
    TableRowColumn,
} from 'material-ui/Table';

import {
    Toolbar, ToolbarGroup, ToolbarSeparator, ToolbarTitle
} from 'material-ui/Toolbar';
import IconButton from 'material-ui/IconButton';
import AddColumn from 'material-ui/svg-icons/action/view-column';


import BottomBar from './BottomBar';
import './ListBox.css';


const ListBox = (props) => (
    <div>
        <Toolbar>
            <ToolbarTitle text={props.title}/>
            <IconButton><AddColumn /></IconButton>
        </Toolbar>
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
        <BottomBar/>
    </div>
);

export default ListBox;

