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
    Toolbar, ToolbarTitle
} from 'material-ui/Toolbar';
import FlatButton from 'material-ui/FlatButton';
import AddColumn from 'material-ui/svg-icons/action/view-column';


import BottomBar from './BottomBar';
import './ListBox.css';

const toolbarStyle = {
    backgroundColor: "#fff",
};

const addColumnStyle = {
    color: "#000",
    flex: "none",
};

const ListBox = (props) => (
    <div>
        <Toolbar style={toolbarStyle}>
            <ToolbarTitle text={props.title} />
            <FlatButton
                style={addColumnStyle}
                label="+"
                labelPosition="before"
                primary={true}
                icon={<AddColumn />}
            />
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

            <TableBody stripedRows={true}>
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

        <BottomBar />
    </div>
);

export default ListBox;

