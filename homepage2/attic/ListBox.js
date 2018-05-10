import React from 'react';
import AddColumnButton from './AddColumnButton';
import AddRowButton from './AddRowButton';
import ListItems from './ListItems';
import './ListBox.css';

const ListBox = (props) => (
    <div className="ListBox">
        <div className="ListBoxTitleBar">
            <span>{props.name} </span>
            <AddColumnButton />
        </div>
        <ListItems
            columns={props.columns}
            items={props.items}
        />
        <AddRowButton />
    </div>
);

export default ListBox;
