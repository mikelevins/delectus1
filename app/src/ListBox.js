import React from 'react';
import AddColumnButton from './AddColumnButton';
import AddRowButton from './AddRowButton';
import ListItems from './ListItems';

const ListBox = (props) => (
    <div className="ListBox">
        <span className="ListBoxName">{props.name}</span>
        <AddColumnButton />
        <p></p>
        <ListItems items={props.items} />
        <AddRowButton />
    </div>
);

export default ListBox;
