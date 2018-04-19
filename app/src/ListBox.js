import React from 'react';
import AddItemButton from './AddItemButton';
import ListItems from './ListItems';

const ListBox = (props) => (
    <div className="ListBox">
        <span className="ListBoxName">{props.name}</span>
        <span className="AddColumnButton">+</span>
        <p></p>
        <ListItems items={props.items} />
        <AddItemButton />
    </div>
);

export default ListBox;
