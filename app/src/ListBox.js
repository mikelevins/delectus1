import React from 'react';
import AddColumnButton from './AddColumnButton';
import AddRowButton from './AddRowButton';
import ListItems from './ListItems';

const ListBox = (props) => (
    <div>
        <span>{props.name}</span>
        <AddColumnButton />
        <p>{console.log(props.theme.palette.borderColor)}</p>
        <ListItems
            columns={props.columns}
            items={props.items}
        />
        <AddRowButton />
    </div>
);

export default ListBox;
