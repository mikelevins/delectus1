import React from 'react';
import AddColumnButton from './AddColumnButton';
import AddRowButton from './AddRowButton';
import ListItems from './ListItems';
import styled from 'styled-components';

const ListBox = (props) => (
    <div>
        <span>{props.name}</span>
        <AddColumnButton />
        <p></p>
        <ListItems
         columns={props.columns}
         items={props.items}
         />
        <AddRowButton />
    </div>
);

export default ListBox;
