import React from 'react';
import AddColumnButton from './AddColumnButton';
import AddRowButton from './AddRowButton';
import ListItems from './ListItems';
import styled from 'styled-components';

const ListBoxStyle = styled.div`
  border-color: #402000;
  border-style: solid;
  border-width: 1;
  margin: 0.25em;
  padding: 1rem;    
`;

const ListBox = (props) => (
    <ListBoxStyle>
        <span>{props.name}</span>
        <AddColumnButton />
        <p></p>
        <ListItems
            columns={props.columns}
            items={props.items}
        />
        <AddRowButton />
    </ListBoxStyle>
);

export default ListBox;
