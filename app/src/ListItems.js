import React from 'react';
import ListItem from './ListItem';
import styled from 'styled-components';
import './App.css';

const ListItemsTable = styled.table`
    border-collapse: collapse;
    table-layout: fixed;
    width: 100%;
`;

const ListItems = (props) => (
    <div>
        <ListItemsTable><tbody>
            <tr>{props.columns.map(function (field) {
                return <th> {field} </th>
            })}
            </tr>
            {props.items.map(function (item) {
                return <ListItem itemValue={item} />;
            })}
        </tbody></ListItemsTable>
    </div>
);

export default ListItems;