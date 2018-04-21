import React from 'react';
import ListItem from './ListItem';
import './App.css';

const ListItems = (props) => (
    <div>
        <table><tbody>
            <tr>{props.columns.map(function (field) {
                return <th> {field} </th>
            })}
            </tr>
            {props.items.map(function (item) {
                return <ListItem itemValue={item} />;
            })}
        </tbody></table>
    </div>
);

export default ListItems;