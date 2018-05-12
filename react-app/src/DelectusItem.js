var cuid = require('cuid');

class DelectusItem {
    constructor(props) {
        this._id = props._id ? props._id : "item:" + cuid();
        this._rev = props._rev ? props._rev : "";
        this._deleted = props._deleted ? props._deleted : false;
        this.type = "DelectusItem";
        this.version = 1;
        this.list = props.list ? props.list._id : false;
        this.title = props.title ? props.title : "An Anonymous Item";
        this.checked = props.checked ? props.checked : false;
        this.fields = props.fields;
        this.createdAt = props.createdAt ? props.createdAt : Date.now();
        this.updatedAt = props.updatedAt ? props.updatedAt : Date.now();
    }
};

export default DelectusItem;

