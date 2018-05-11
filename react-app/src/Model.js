var cuid = require('cuid');

class ListModel {
    constructor(props) {
        this._id = props._id ? props._id : "list:" + cuid();
        this._rev = props._rev ? props._rev : "";
        this._deleted = props._deleted ? props._deleted : false;
        this.type = "list";
        this.version = 1;
        this.title = props.title ? props.title : "An Anonymous List";
        this.note = "";
        this.checked = props.checked ? props.checked : false;
        this.createdAt = Date.now();
        this.updatedAt = Date.now();
    }
};

class CollectionModel {
    constructor(props) {
        this._id = props._id ? props._id : "collection:" + cuid();
        this._rev = props._rev ? props._rev : "";
        this._deleted = props._deleted ? props._deleted : false;
        this.type = "collection";
        this.version = 1;
        this.title = props.title ? props.title : "An Anonymous Collection";
        this.note = "";
        // ids of member lists and collections
        this.members = props.members ? props.members.map((i)=>i._id) : []; 
        this.createdAt = Date.now();
        this.updatedAt = Date.now();
    }
};

class ListItemModel {
    constructor(props) {
        this._id = props._id ? props._id : "item:" + cuid();
        this._rev = props._rev ? props._rev : "";
        this._deleted = props._deleted ? props._deleted : false;
        this.type = "item";
        this.version = 1;
        this.list = props.list ? props.list._id : false;
        this.title = props.title ? props.title : "An Anonymous Item";
        this.checked = props.checked ? props.checked : false;
        this.fields = props.fields;
        this.createdAt = Date.now();
        this.updatedAt = Date.now();
    }
};

export {CollectionModel, ListModel, ListItemModel};

