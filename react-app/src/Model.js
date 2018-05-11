var cuid = require('cuid');

class DelectusList {
    constructor(props) {
        this._id = props._id ? props._id : "list:" + cuid();
        this._rev = props._rev ? props._rev : "";
        this._deleted = props._deleted ? props._deleted : false;
        this.type = "DelectusList";
        this.version = 1;
        this.title = props.title ? props.title : "An Anonymous List";
        this.note = "";
        this.checked = props.checked ? props.checked : false;
        this.createdAt = Date.now();
        this.updatedAt = Date.now();
    }
};

class DelectusCollection {
    constructor(props) {
        this._id = props._id ? props._id : "collection:" + cuid();
        this._rev = props._rev ? props._rev : "";
        this._deleted = props._deleted ? props._deleted : false;
        this.type = "DelectusCollection";
        this.version = 1;
        this.title = props.title ? props.title : "An Anonymous Collection";
        this.note = "";
        // ids of member lists and collections
        this.members = props.members ? props.members.map((i)=>i._id) : []; 
        this.createdAt = Date.now();
        this.updatedAt = Date.now();
    }
};

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
        this.createdAt = Date.now();
        this.updatedAt = Date.now();
    }
};

export {DelectusCollection, DelectusList, DelectusItem};

