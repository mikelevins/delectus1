import { observable, action } from "mobx";

export var AppState = observable({
    DelectulusInstance: {
        host: "mars.local",
        port: "5984",
        protocol: "http",
        dbName: "delectulus",
        remoteCouch: null
    },
    Users: ["granny","greer","mikel"],
    SelectedUser: null,
  });

  AppState.updateUserSelection = action(function setUser(user) {
    AppState.SelectedUser = user;
  });

  AppState.updateRemoteCouch = action(function setCouch(remote) {
    AppState.DelectulusInstance.remoteCouch = remote;
  });