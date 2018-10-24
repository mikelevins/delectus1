# Delectus 2

A web-based version of the Delectus list manager

## delectus-server

This is a server application that handles requests from the browser-based user interface.

### Running the server

``` shell
$ cd delectus-server
$ PORT=3001 node bin/www
```

The default port for an Express application is 3000. We choose `PORT=3001` in order to avoid colliding with the React client, which also by default runs on port 3000.

## delectus-client

This is a client application built in React. The client process serves the user interface application to requesting browsers. The React code then communicates with the Express-based server application. in order to create accounts, perform user and admin tasks, and set up sessions between the client and the CouchDB database.

### Running the client

``` shell
$ cd delectus-client
$ npm start
```