# delectus

A [re-frame](https://github.com/day8/re-frame) application.

Commands used to create this project:

1. $ lein new re-frame delectus +less +10x +cider +handler
2. $ lein deps
3. $ yarn install
4. [to start auto-compilation of less CSS] $ lein less auto
5. [to start app server with autocompilation and hot loading] lein dev

## Development Mode

### Start Cider from Emacs:

Refer to the [shadow-cljs Emacs / CIDER documentation](https://shadow-cljs.github.io/docs/UsersGuide.html#cider).

The mentioned `dir-local.el` file has been created.

### Compile css:

Compile css file once.

```
lein less once
```

Automatically recompile css file on change.

```
lein less auto
```

### Run application:

```
lein dev
```

shadow-cljs will automatically push cljs changes to the browser.

Wait a bit, then browse to [http://localhost:8280](http://localhost:8280).

## Production Build

```
lein clean
lein with-profile prod uberjar
```

That should compile the ClojureScript code first, and then create the standalone jar.

When you run the jar you can set the port the ring server will use by setting the environment variable PORT.
If it's not set, it will run on port 3000 by default.

To deploy to heroku, first create your app:

```
heroku create
```

Then deploy the application:

```
git push heroku master
```
