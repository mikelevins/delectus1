# delectus web app

Used to create this project:

1. $ lein new compojure-app delectus
2. add :adapter {:port 4000} to the :ring key in project.edn
   to control the port on which the jetty server launches

## Prerequisites

You will need [Leiningen][1] 1.7.0 or above installed.

[1]: https://github.com/technomancy/leiningen

## Running

To start a web server for the application, run:

    lein ring server

## License

Copyright © 2020 mikel evins
