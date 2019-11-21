(ns admin-webapp.doo-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [admin-webapp.core-test]))

(doo-tests 'admin-webapp.core-test)

