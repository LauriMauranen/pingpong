(ns pingpong.test-runner
  (:require
   [doo.runner :refer-macros [doo-tests]]
   [pingpong.core-test]
   [pingpong.common-test]))

(enable-console-print!)

(doo-tests 'pingpong.core-test
           'pingpong.common-test)
