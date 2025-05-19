#!/bin/bash
rm -rd cljs-test-runner-out && clojure -M:cljs -r akovantsev\.pp-test.* -x node