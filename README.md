sicp
====

Solutions to exercises from sicp book. The code was tested on MIT/GNU Scheme
microcode 15.3.

run tests
=========
cd <chapter dir>
for f in `ls`; do mit-scheme --load $f | tee -i /tmp/tests.out; cat /tmp/tests.out | grep --colour Failed && break; done;
