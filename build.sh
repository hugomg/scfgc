#! /bin/sh

corebuild \
  -j 5 \
  -use-menhir \
  -pkg re2 \
  -pkg fileutils \
  $@
