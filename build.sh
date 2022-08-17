#!/bin/bash

# link against n curses library
#gcc -o test test.c -lncurses

# yak shaving agian..

# add include directory of guile 3
# -I/usr/include/guile/3.0/
#
# add library directory of guile 3
#
# -L /usr/lib/x86_64-linux-gnu/libguile-3.0
#
# make a dynamic library for bessel function from c
#gcc bessel.c -o libbessel.so -I/usr/include/guile/3.0 -L/usr/lib/x86_64-linux-gnu -shared -fPIC

gcc `pkg-config --cflags guile-3.0` -shared -o libguile-bessel.so -fPIC bessel.c -lm


gcc `pkg-config --cflags guile-3.0` -shared -o libguile-getch.so -fPIC getch.c -lcurses
 


