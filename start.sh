#!/bin/bash

echo "Ewok/1.0 (Wicket)"
echo "BETA (c) 2009 simulacity.com"
erl -detached -sname ewok -pa ./ebin -s ewok
