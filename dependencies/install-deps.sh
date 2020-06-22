#!/bin/sh

packages=`tr '\n' ' ' < raco-requirements.txt`
raco pkg install --skip-installed $packages
