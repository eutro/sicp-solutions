#!/bin/sh

packages=`tr '\n' ' ' < raco-requirements.txt`
raco pkg install --auto --skip-installed $packages
