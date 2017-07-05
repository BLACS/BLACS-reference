#!/bin/sh
docker build -t reference .
docker run   -t --rm --name reference reference
