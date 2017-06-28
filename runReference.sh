#!/bin/sh
docker build -t reference .
docker run   -t --rm --name reference -p 127.0.0.1:8080:8080 reference
