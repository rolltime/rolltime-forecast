#!/bin/bash

#
# Cleaning Docker containers.
#
docker rm $(docker ps -q -a)

#
# Cleaning Docker images.
#
docker rmi $(docker images -q)
