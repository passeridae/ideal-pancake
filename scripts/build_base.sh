#! /bin/bash -e

TAG=oswynb/ideal-pancake
docker build --rm=true --tag=$TAG-base:latest -f BaseDockerfile .
