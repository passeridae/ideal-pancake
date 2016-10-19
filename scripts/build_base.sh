#! /bin/bash -e

export GIT_REV=$(git describe --always)
TAG=oswynb/ideal-pancake
docker build --rm=true --tag=$TAG-base:${GIT_REV} -f BaseDockerfile .
