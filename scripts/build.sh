#! /bin/bash -e

export GIT_REV=$(git describe --always)
TAG=ideal-pancake
docker build --rm=true --tag=$TAG-build:${GIT_REV} -f BuildDockerfile .
cd run
mkdir -p export
rm -rf export/*
docker run --name tmp-build $TAG-build:${GIT_REV} ls
docker cp tmp-build:/build/.stack-work/install/x86_64-linux/lts-7.0/8.0.1/bin/ export/
docker cp tmp-build:/build/static export/static
docker cp tmp-build:/build/database export/database
docker cp tmp-build:/build/run_pancake.sh export/run_pancake.sh
docker build --rm=true --tag=$TAG:${GIT_REV} .
docker rm tmp-build
docker rmi $TAG-build:${GIT_REV}
docker tag $TAG:${GIT_REV} $TAG:latest
