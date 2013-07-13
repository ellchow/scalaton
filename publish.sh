#!/usr/bin/env bash

project=$1
dt=`date`

git clone -b gh-pages git@github.com:ellchow/scalaton.git publish-repo && \

sbt "project $project" clean publish && \

cd publish-repo && \

mkdir -p snapshots releases && \

git add snapshots releases && \

git commit -m $project && \

git push && \

echo 'removing publish-repo'
cd .. && rm -rf publish-repo

