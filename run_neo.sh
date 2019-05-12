#! /bin/bash

docker run \
    -e EO4J_AUTH=none \
    -p 7474:7474 \
    -p 7687:7687 \
    -v $HOME/dev/docker_vols/neo4j/data:/data \
    neo4j