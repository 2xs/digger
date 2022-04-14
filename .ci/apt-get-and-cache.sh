#!/bin/bash

# "apt update ; apt install", with caching of the downloaded packages
# and current state, to minimise load on debian mirrors without having
# to build a new docker image

CACHE=.ci/debian-cache.tar.gz

if [ -z "$1" ]; then
    printf 'Usage: %s <package>...\n' "$0"
    exit 1
fi

if ls -sh $CACHE; then
    sudo tar xvaf $CACHE -C /var
else
    printf 'No debian cache to extract\n'
fi

# Remove the configuration that disable apt cache
sudo rm /etc/apt/apt.conf.d/docker-clean

sudo apt update -y -q
# first only download the packages, so that they end up in the cache
sudo DEBIAN_FRONTEND=noninteractive apt install -yqd "$@"
sudo tar caf $CACHE -C /var lib/apt cache/apt
ls -sh $CACHE

# now really install the packages
sudo DEBIAN_FRONTEND=noninteractive apt install -yq --no-download "$@"
