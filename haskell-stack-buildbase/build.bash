#!/bin/bash

cd $(dirname $0)

STACK_YAML=${1}
[ -f "${STACK_YAML}" ] || { echo "Stack-yaml file not found: ${STACK_YAML}" 1>&2; exit 1; }

RESOLVER_VERSION=$(yq '.resolver' ${STACK_YAML})
[[ "${RESOLVER_VERSION}" =~ ^lts-.+$ ]] || { echo "Invalid resolver version: ${RESOLVER_VERSION}." 1>&2; exit 1; }

mkdir -p stack-snapshots

docker build -t haskell-stack-buildbase:${RESOLVER_VERSION} -f- . <<EOF
FROM fpco/stack-build:${RESOLVER_VERSION}
COPY stack-snapshots /root/.stack/
COPY . /opt/build
RUN apt update && apt install -y pkg-config
RUN stack -v --stack-yaml=/opt/build/${STACK_YAML} build --system-ghc --only-dependencies && rm -fr /opt/build
EOF
