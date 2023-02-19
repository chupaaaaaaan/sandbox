#!/bin/bash

set -e

cd $(dirname $0)

STACK_YAML="${1}"
[ -f "${STACK_YAML}" ] || { echo "Stack-yaml file not found: ${STACK_YAML}" 1>&2; exit 1; }

RESOLVER_VERSION="$(yq '.resolver' ${STACK_YAML})"
[[ "${RESOLVER_VERSION}" =~ ^lts-.+$ ]] || { echo "Invalid resolver version: ${RESOLVER_VERSION}." 1>&2; exit 1; }

BASE_IMAGE_NAME=haskell-stack-buildbase
IMAGE="${BASE_IMAGE_NAME}:${RESOLVER_VERSION}"

SNAPSHOT_DIR=snapshots
SNAPSHOT_ORIG_DIR="${SNAPSHOT_DIR}-orig"

## buildbaseイメージがあれば、事前にsnapshotを取り出す
[ -d "${SNAPSHOT_DIR}" ] && mv "${SNAPSHOT_DIR}" "${SNAPSHOT_ORIG_DIR}"
mkdir "${SNAPSHOT_DIR}"
(docker image ls | grep "${BASE_IMAGE_NAME}" > /dev/null) &&
    docker run --rm -v "${SNAPSHOT_DIR}":/data "${BASE_IMAGE_NAME}" /bin/bash -c "cp -r /root/.stack/* /data; chown -R 1000:1000 /data/*"
rm -rf "${SNAPSHOT_ORIG_DIR}"

docker build -t "${IMAGE}" -f- . <<EOF
FROM fpco/stack-build:${RESOLVER_VERSION}
COPY ${SNAPSHOT_DIR} /root/.stack/
COPY . /opt/build
RUN apt update && apt install -y pkg-config
RUN stack -v --stack-yaml=/opt/build/${STACK_YAML} build --system-ghc --only-dependencies && rm -fr /opt/build
EOF

docker tag "${IMAGE}" "${BASE_IMAGE_NAME}"
