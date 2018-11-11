#!/bin/sh -x

ROOT=`pwd`

CI_IMAGE=ucmbuild:single
CI_IMAGE_REMOTE=skymaverick/meson-ucm:xenial

CI_NAME="ucmdocker-worker"
CI_CONFIG="Dockerfile"
CI_GENERATOR="/tools/travis/echo_docker_template.sh"

info() {
    echo "\033[33;1m$1\033[0m"
}

CREATE_DOCKER_FILE() {
    if [ -f $CI_CONFIG ]
    then
        rm -f $CI_CONFIG
    fi
    $CI_GENERATOR >> $CI_CONFIG
}

CI_CREATE_NEW() {
    info "Create docker image ${1} as ${2} in ${ROOT} "
    CREATE_DOCKER_FILE && echo  "ADD . /root"    >> $CI_CONFIG

    docker build -t ${1} ${ROOT}
}

CI_CREATE_FAST() {
    info "Fast create docker image ${1} as ${2} in ${ROOT} "
    
    if [ -f $CI_CONFIG ]
    then
        rm -f $CI_CONFIG
    fi
    
    echo "FROM ${2}"    >> $CI_CONFIG
    echo "ADD . /root " >> $CI_CONFIG
    
    docker build -t ${1} ${ROOT}
}

CI_CLEANUP() {
    info "Cleanup after build"
    docker stop ${1}
    docker rm -f ${1}
}

case $1 in
    CREATE)
        CI_CREATE_NEW $CI_IMAGE $CI_NAME
        docker run -dit -v ${ROOT}/build:/root/build \
                   -w /root --privileged=true \
                   --net=host \
                   --name ${CI_NAME} ${CI_IMAGE} /sbin/init
    ;;
    CREATE_FAST)
        CI_CREATE_FAST $CI_IMAGE $CI_IMAGE_REMOTE $CI_NAME
        docker run -dit -v ${ROOT}/build:/root/build \
                   -w /root --privileged=true \
                   --net=host \
                   --name ${CI_NAME} ${CI_IMAGE} /sbin/init
    ;;
    RUN_DEBUG)
        docker exec -ti ${CI_NAME} ./run.sh build debug
    ;;
    RUN_RELEASE)
        docker exec -ti ${CI_NAME} ./run.sh build release
    ;;
    RUN_COVERITY)
#docker exec -ti ${CI_NAME} ./run.sh debug
        docker exec -ti ${CI_NAME} mkdir cov-build && meson cov-build
        docker exec -ti ${CI_NAME} ./tools/travis/coverity.sh build
        docker exec -ti ${CI_NAME} ./tools/travis/coverity.sh upload
    ;;
    CLEANUP)
        CI_CLEANUP $CI_NAME
    ;;
    UPDATE_DH)
        CREATE_DOCKER_FILE
        docker push $CI_IMAGE_REMOTE
    ;;
    *)
        info "This is not manager command"
        exit 1
    ;;
esac
