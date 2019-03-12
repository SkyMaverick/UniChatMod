#!/bin/sh

ROOT=`pwd`
BUILD_DIR=${ROOT}/build

CI_IMAGE=ucmbuild:single
CI_IMAGE_REMOTE=skymaverick/meson-ucm:trusty

CI_NAME="ucmdocker-worker"
CI_CONFIG="Dockerfile"
CI_GENERATOR="./tools/travis/echo_docker_template.sh"
CI_COVERITY="./tools/travis/coverity.sh"
CI_COVERITY_LOADER="./tools/travis/get_coverity.sh"
CI_SF_UPLOADER="./tools/travis/sfupload.sh"

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

    docker build\
    -t ${1} ${ROOT}
}

CI_CREATE_FAST() {
    info "Fast create docker image ${1} as ${2} in ${ROOT} "
    
    if [ -f $CI_CONFIG ]
    then
        rm -f $CI_CONFIG
    fi
    
    echo "FROM ${2}"    >> $CI_CONFIG
    echo "ADD . /root " >> $CI_CONFIG
    
    docker build\
    -t ${1} ${ROOT}
}

CI_CLEANUP() {
    info "Cleanup after build"
    docker stop ${1}
    docker rm -f ${1}
}

case $1 in
    CREATE)
        set -e
        if ! [ -d ${BUILD_DIR} ]
        then
            mkdir -p ${BUILD_DIR}
        fi
        
        CI_CREATE_NEW $CI_IMAGE $CI_NAME
        ENV_FILE=".cov-env"
        if [ -f ${ENV_FILE} ]
        then
            docker run -dit -v ${BUILD_DIR}:/root/build \
                       -w /root --privileged=true \
                       --net=host \
                       --env-file=${ENV_FILE} \
                       --name ${CI_NAME} ${CI_IMAGE} /sbin/init
        else
            docker run -dit -v ${BUILD_DIR}:/root/build \
                       -w /root --privileged=true \
                       --net=host \
                       --name ${CI_NAME} ${CI_IMAGE} /sbin/init
        fi
        set +e
    ;;
    CREATE_FAST)
        set -e
        if ! [ -d ${BUILD_DIR} ]
        then
            mkdir -p ${BUILD_DIR}
        fi
        CI_CREATE_FAST $CI_IMAGE $CI_IMAGE_REMOTE $CI_NAME
        docker run -dit -v ${BUILD_DIR}:/root/build \
                   -w /root --privileged=true \
                   --net=host \
                   --name ${CI_NAME} ${CI_IMAGE} /sbin/init
        set +e
    ;;
    RUN_DEBUG)
        set -e
        docker exec -ti ${CI_NAME} ./run.py debug
        set +e
    ;;
    RUN_RELEASE)
        set -e
        docker exec -ti ${CI_NAME} ./run.py release
        set +e
    ;;
    RUN_COVERITY)
        set -e
        docker exec -ti ${CI_NAME} sh -c "mkdir cov-build && meson cov-build"
        docker exec -ti ${CI_NAME} ${CI_COVERITY_LOADER}
        docker exec -ti ${CI_NAME} ${CI_COVERITY} build
        docker exec -ti ${CI_NAME} ${CI_COVERITY} upload
        set +e
    ;;
    DEPLOY)
        docker exec -ti ${CI_NAME} ./run.py pack_arc
        docker exec -ti ${CI_NAME} ./run.py pack_deb
        docker exec -ti ${CI_NAME} ./run.py pack_sh
        ${CI_SF_UPLOADER}
    ;;
    CLEANUP)
        set -e
        CI_CLEANUP $CI_NAME
        set +e
    ;;
    UPDATE_DH)
        set -e
        CREATE_DOCKER_FILE  >> ${CI_CONFIG}
        
        docker build -t $CI_IMAGE_REMOTE .
        docker push $CI_IMAGE_REMOTE
        set +e
    ;;
    *)
        info "This is not manager command"
        exit 1
    ;;
esac
