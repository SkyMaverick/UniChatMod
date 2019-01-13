echo Decrtypting id_rsa...
openssl aes-256-cbc -K $encrypted_cafd02e1f1a1_key -iv $encrypted_cafd02e1f1a1_iv -in ${TRAVIS_BUILD_DIR}/tools/travis/id_rsa.enc -out ${TRAVIS_BUILD_DIR}/tools/travis/id_rsa -d
eval "$(ssh-agent -s)"
chmod 600 tools/travis/id_rsa
ssh-add tools/travis/id_rsa || exit 1

PKGSDIR=${TRAVIS_BUILD_DIR}/build/pkgs
SSHOPTS="ssh -o StrictHostKeyChecking=no"

case "$TRAVIS_OS_NAME" in
    linux)
        echo Uploading linux artifacts...
        if [ -d ${PKGSDIR} ]; then
            if [ "${TRAVIS_BRANCH}" == "master" ]; then
                rsync -avP -e "$SSHOPTS" ${PKGSDIR}/*.* skymaverick,UniChatMod@frs.sourceforge.net:/home/frs/project/UniChatMod/daily/linux || exit 1
            fi
        else
            echo "Don't found packages"
        fi
    ;;
esac
