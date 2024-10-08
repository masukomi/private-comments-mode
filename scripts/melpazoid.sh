#!/bin/bash -eux

EMACS="${EMACS:=emacs}"

if [[ -z $(du -s melpazoid-master 2>/dev/null | cut -f1) ]] || \
       [[ $(du -s melpazoid-master 2>/dev/null | cut -f1) -le "100" ]] ; then
    curl -sLk -O https://github.com/riscy/melpazoid/archive/master.zip
    unzip master.zip
    rm -f master.zip
fi

ROOT=$(git rev-parse --show-toplevel)
cd ${ROOT}
PKG_PATH="${ROOT}/melpazoid-master/$(basename $(pwd))"
PKG_NAME=$(basename "$PKG_PATH")
PKG_MAIN=$(cask files | ggrep -E -- "pkg.el$" || true)
PKG_MAIN=$(basename ${PKG_MAIN:-${PKG_NAME}.el})
rm -rf ${PKG_PATH}
mkdir -p ${PKG_PATH}
rsync -v --exclude '*autoloads.el' $(cask files) ${PKG_PATH}/
if [ -s "${ROOT}/LICENSE" ]; then
  cp -p "${ROOT}/LICENSE" ${PKG_PATH}
fi
cd melpazoid-master
cd ${ROOT}
# pkg-info => epl => expects package-alist to be set, ergo: package-initialize
#            (add-function :after (symbol-function (quote byte-compile-file)) \
#              (lambda (filename &rest _args) (delete-file (concat filename \"c\")))) \
PACKAGE_MAIN=$PKG_MAIN EMACS=$EMACS \
  cask emacs -Q --batch -l cl -l checkdoc \
  -l package --eval "(setq package-user-dir \"$(cask package-directory)\")" \
  -f package-initialize -L ${ROOT}/melpazoid-master/melpazoid \
  --eval "(let ((default-directory \"${PKG_PATH}\")) \
              (require (quote melpazoid)))"
