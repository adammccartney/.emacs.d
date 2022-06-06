#!/usr/bin/bash

# magit.sh: pre make hook for setting up magit

output_magit_config.mk () {
    # will create a file used to 
    EMACS_VERSION=$(emacs --version | head -1 | awk '{ print $3  }')
    CONFIG_FILE="${HOME}/.emacs.d/site-lisp/${EMACS_VERSION}/magit/config.mk"
    rm ${CONFIG_FILE}
    touch ${CONFIG_FILE}
    echo "LOAD_PATH  = -L ~/.emacs.d/site-lisp/${EMACS_VERSION}/magit/lisp" | tee -a ${CONFIG_FILE}
    echo "LOAD_PATH += -L ~/.emacs.d/site-lisp/${EMACS_VERSION}/dash"           | tee -a ${CONFIG_FILE} 
    echo "LOAD_PATH += -L ~/.emacs.d/site-lisp/${EMACS_VERSION}/transient/lisp" | tee -a ${CONFIG_FILE}
    echo "LOAD_PATH += -L ~/.emacs.d/site-lisp/${EMACS_VERSION}/with-editor"    | tee -a ${CONFIG_FILE}
}

output_magit_config.mk

