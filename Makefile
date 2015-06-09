NAME := etss
CASK ?= cask
CWD ?= $(shell pwd)
RM := rm -rf

.PHONY: test compile dist clean

test: compile
	@echo "** Running all tests...."
	@echo "*** For fine-tuned test, use 'cask exec ert-runner' directly (see README)"
	@${CASK} exec ert-runner

# TODO better way to work with sub source directory? Here we need to set
# EMACSLOADPATH outside of `cask'.
compile:
	@echo "** Compiling..."
	@EMACSLOADPATH=${CWD}/src:: ${CASK} build

# TODO there are two bugs about `package-build' and `cask'
# 
# 1. `package-build' requirements need explicit minimum versions, nil or blank
# string results in errors.
# 2. `cask package' doesn't create the same `<package>-pkg.el' file as `cask pkg-file'
#
# The following recipe works around this bug for now.
PKG_EL_FILE := ${NAME}-pkg.el
dist:
	@echo "** Packaging..."
	${CASK} pkg-file
	${CASK} package
	${RM} ${PKG_EL_FILE}

clean:
	@echo "** Cleaning..."
	${CASK} clean-elc
	${RM} dist/
