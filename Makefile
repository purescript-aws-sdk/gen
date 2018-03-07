.DEFAULT_GOAL := build

VERSION_AWS := $(shell cat aws-sdk-js/package.json | jq -r .version)
VERSION := 0.0.$(shell git log --oneline | wc -l | tr -d '[:space:]')

GITHUB_TOKEN ?= $(error Requires a github personal access token with public_repo scope: https://github.com/settings/tokens)

DIR_PS_PROJECTS := aws-sdk-purs
DIR_PS_PROJECT := ${DIR_PS_PROJECTS}/purescript-aws-acm
DIR_PS_PROJECT_OUTPUT_NAME := AWS.ACM
DIR_TMP := /tmp/aws-sdk-purs

clean:
	rm -fr aws-sdk-purs bower_components output

init:
	bower update

build:
	pulp build

run:
	pulp run

init-all:
	cd ${DIR_PS_PROJECT} && bower update && pulp build
	ls ${DIR_PS_PROJECTS} | xargs -n1 -P2 sh -c ' \
		cp -pR ${DIR_PS_PROJECT}/bower_components ${DIR_PS_PROJECTS}/$$0 && \
		cp -pR ${DIR_PS_PROJECT}/output ${DIR_PS_PROJECTS}/$$0 && \
		rm -fr ${DIR_PS_PROJECTS}/$$0/output/${DIR_PS_PROJECT_OUTPUT_NAME} || \
		true'

test-all: init-all
	ls ${DIR_PS_PROJECTS} | xargs -n1 -P2 sh -c 'make test-$$0 || exit 255'

release-all: init-all
	ls ${DIR_PS_PROJECTS} | xargs -n1 -P2 sh -c 'make release-$$0 || exit 255'

create-git-%:
	curl 'https://api.github.com/orgs/purescript-aws-sdk/repos' \
		-d '{ "name": "$*", "auto_init": true }' \
		-H 'Authorization: token ${GITHUB_TOKEN}'

git-rebase-%:
	rm -fr ${DIR_TMP} && mkdir -p ${DIR_TMP} || true
	git clone git@github.com:purescript-aws-sdk/$*.git ${DIR_TMP}/$*
	mv ${DIR_TMP}/$*/.git ${DIR_PS_PROJECTS}/$*
	rm -fr ${DIR_TMP}/$*

	cd ${DIR_PS_PROJECTS}/$* && \
		git add . && \
		git commit -m 'PureScript mapping for aws-sdk-js ${VERSION_AWS}' || \
		true

git-push-%:
	cd ${DIR_PS_PROJECTS}/$* && \
		git push origin master

test-%:
	cd ${DIR_PS_PROJECTS}/$* && pulp build

release-%:
	make create-git-$*
	make git-rebase-$*
	make test-$*

	make git-push-$*
	cd ${DIR_PS_PROJECTS}/$* && \
		pulp version ${VERSION} && \
		yes | pulp publish
