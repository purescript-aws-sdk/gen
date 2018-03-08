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
	for project in ${DIR_PS_PROJECTS}/*; do \
		cp -pR ${DIR_PS_PROJECT}/bower_components $${project}; \
		cp -pR ${DIR_PS_PROJECT}/output $${project}; \
		rm -fr ${project}/output/${DIR_PS_PROJECT_OUTPUT_NAME}; \
	done

test-all:
	for project in ${DIR_PS_PROJECTS}/*; do \
		make test-$$(basename $${project}) || break 0 2>/dev/null; \
	done

release-all:
	for project in ${DIR_PS_PROJECTS}/*; do \
		make release-$$(basename $${project}) || break 0 2>/dev/null; \
	done

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
	echo "create-git-$* $$(date)"
	make create-git-$*
	echo "git-rebase-$* $$(date)"
	make git-rebase-$*
	echo "test-$* $$(date)"
	make test-$*

	echo "git-push-$* $$(date)"
	make git-push-$*
	cd ${DIR_PS_PROJECTS}/$* && \
		echo "pulp-version $* $$(date)" && \
		pulp version ${VERSION} && \
		echo "pulp-publish $* $$(date)" && \
		yes | pulp publish --no-push && \
		echo "git push $* $$(date)" && \
		git push origin v${VERSION}
