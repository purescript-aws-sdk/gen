.DEFAULT_GOAL := build

VERSION_AWS := $(shell cat aws-sdk-js/package.json | jq -r .version)

VERSION_MAJ_MIN := 0.1
VERSION_PATCH := $(shell git fetch --tags && git tag -l v${VERSION_MAJ_MIN}.[^0] | wc -l | tr -d '[:space:]')
VERSION := ${VERSION_MAJ_MIN}.$$((${VERSION_PATCH} + 1))

GITHUB_TOKEN ?= $(error Requires a github personal access token with public_repo scope: https://github.com/settings/tokens)
GITHUB_OWNER := purescript-aws-sdk

DIR_PS_PROJECTS := aws-sdk-purs
DIR_PS_PROJECT := ${DIR_PS_PROJECTS}/purescript-aws-acm
DIR_PS_PROJECT_OUTPUT_NAME := AWS.ACM
DIR_TMP := /tmp/aws-sdk-purs

clean:
	rm -fr aws-sdk-purs bower_components output

init:
	git submodule update --init --recursive
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

	git tag v${VERSION}
	git push origin --tags

create-git-%:
	curl 'https://api.github.com/orgs/${GITHUB_OWNER}/repos' \
		-H 'Authorization: token ${GITHUB_TOKEN}' \
		-d '{ "name": "$*", "auto_init": true, "has_issues": false, "has_projects": false, "has_wiki": false }'

delete-git-%:
	curl 'https://api.github.com/repos/${GITHUB_OWNER}/$*' \
		-H 'Authorization: token ${GITHUB_TOKEN}' \
		-X DELETE

git-rebase-%:
	rm -fr ${DIR_TMP}/$* ${DIR_PS_PROJECTS}/$*/.git
	mkdir -p ${DIR_TMP}

	git clone git@github.com:${GITHUB_OWNER}/$*.git ${DIR_TMP}/$*
	mv ${DIR_TMP}/$*/.git ${DIR_PS_PROJECTS}/$*
	rm -fr ${DIR_TMP}/$*

	cd ${DIR_PS_PROJECTS}/$* && \
		git add . && \
		git commit -m 'PureScript mapping for aws-sdk-js ${VERSION_AWS}' || \
		true

git-push-%:
	cd ${DIR_PS_PROJECTS}/$* && \
		git push origin master

build-%:
	cd ${DIR_PS_PROJECTS}/$* && \
		pulp build && \
		pulp doc && mv generated-docs docs

release-%:
ifneq ($(shell git rev-parse --abbrev-ref HEAD), master)
	$(error Cannot release: You aren't on the master branch)
endif
ifneq ($(shell git rev-list HEAD...origin/master --count),0)
	$(error Cannot release: Your branch is not up to date)
endif
ifneq ($(shell git status --porcelain),)
	$(error Cannot release: You have unstaged changes)
endif

	make create-git-$*
	make git-rebase-$*

	if [ ! -z "$$(cd ${DIR_PS_PROJECTS}/$* && git tag | grep "^v${VERSION}$$")" ]; then \
		echo "Version ${VERSION} already released"; \
	else \
		make build-$*; \
		make git-push-$*; \
		cd ${DIR_PS_PROJECTS}/$* && \
			git tag v${VERSION} && \
			git push origin --tags; \
	fi
