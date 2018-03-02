.PHONY: build release

build:
	bower update
	pulp build

release:
	pulp version ${VERSION}
	pulp publish
