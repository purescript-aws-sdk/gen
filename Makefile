.PHONY: clean build run

AWS_VERSION := $(shell cat aws-sdk-js/package.json | jq -r .version)

clean:
	rm -fr aws-sdk-purs bower_components output

build:
	bower update
	pulp build

run:
	pulp run
