.PHONY: clean init build run

build:
	pulp build

clean:
	rm -fr aws-sdk-js aws-sdk-purs bower_components output

init: update-dependencies
	git clone https://github.com/aws/aws-sdk-js.git

update-dependencies:
	bower update

run:
	pulp run
