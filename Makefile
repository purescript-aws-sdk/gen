.PHONY: clean init build run

clean:
	rm -fr aws-sdk-js aws-sdk-purs bower_components output

init:
	git clone https://github.com/aws/aws-sdk-js.git

build:
	bower update
	pulp build

run:
	pulp run
