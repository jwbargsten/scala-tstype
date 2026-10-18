.PHONY: fmt clean build test dist publish-local publish run-example

fmt:
	sbt scalafmtAll

clean:
	-find . \( -name target -o -name .scala-build \) -prune -exec rm -rf {} +
	sbt clean

build:
	sbt compile

test:
	sbt test

dist: clean build test
	publish-sbt-sonatype publishSigned

publish-local:
	sbt publishLocal

publish: publish-local dist run-example
	publish-sbt-sonatype sonaUpload

run-example:
	cd example && scala-cli run .
