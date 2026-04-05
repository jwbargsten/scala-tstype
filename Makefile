.PHONY: publish-local test fmt

publish-local:
	sbt publishLocal

fmt:
	sbt scalafmtSbt	scalafmtAll

test:
	sbt test
