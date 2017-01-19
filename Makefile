build:
	sbt update; sbt compile; cabal build

clean:
	sbt clean; cabal clean; rm -rf dist target *~
