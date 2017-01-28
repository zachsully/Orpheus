DATADIR = data

build:
	sbt update; sbt compile; cabal build

data: dataDir
	git clone git@github.com:MutopiaProject/MutopiaProject.git

dataDir:
	mkdir -p data;

wipeData:
	rm -rf data;

clean:
	sbt clean; cabal clean; rm -rf dist target *~
