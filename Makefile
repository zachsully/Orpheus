data : mutopia bachMusicXML

mutopia: dir
	cd data; git clone git@github.com:MutopiaProject/MutopiaProject.git

bachMusicXML: dir
	cd data; git clone git@github.com:jshanley/bach-chorales.git

dir:
	mkdir -p data
