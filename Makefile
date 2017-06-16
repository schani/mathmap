all : stills.html recursive

stills.html : stills.chtml dist/mathmap.chh
	chpp stills.chtml >stills.html

recursive :
	$(MAKE) -C dist

tar :
	mkdir /tmp/mathmap-home
	cp -R * /tmp/mathmap-home
	find /tmp/mathmap-home -name .svn | xargs rm -rf
	find /tmp/mathmap-home -name '*~' | xargs rm
	( cd /tmp ; tar -zcvf mathmap-home.tar.gz mathmap-home )
	cp /tmp/mathmap-home.tar.gz .
