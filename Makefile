all: data/firefox-bugs.rds data/firefox-commits.rds data/firefox-events.rds data/mozilla-hg-log.bz2 report/load-firefox-bugs.html report/load-firefox-commits.html report/load-firefox-events.html

clean:
	rm -f data/firefox-bugs.rds data/firefox-commits.rds data/firefox-events.rds data/mozilla-hg-log.bz2 report/load-firefox-bugs.html report/load-firefox-commits.html report/load-firefox-events.html

data/firefox-bugs.rds:  script/load-firefox-bugs.R
	./run-script.rb script/load-firefox-bugs.R

report/load-firefox-bugs.html:  script/load-firefox-bugs.R
	./run-script.rb script/load-firefox-bugs.R

data/firefox-commits.rds: data/mozilla-hg-log.bz2 script/load-firefox-commits.R
	./run-script.rb script/load-firefox-commits.R

report/load-firefox-commits.html: data/mozilla-hg-log.bz2 script/load-firefox-commits.R
	./run-script.rb script/load-firefox-commits.R

data/firefox-events.rds: data/firefox-bugs.rds script/load-firefox-events.R
	./run-script.rb script/load-firefox-events.R

report/load-firefox-events.html: data/firefox-bugs.rds script/load-firefox-events.R
	./run-script.rb script/load-firefox-events.R

data/mozilla-hg-log.bz2:  script/load-mozilla-hglog.rb
	./run-script.rb script/load-mozilla-hglog.rb
