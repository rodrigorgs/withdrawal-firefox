all: data/firefox-backouts.rds data/firefox-bugs.rds data/firefox-commits.rds data/firefox-events.rds data/firefox-fixes.rds data/mozilla-hg-log.bz2 report/detect-backout-commits.html report/detect-fix-commits.html report/load-firefox-bugs.html report/load-firefox-commits.html report/load-firefox-events.html

clean:
	rm -f data/firefox-backouts.rds data/firefox-bugs.rds data/firefox-commits.rds data/firefox-events.rds data/firefox-fixes.rds data/mozilla-hg-log.bz2 report/detect-backout-commits.html report/detect-fix-commits.html report/load-firefox-bugs.html report/load-firefox-commits.html report/load-firefox-events.html

data/firefox-backouts.rds: data/firefox-commits.rds script/detect-backout-commits.R
	./run-script.rb script/detect-backout-commits.R

report/detect-backout-commits.html: data/firefox-commits.rds script/detect-backout-commits.R
	./run-script.rb script/detect-backout-commits.R

data/firefox-fixes.rds: data/firefox-commits.rds data/firefox-bugs.rds script/detect-fix-commits.R
	./run-script.rb script/detect-fix-commits.R

report/detect-fix-commits.html: data/firefox-commits.rds data/firefox-bugs.rds script/detect-fix-commits.R
	./run-script.rb script/detect-fix-commits.R

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
