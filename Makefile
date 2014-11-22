all: data/firefox-backouts.rds data/firefox-bug-data.rds data/firefox-bugs.rds data/firefox-commit-data.rds data/firefox-commits.rds data/firefox-event-data.rds data/firefox-event-labels.rds data/firefox-events.rds data/firefox-fixes.rds data/firefox-reviews.rds data/mozilla-hg-log.bz2 report/aggregate-events-to-bugs.html report/analyse.html report/detect-backout-commits.html report/detect-fix-commits.html report/detect-reviews.html report/label-events.html report/load-firefox-bugs.html report/load-firefox-commits.html report/load-firefox-events.html report/transform-commit-data.html report/transform-event-data.html

clean:
	rm -f data/firefox-backouts.rds data/firefox-bug-data.rds data/firefox-bugs.rds data/firefox-commit-data.rds data/firefox-commits.rds data/firefox-event-data.rds data/firefox-event-labels.rds data/firefox-events.rds data/firefox-fixes.rds data/firefox-reviews.rds data/mozilla-hg-log.bz2 report/aggregate-events-to-bugs.html report/analyse.html report/detect-backout-commits.html report/detect-fix-commits.html report/detect-reviews.html report/label-events.html report/load-firefox-bugs.html report/load-firefox-commits.html report/load-firefox-events.html report/transform-commit-data.html report/transform-event-data.html

data/firefox-bug-data.rds: data/firefox-event-data.rds data/firefox-event-labels.rds data/firefox-bugs.rds script/aggregate-events-to-bugs.R
	./run-script.rb script/aggregate-events-to-bugs.R

report/aggregate-events-to-bugs.html: data/firefox-event-data.rds data/firefox-event-labels.rds data/firefox-bugs.rds script/aggregate-events-to-bugs.R
	./run-script.rb script/aggregate-events-to-bugs.R

report/analyse.html: data/firefox-bug-data.rds script/analyse.R
	./run-script.rb script/analyse.R

data/firefox-backouts.rds: data/firefox-commits.rds script/detect-backout-commits.R
	./run-script.rb script/detect-backout-commits.R

report/detect-backout-commits.html: data/firefox-commits.rds script/detect-backout-commits.R
	./run-script.rb script/detect-backout-commits.R

data/firefox-fixes.rds: data/firefox-commits.rds script/detect-fix-commits.R
	./run-script.rb script/detect-fix-commits.R

report/detect-fix-commits.html: data/firefox-commits.rds script/detect-fix-commits.R
	./run-script.rb script/detect-fix-commits.R

data/firefox-reviews.rds: data/firefox-events.rds script/detect-reviews.R
	./run-script.rb script/detect-reviews.R

report/detect-reviews.html: data/firefox-events.rds script/detect-reviews.R
	./run-script.rb script/detect-reviews.R

data/firefox-event-labels.rds: data/firefox-event-data.rds data/firefox-reviews.rds script/label-events.R
	./run-script.rb script/label-events.R

report/label-events.html: data/firefox-event-data.rds data/firefox-reviews.rds script/label-events.R
	./run-script.rb script/label-events.R

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

data/firefox-commit-data.rds: data/firefox-commits.rds data/firefox-backouts.rds data/firefox-fixes.rds script/transform-commit-data.R
	./run-script.rb script/transform-commit-data.R

report/transform-commit-data.html: data/firefox-commits.rds data/firefox-backouts.rds data/firefox-fixes.rds script/transform-commit-data.R
	./run-script.rb script/transform-commit-data.R

data/firefox-event-data.rds: data/firefox-bugs.rds data/firefox-events.rds data/firefox-commit-data.rds script/transform-event-data.R
	./run-script.rb script/transform-event-data.R

report/transform-event-data.html: data/firefox-bugs.rds data/firefox-events.rds data/firefox-commit-data.rds script/transform-event-data.R
	./run-script.rb script/transform-event-data.R

data/mozilla-hg-log.bz2:  script/load-mozilla-hglog.rb
	./run-script.rb script/load-mozilla-hglog.rb
