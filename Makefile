all: data/firefox-bugs.rds data/firefox-events.rds report/load-firefox-bugs.html report/load-firefox-events.html

clean:
	rm -f data/firefox-bugs.rds data/firefox-events.rds report/load-firefox-bugs.html report/load-firefox-events.html

data/firefox-bugs.rds:  script/load-firefox-bugs.R
	./run-script.rb script/load-firefox-bugs.R

report/load-firefox-bugs.html:  script/load-firefox-bugs.R
	./run-script.rb script/load-firefox-bugs.R

data/firefox-events.rds: data/firefox-bugs.rds script/load-firefox-events.R
	./run-script.rb script/load-firefox-events.R

report/load-firefox-events.html: data/firefox-bugs.rds script/load-firefox-events.R
	./run-script.rb script/load-firefox-events.R
