#!/bin/bash

if [[ $1 == stat ]]; then
    echo Running instances:
    echo
    ps aux | grep '\--class scrape' | grep -E 'scrapers/(.*).py .*' -o
    echo
    exit 0
fi

# check if already running
if [[ $(ps aux | grep '\--class scrape' | wc -l) -ne 1 ]]; then
    echo "Some processes are already running. Either kill them or let them complete. And then try again."
    exit 1
fi


dir_=/home/bibek/projects/scrapeet
venv=/home/bibek/.local/share/virtualenvs/scrapeet-uUCGsuij
py=$venv/bin/python

# past items
for x in nagariknews news24 ratopati; do
    $py $dir_/scrapers/$x.py --clear-errors-all
    $py $dir_/scrapers/$x.py --set-done-false
    termite --class scrape -e "$py $dir_/scrapers/$x.py --past-all" &
    sleep 0.3s
done

# current items
for x in onlinekhabar setopati; do
    termite --class scrape -e "$py $dir_/scrapers/$x.py --current-all" &
    sleep 0.3s
done

# Run for kantipur
termite --class scrape -e "$py $dir_/scrapers/tkp.py news" &
