# Garmin
Visualization for FIT-Files created by Garmin Forerunner 735XT using Shiny


## Usage
handled by makefile:

1. **fetch:** mounts + unmounts the watch and copies new data to working dir

2. **decode:** created *.RDS and *.png files from FIT-files, which contain correctly converted information from the *.FIT-files, and are directly accessed later by the UI script

3. **stats:** collects information from the new *.RDS files and stores them in a single script (the session statistics of the activity)

4. **launch:** starts the shiny UI - manual access via browser necessary

5. **all:** fetch, decode and stats all at once

6. **backup:** copies files to backup folders (useful for development)
