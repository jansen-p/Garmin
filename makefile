RDS_DIR    := data/ACT_RDS
ACT_DIR     := data/ACTIVITY
COU_DIR     := data/COURSES
PRO_DIR     := data/f

#backup ACT_DIR, COU_DIR and RDS_DIR files
backup:
	cp -rf $(ACT_DIR)/* data/.ACTIVITY_backup
	cp -rf $(COU_DIR)/* data/.COURSES_backup
	cp -rf $(RDS_DIR)/* data/.ACT_RDS_backup



#copy FIT files from watch to ACT_DIR and COU_DIR
fetch:
	-src/copy_act_courses.sh

#decode FIT files, creates *.rds files
decode:
	Rscript dec.r > /dev/null 2>&1
	-mv $(PRO_DIR)/* $(ACT_DIR)/

#update the stats.rds file
stats:
	Rscript stats.r > /dev/null 2>&1



#launch the UI
launch:
	Rscript src/launch.r
	#R -e "shiny::runApp('../Garmin')" &

#convert all FIT files, remove stats.rds and recreate it
all: fetch decode stats

clean:
	-rm -r $(RDS_DIR)/*
	-rm -r $(ACT_DIR)/*
	-rm -r $(COU_DIR)/*
	-rm -r $(PRO_DIR)/*
