library(tidyverse)
#library(jsonlite)

#scans the RDS folder and searches for new RDS which haven't been saved to the stats.rds yet
#all files which aren't represented are read in and appended to stats.rds

stats_filename <- "stats.rds"
rds_folder <- "ACT_RDS/"

rds_files <- list.files(rds_folder)

if (stats_filename %in% rds_files){
    #some stats already generated, append new stats
    stats <- readRDS(paste(rds_folder,stats_filename,sep=""))
} else {
    stats <- data.frame("filename"="","sport"="")
}

#not modified anything so far
mod <- FALSE

for (f in rds_files){
	if(grepl("png",f)){
		print(paste("Skipping",f))
		next
	}
    #just extraction of filename
    f_name <- unlist(strsplit(f,"[.]"))[1]
    sport <- unlist(strsplit(f_name,"[_]"))[3]
    
    if(f_name != "stats" & dim(filter(stats, filename == f_name))[1] == 0){
        #only read file if its not represented in stats.rds yet (avoid IO)
        file <- readRDS(paste(rds_folder,f,sep=""))
        
        print(paste("Saving stats of file ",f_name,"...",sep=""))
        
        #insert filename and sporttype
        file$session$filename <- f_name
        file$session$sport <- sport
        
        #append session-row to stats.rds
        stats <- merge(stats,file$session,all=TRUE)
        mod <- TRUE
    }
}

#only save if modified something
if(mod){
    print("Saving updates stats.rds...")
    saveRDS(stats,paste(rds_folder,stats_filename,sep=""))
}
