library(fit)
library(tidyverse)
library(ggmap)
library(httr)
library(ggpubr)
library(jsonlite)

#input: int from FIT, out: string
get_sport <- function(f){
	case_when(
			  f == 2 ~ "Biking",
			  f == 17 ~ "Hiking",
			  f == 1 ~ "Running",
			  f == 5 ~ "Swimming",
			  TRUE ~ "UNKNOWN_SPORT"
	)
}

#convert timestamp to date (weird, but off by only some minutes)
to_date <- function (t) {as.POSIXct(as.numeric(as.character(t)), origin="1990-01-01", tz="GMT+23")}

#array: hours k[1], mins k[2], secs k[3] (from int (seconds))
to_duration <- function (t,x="h") {
	if(x!="s"){
		t <- round(t/1000) #normalize if t comes from f.ex total_elapsed_time
	}
	hours <- t%/%3600
	mins <- t%%3600%/%60
	secs <- t%%60
	paste(hours,":",floor(mins),":",floor(secs),sep="")
}

#km/h -> min/km
to_pace <- function (t){
	h <- 60/t
	mins <- floor(h)
	secs <- round((h-mins)*60)
	ifelse(secs >= 60, paste(mins+1,":",secs-60,sep=""),paste(mins,":",secs,sep=""))
}

#m/s -> km/h
to_kmh <- function (ms) {ms*3.6}

#m/s -> min/100m
to_swimpace <- function (ms) {
	t <- 10**4/(ms*6) #[sec]
	mins <- floor(t)
	secs <- round((t-mins)*60)
	ifelse(secs >= 60, paste(mins+1,":",secs-60,sep=""),paste(mins,":",secs,sep=""))
}

#cadence, time (unit from dfs), poollength [m]
to_swolf <- function (cad,time,length){
	round(cad + (10**5/834)/(10**4/length),1)
}

convert_session <- function(buf,sport){
	if(sport=="Running" | sport=="Hiking"){
		data.frame("avg_hr"            = buf$avg_heart_rate,
				   "max_hr"            = buf$max_heart_rate,
				   "avg_speed"         = round(to_kmh(buf$avg_speed/1000),1),
				   "max_speed"         = round(to_kmh(buf$max_speed/1000),1),
				   "avg_pace"          = to_pace(round(to_kmh(buf$avg_speed/1000),1)),
				   "max_pace"          = to_pace(round(to_kmh(buf$max_speed/1000),1)),
				   "total_ascent"      = buf$total_ascent, # TODO
				   "total_descent"     = buf$total_descent, # TODO
				   "total_distance"    = round(buf$total_distance/10**5,2),
				   "total_time"        = to_duration(buf$total_elapsed_time),
				   "total_time_moving" = to_duration(buf$total_timer_time),
				   "num_laps"          = buf$num_laps,
				   "training_effect"   = buf$total_training_effect/10)
	}
	else if(sport=="Biking"){
		data.frame("avg_hr"            = buf$avg_heart_rate,
				   "max_hr"            = buf$max_heart_rate,
				   "avg_speed"         = round(to_kmh(buf$avg_speed/1000),1),
				   "max_speed"         = round(to_kmh(buf$max_speed/1000),1),
				   "total_ascent"      = buf$total_ascent, # TODO
				   "total_descent"     = buf$total_descent, # TODO
				   "total_distance"    = round(buf$total_distance/10**5,2),
				   "total_time"        = to_duration(buf$total_elapsed_time),
				   "total_time_moving" = to_duration(buf$total_timer_time),
				   "training_effect"   = buf$total_training_effect/10)
	}
	else if(sport=="Swimming"){
		data.frame("avg_cadence"       = buf$avg_cadence, #[Züge/min]
				   "total_cycles"      = buf$total_cycles, #Gesamtzahl Züge
				   "avg_speed"         = to_swimpace(buf$avg_speed),
				   "max_speed"         = to_swimpace(buf$max_speed),
				   "avg_swolf"         = to_swolf(buf$avg_cadence,buf$avg_speed,buf$pool_length),
				   "num_laps"          = buf$num_laps,
				   "pool_length"       = buf$pool_length/100,
				   "total_distance"    = round(buf$total_distance/10**5,2),#[km]
				   "total_time"        = to_duration(buf$total_elapsed_time), #[h:m:s]
				   "total_time_moving" = to_duration(buf$total_timer_time)) #[h:m:s]

	}
}


convert_lap <- function(buf,sport,pool_length=0){
	if(sport=="Running" | sport=="Hiking"){
		data.frame("avg_hr"            = buf$avg_heart_rate,
				   "avg_speed"         = round(to_kmh(buf$avg_speed/1000),1),
				   "max_speed"         = round(to_kmh(buf$max_speed/1000),1),
				   "avg_pace"          = to_pace(round(to_kmh(buf$avg_speed/1000),1)),
				   "total_distance"    = round(buf$total_distance/10**5,2),
				   "total_time"        = to_duration(buf$total_elapsed_time),
				   "total_time_moving" = to_duration(buf$total_timer_time))
	}
	else if(sport=="Biking"){
		data.frame("avg_hr"            = buf$avg_heart_rate,
				   "avg_speed"         = round(to_kmh(buf$avg_speed/1000),1),
				   "max_speed"         = round(to_kmh(buf$max_speed/1000),1),
				   "total_distance"    = round(buf$total_distance/10**5,2),
				   "total_time"        = to_duration(buf$total_elapsed_time),
				   "total_time_moving" = to_duration(buf$total_timer_time))
	}
	else if(sport=="Swimming"){
		data.frame("avg_cadence"       = buf$avg_cadence, #[Züge/min]
				   "total_cycles"      = buf$total_cycles, #Gesamtzahl Züge
				   "avg_speed"         = to_swimpace(buf$avg_speed),
				   "max_speed"         = to_swimpace(buf$max_speed),
				   "num_lengths"       = buf$num_active_lengths,
				   "avg_swolf"         = to_swolf(buf$avg_cadence,buf$avg_speed,pool_length),
				   "total_distance"    = round(buf$total_distance/10**5,2),#[km]
				   "total_time"        = to_duration(buf$total_elapsed_time), #[h:m:s]
				   "total_time_moving" = to_duration(buf$total_timer_time)) #[h:m:s]
	}
}


convert_record <- function(buf,sport){
	if(sport=="Running" | sport=="Hiking"){
		data.frame("distance" = round(buf$distance/10**5,3),
				   "hr"       = buf$heart_rate,
				   "speed"    = round(to_kmh(buf$speed/1000),1),
				   "pace"     = to_pace(round(to_kmh(buf$speed/1000),1)),
				   "pos_lat"  = buf$position_lat,
				   "pos_long" = buf$position_long,
				   "time"     = to_duration(buf$timestamp-file$session$start_time,"s"))
	}
	else if(sport=="Biking"){
		data.frame("distance" = round(buf$distance/10**5,3),
				   "hr"       = buf$heart_rate,
				   "speed"    = round(to_kmh(buf$speed/1000),1),
				   "pos_lat"  = buf$position_lat,
				   "pos_long" = buf$position_long,
				   "time"     = to_duration(buf$timestamp-file$session$start_time,"s"))
	}
}


convert_length <- function(buf,sport){
	if(sport=="Swimming"){
		data.frame("cadence"  = buf$avg_swimming_cadence, #[Züge/min]
				   "speed"    = to_swimpace(buf$avg_speed),
				   "swolf"    = to_swolf(buf$avg_swimming_cadence,buf$avg_speed,file$session$pool_length),
				   #"distance" = round(buf$distance/10**5,3),#[km]
				   "strokes"  = buf$total_strokes,
				   "time"     = to_duration(buf$start_time-file$session$start_time,"s")) #[h:m:s]
	}
}


#remove rows which contain any NA values
clean_frame <- function(buf,sport){
	buf[complete.cases(buf),]
}

#convert dataframe containing pos_lat and pos_long cols into array of altitudes
req <- function(p){
		points_comb <- paste(p$pos_lat,p$pos_long,sep=',')
		coords_lst <- paste(points_comb,collapse='|')

		elev_req <- GET(#"https://api.airmap.com/elevation/v1/ele",
						"https://api.opentopodata.org/v1/eudem25m?",  #dataset covers EU at 25m resolution
					    query = list(locations = coords_lst))
		print(paste("Requesting elevation for",nrow(p),"rows from API:",http_status(elev_req)$category))
		elev_req
}

act_files <- list.files('f/')
#contains all fit files to be processed
fit_files <- c()
for (f in act_files){
	sp <- unlist(strsplit(f,"[.]"))
	#print(sp[1:length(sp)-1])
	if(sp[length(sp)] == "FIT" | sp[length(sp)] == "fit"){
		#file is a fit file! save its name
		fit_files <- c(fit_files,sp[1:length(sp)-1])
	}
}

for (f in fit_files){
	file <- read.fit(paste('f/',f,'.FIT',sep=""))

	file_sport_type <- get_sport(file$sport$sport)
	file_date <- to_date(file$session$start_time)
	file_session <- convert_session(file$session,file_sport_type)
	file_lap     <- convert_lap(file$lap,file_sport_type,ifelse(!is.null(file$session$pool_length),
																file$session$pool_length,0))
	#non-swimming: read record
	if(is.null(file$session$pool_length)){
		file_record  <- convert_record(file$record,file_sport_type)
	}
	#swimming: record for some reason useless, use length table
	else{
		file_record <- convert_length(file$length,file_sport_type)
	}

	out <- c()
	out$session <- clean_frame(file_session)
	out$lap <- clean_frame(file_lap)
	out$record <- clean_frame(file_record)

	filename <- paste0("ACT_RDS/",gsub(":","-",gsub(" ","_",file_date)),"_",file_sport_type)
	print(paste("Processing",filename,"..."))
	print("")

	if(file_sport_type != "Swimming"){
		#add altitude values to record frame
		points <- out$record
		response <- c()
		points_split <- split(points, (as.numeric(rownames(points))-1) %/% 100)
		for (i in points_split){
			r <- req(i)
			warn_for_status(r)
			response <- c(response,fromJSON(content(r,as="text",encoding="UTF-8"))$results$elevation)
		}

		out$record$altitude <- unlist(response)

		#create own total_ascent + total_descent values
		alt <- unlist(response)
		asc <- 0
		dsc <- 0
		for (i in 1:(length(alt)-1)){
			if(alt[i] < alt[i+1])
				asc <- asc + alt[i+1]-alt[i]
			else 
				dsc <- dsc + alt[i]-alt[i+1]
		}
		out$session$total_ascent2 <- asc
		out$session$total_descent2 <- dsc


		#create maps from lon/lat values
		#points <- subset(out$record, complete.cases(out$record))
		map <- get_stamenmap(bbox = c(left=min(points$pos_long)-0.005,
									  bottom=min(points$pos_lat)-0.002,
									  right=max(points$pos_long)+0.005,
									  top=max(points$pos_lat)+0.002), zoom = 14)
		plot <- ggmap(map, extent='panel') +
			geom_path(aes(x = pos_long, y = pos_lat, colour = hr),data = points, size = 1.2) +
			scale_colour_gradientn(colours = rainbow(3.5)) +
			ggtitle("Map") + ylab('latitude') + xlab('longitude')

		#ggplot2::ggsave(paste0(filename,".png"))

		plot2 <- ggmap(map, extent='panel') +
			geom_path(aes(x = pos_long, y = pos_lat, colour = altitude),data = out$record, size = 1.2) +
			scale_colour_gradientn(colours = rainbow(3.5)) +
			ggtitle("Map") + ylab('latitude') + xlab('longitude')

		x <- ggarrange(plot, plot2, ncol = 2, nrow = 1, legend = "bottom")
		ggplot2::ggsave(paste0(filename,"_test.png"))
	}

	saveRDS(out,paste0(filename,".rds"))
	break
}
