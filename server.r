library(tidyverse)
library(shinyWidgets)
library(ggmap)
library(scales)
library(fullcalendar)

files <- list.files('ACT_RDS')
stats <- readRDS('ACT_RDS/stats.rds')

data = data.frame(title = paste("Event", 1:3),
                  start = c("2020-09-01", "2020-09-01", "2020-09-15"),
                  #end = c("2020-09-02", "2020-09-04", "2020-09-18"),
                  color = c("red", "blue", "green"))

data2 <- data.frame(title = stats$sport[-1],
					start =unlist(lapply(strsplit(stats$filename,'_'), function (x) unlist(x)[1]))[-1],
					color = unlist(lapply(stats$sport, function(x) if(x=="Running") "Darkyellow" else if(x=="Hiking") "Green" else if(x=="Swimming") "Darkblue" else if(x=="Biking") "Brown"))
					)

server <- function(input, output) {

	#file <- data.frame()
	rv <- reactiveValues(cur_file = data.frame(), #file for the current selected event
						 substats = data.frame(),
						 graph_selection = c(), #lists for graphs
						 inserted_sing = c(),
						 inserted_comp = c()) #currently drawn graphs

	#draw graphs based on "Choose graphs" selection in Single Events
	observeEvent({input$gs_sing
		input$event
		input$sport_type
		input$dates},{
			sel <- input$gs_sing

			for (i in sel){
				if(input$stat_type == "Single Events"){
					if (! i %in% rv$inserted_sing){
						insertUI(selector = '#graphs_sing',
								 where = 'afterBegin',
								 if(is.character(input$event) & !is.null(input$event)){
										 if(input$sport_type == "Swimming"){
											 #swimming selection
											 if(i == "pace"){
												 ui = tags$div(tags$p(renderPlot(ggplot(rv$cur_file$record,
																						aes(x=as.POSIXct(time,format="%H:%M:%S"),y=as.POSIXct(speed,format="%M:%S")))+
																				 geom_line()+
																				 geom_point()+
																				 scale_y_datetime(labels = date_format("%M:%S"))+
																				 xlab("Time [min:sec]")+ylab("Pace [min:sec]"))), id = i)
											 }
											 else if(i == "ca_sw"){ #cadence/swolf
												 mult <- 3
												 ui = tags$div(tags$p(renderPlot(ggplot(rv$cur_file$record, aes(x=as.POSIXct(time,format="%H:%M:%S")))+
																				 geom_line(aes(y=swolf/mult), color='blue')+
																				 geom_point(aes(y=swolf/mult), color='blue')+
																				 geom_line(aes(y=cadence), color='red')+
																				 geom_point(aes(y=cadence), color='red')+
																				 scale_y_continuous(
																									name = "Cadence (red)",
																									sec.axis = sec_axis(~.*mult, name="Swolf (blue)"))+
																				 xlab("Time [min:sec]"))), id = i)
											 }
										 }
										 else if(input$sport_type == "Running"){
											 if(i == "pace"){
												 ui = tags$div(tags$p(renderPlot(ggplot(rv$cur_file$record, aes(x=as.POSIXct(time, format="%H:%M:%S")))+
																				 geom_line(aes(y=as.POSIXct(pace, format="%M:%S")), color='red')+
																				 geom_point(aes(y=as.POSIXct(pace, format="%M:%S")), color='red')+
																				 scale_y_datetime(labels = date_format("%M:%S"))+
																				 ylab("Pace [min/1km]")+
																				 xlab("Time [min:sec]"))), id = i)
											 }
											 else if(i == "hr"){
												 mult <- 4
												 ui = tags$div(tags$p(renderPlot(ggplot(rv$cur_file$record, aes(x=as.POSIXct(time, format="%H:%M:%S")))+
																				 geom_line(aes(y=hr), color='red')+
																				 ylab("Heartrate [bpm]")+
																				 xlab("Distance"))), id = i)
											 }
											 else if(i == "lap"){
												 ui = tags$div(tags$p(renderTable(rv$cur_file$lap)),id = i)
											 }
											 else if(i == "map"){
												 ui = tags$div(tags$p(renderImage({

							 						 print(paste("Strsplit stuff:",input$event,"null?",is.null(input$event)))
													 name <- unlist(strsplit(input$event,' '))
													 filename <- normalizePath(paste0('ACT_RDS/',gsub("[.]","-",name[1]),'_',gsub(":","-",name[4]),'_',input$sport_type,'_test.png'))
													 list(src = filename,
														  width = 1100)}, deleteFile = FALSE)),id = i)
											 }
											 #else if(i == "amap"){
											 #    ui = tags$div(tags$p(renderImage({
											 #		 name <- unlist(strsplit(input$event,' '))
											 #		 filename <- normalizePath(paste0('ACT_RDS/',gsub("[.]","-",name[1]),'_',gsub(":","-",name[4]),'_',input$sport_type,'_alt.png'))
											 #		 list(src = filename,
											 #			  width = 1100)}, deleteFile = FALSE)),id = i)
											 #}
										 }
										 else{ #Biking, Hiking
											 if(i == "dist"){
												 mult <- 4
												 ui = tags$div(tags$p(renderPlot(ggplot(rv$cur_file$record, aes(x=distance))+
																				 geom_line(aes(y=hr/mult), color='red')+
																				 geom_line(aes(y=speed), color='blue')+
																				 scale_y_continuous(
																									name = "Speed [km/h] (blue)",
																									sec.axis = sec_axis(~.*mult, name="Heart Rate [bpm] (red)"))+
																				 xlab("Distance [km]"))), id = i)
											 }
											 #else if(i == "distance"){
											 #    mult <- 4
											 #    ui = tags$div(tags$p(renderPlot(ggplot(rv$cur_file$record, aes(x=distance))+
											 #									 geom_line(aes(y=hr/mult), color='red')+
											 #									 geom_line(aes(y=speed), color='blue')+
											 #									 scale_y_continuous(
											 #														name = "Speed [km/h] (blue)",
											 #														sec.axis = sec_axis(~.*mult, name="Heart Rate [bpm] (red)"))+
											 #									 xlab("Distance"))), id = i)
											 #}
											 else if(i == "altitude"){
												 ui = tags$div(tags$p(renderPlot(ggplot(rv$cur_file$record, aes(x=distance))+
																				 geom_line(aes(y=altitude), color='blue')+
																				 ylab("Altitude [m]")+
																				 xlab("Distance [km]"))), id = i)
											 }
											 else if(i == "lap"){
												 ui = tags$div(tags$p(renderTable(rv$cur_file$lap)),id = i)
											 }
											 else if(i == "map"){
												 ui = tags$div(tags$p(renderImage({
													 name <- unlist(strsplit(input$event,' '))
													 filename <- normalizePath(paste0('ACT_RDS/',gsub("[.]","-",name[1]),'_',gsub(":","-",name[4]),'_',input$sport_type,'_test.png'))
													 list(src = filename,
														  height = 1100)}, deleteFile = FALSE)),id = i)
											 }
											 #else if(i == "amap"){
											 #    ui = tags$div(tags$p(renderImage({
											 #		 name <- unlist(strsplit(input$event,' '))
											 #		 filename <- normalizePath(paste0('ACT_RDS/',gsub("[.]","-",name[1]),'_',gsub(":","-",name[4]),'_',input$sport_type,'_alt.png'))
											 #		 list(src = filename,
											 #			  height = 800)}, deleteFile = FALSE)),id = i)
											 #}
										 }
								 }
						)
						rv$inserted <<- c(rv$inserted,i)
					}
				}
			}
			#checks all inserted graphics: if they aren't represented in selected selection list, remove them
			for (i in rv$inserted_sing){
				if (! i %in% sel){
					removeUI(
							 selector = paste0('#',i)
					)
					rv$inserted_sing <<- rv$inserted_sing[unlist(lapply(rv$inserted_sing,function(x) x != i))]
				}
			}
	 })
	observeEvent({input$gs_comp
		input$event
		input$sport_type
		input$dates},{
			sel <- input$gs_comp

			for (i in sel){
					if (! i %in% rv$inserted_comp){
						insertUI(selector = '#graphs_comp',
								 where = 'afterBegin',
								 if(is.character(input$event) & !is.null(input$event)){
										 if(input$sport_type == "Swimming"){
											 if(i == "dist"){
												 ui = tags$div(tags$p(renderPlot(ggplot(rv$substats, aes(x=as.POSIXct(substr(rv$substats$filename,1,19), format="%Y-%m-%d_%H-%M-%S")))+
																				 geom_line(aes(y=total_distance), color='red')+
																				 geom_point(aes(y=total_distance), color='red')+
																				 facet_wrap(~pool_length)+
																				 ylab("Distance")+
																				 xlab("Time"))), id = i)
											 }
											 else if(i == "swolf"){
												 mult <- 1/4
												 ui = tags$div(tags$p(renderPlot(ggplot(rv$substats, aes(x=as.POSIXct(substr(rv$substats$filename,1,19), format="%Y-%m-%d_%H-%M-%S")))+
																				 geom_line(aes(y=avg_swolf), color='blue')+
																				 geom_point(aes(y=avg_swolf), color='blue')+
																				 geom_line(aes(y=avg_cadence/mult), color='red')+
																				 geom_point(aes(y=avg_cadence/mult), color='red')+
																				 scale_y_continuous(
																									name = "Swolf (blue)",
																									sec.axis = sec_axis(~.*mult, name="Cadence (red)"))+
																				 facet_wrap(~pool_length)+
																				 ylab("Swolf")+
																				 xlab("Time"))), id = i)
											 }
											 else if(i == "pace"){
												 ui = tags$div(tags$p(renderPlot(ggplot(rv$substats, aes(x=as.POSIXct(substr(rv$substats$filename,1,19), format="%Y-%m-%d_%H-%M-%S")))+
																				 geom_line(aes(y=as.POSIXct(avg_speed, format="%M:%S")), color='blue')+
																				 geom_point(aes(y=as.POSIXct(avg_speed, format="%M:%S")), color='blue')+
																				 geom_line(aes(y=as.POSIXct(max_speed, format="%M:%S")), color='red')+
																				 geom_point(aes(y=as.POSIXct(max_speed, format="%M:%S")), color='red')+
																				 scale_y_datetime(labels = date_format("%M:%S"))+
																				 ylab("Max. Pace (red)         Avg. Pace (blue)")+
																				 xlab("Time"))), id = i)
											 }
										 }
										 else if(input$sport_type == "Running"){
											 if(i == "hr"){
												 ui = tags$div(tags$p(renderPlot(ggplot(rv$substats,aes(x=as.POSIXct(substr(rv$substats$filename,1,19), format="%Y-%m-%d_%H-%M-%S")))+
																				 geom_line(aes(y=max_hr), color='red')+
																				 geom_point(aes(y=max_hr), color='red')+
																				 geom_line(aes(y=avg_hr), color='blue')+
																				 geom_point(aes(y=avg_hr), color='blue')+
																				 scale_y_continuous(
																									name = "Avg. Heart Rate [bpm] (blue)",
																									sec.axis = sec_axis(~., name="Max. Heart Rate [bpm] (red)"))+
																				 xlab("Time [hour:min:sec]"))), id = i)
											 }
											 else if(i == "te"){
												 ui = tags$div(tags$p(renderPlot(ggplot(rv$substats, aes(x=as.POSIXct(substr(rv$substats$filename,1,19), format="%Y-%m-%d_%H-%M-%S")))+
																				 geom_line(aes(y=training_effect), color='red')+
																				 geom_point(aes(y=training_effect), color='red')+
																				 ylab("Training effect")+
																				 xlab("Time"))), id = i)
											 }
											 else if(i == "dist"){
												 ui = tags$div(tags$p(renderPlot(ggplot(rv$substats, aes(x=as.POSIXct(substr(rv$substats$filename,1,19), format="%Y-%m-%d_%H-%M-%S")))+
																				 geom_line(aes(y=total_distance), color='blue')+
																				 geom_point(aes(y=total_distance), color='blue')+
																				 ylab("Distance [km]")+
																				 xlab("Date"))), id = i)
											 }
											 else if(i == "pace"){
												 ui = tags$div(tags$p(renderPlot(ggplot(rv$substats, aes(x=as.POSIXct(substr(rv$substats$filename,1,19), format="%Y-%m-%d_%H-%M-%S")))+
																				 geom_line(aes(y=as.POSIXct(avg_pace, format="%M:%S")), color='blue')+
																				 geom_point(aes(y=as.POSIXct(avg_pace, format="%M:%S")), color='blue')+
																				 geom_line(aes(y=as.POSIXct(max_pace, format="%M:%S")), color='red')+
																				 geom_point(aes(y=as.POSIXct(max_pace, format="%M:%S")), color='red')+
																				 scale_y_datetime(labels = date_format("%M:%S"))+
																				 ylab("Max. Pace (red)         Avg. Pace (blue)")+
																				 xlab("Time"))), id = i)
											 }
										 }
										 else{
											 if(i == "hr"){
												 ui = tags$div(tags$p(renderPlot(ggplot(rv$substats,aes(x=as.POSIXct(substr(rv$substats$filename,1,19), format="%Y-%m-%d_%H-%M-%S")))+
																				 geom_line(aes(y=max_hr), color='red')+
																				 geom_point(aes(y=max_hr), color='red')+
																				 geom_line(aes(y=avg_hr), color='blue')+
																				 geom_point(aes(y=avg_hr), color='blue')+
																				 scale_y_continuous(
																									name = "Avg. Heart Rate [bpm] (blue)",
																									sec.axis = sec_axis(~., name="Max. Heart Rate [bpm] (red)"))+
																				 xlab("Time [hour:min:sec]"))), id = i)
											 }
											 else if(i == "dist"){
												 ui = tags$div(tags$p(renderPlot(ggplot(rv$substats, aes(x=as.POSIXct(substr(rv$substats$filename,1,19), format="%Y-%m-%d_%H-%M-%S")))+
																				 geom_line(aes(y=total_distance), color='blue')+
																				 geom_point(aes(y=total_distance), color='blue')+
																				 ylab("Distance [km]")+
																				 xlab("Date"))), id = i)
											 }
											 else if(i == "te"){
												 ui = tags$div(tags$p(renderPlot(ggplot(rv$substats, aes(x=as.POSIXct(substr(rv$substats$filename,1,19), format="%Y-%m-%d_%H-%M-%S")))+
																				 geom_line(aes(y=training_effect), color='red')+
																				 geom_point(aes(y=training_effect), color='red')+
																				 ylab("Training effect")+
																				 xlab("Time"))), id = i)
											 }
											 else if(i == "speed"){
												 ui = tags$div(tags$p(renderPlot(ggplot(rv$substats, aes(x=as.POSIXct(substr(rv$substats$filename,1,19), format="%Y-%m-%d_%H-%M-%S")))+
																				 geom_line(aes(y=as.numeric(as.character(max_speed))), color='red')+
																				 geom_point(aes(y=as.numeric(as.character(max_speed))), color='red')+
																				 geom_line(aes(y=as.numeric(as.character(avg_speed))), color='blue')+
																				 geom_point(aes(y=as.numeric(as.character(avg_speed))), color='blue')+
																				 scale_y_continuous(
																									name = "Avg. Speed [km/h] (blue)",
																									sec.axis = sec_axis(~., name="Max. Speed [km/h] (red)"))+
																				 xlab("Time"))), id = i)
											 }
											 else if(i == "dur"){
												 ui = tags$div(tags$p(renderPlot(ggplot(rv$substats, aes(x=as.POSIXct(substr(rv$substats$filename,1,19), format="%Y-%m-%d_%H-%M-%S")))+
																				 geom_line(aes(y=as.POSIXct(total_time, format="%H:%M:%S")), color='blue')+
																				 geom_point(aes(y=as.POSIXct(total_time, format="%H:%M:%S")), color='blue')+
																				 geom_line(aes(y=as.POSIXct(total_time_moving, format="%H:%M:%S")), color='red')+
																				 geom_point(aes(y=as.POSIXct(total_time_moving, format="%H:%M:%S")), color='red')+
																				 #scale_y_datetime(labels = date_format("%H:%M"))+
																				 ylab("Time moving (red)         Total time (blue) [h:m]")+
																				 xlab("Time"))), id = i)
											 }
										 }
								 }
						)
						rv$inserted_comp <<- c(rv$inserted_comp,i)
					}
			}
			#checks all inserted graphics: if they aren't represented in selected selection list, remove them
			for (i in rv$inserted_comp){
				if (! i %in% sel){
					removeUI(
							 selector = paste0('#',i)
					)
					rv$inserted_comp <<- rv$inserted_comp[unlist(lapply(rv$inserted_comp,function(x) x != i))]
				}
			}
	})


	# TODO: remove
	output$kp <- renderTable(
							 rv$cur_file$session
	)


	#set the substats dataframe, depending on date, sporttype and screen
	observeEvent({input$dates
		input$sport_type},{
			f <- Filter(function(x) !all(is.na(x)), filter(stats,sport == input$sport_type))
			rv$substats <- f[sapply(f$filename, function(x) input$dates[1] <= x & x <= input$dates[2]),]
	})

	#reset drawn graphs and set list according to selected datatype/sport
	observeEvent(input$sport_type,{
			deleteGraphs(rv$inserted_sing)
			deleteGraphs(rv$inserted_comp)
			rv$inserted_sing <- c()
			rv$inserted_comp <- c()
			update_graph_list_sing()
			update_graph_list_comp()
	})
	update_graph_list_sing <- function(){
		#change list for the pickerUI
		if(input$sport_type == "Swimming"){
			rv$graph_selection_sing <- list("Pace"="pace",
									   "Cadence+Swolf"="ca_sw",
									   "Lap"="lap")
		}
		else if(input$sport_type == "Running"){
			rv$graph_selection_sing <- list("Pace"="pace", #contains speed + heartrate
									   "Heartrate"="hr", # same, but depends on dist.
									   "Altitude"="altitude",
									   "Lap"="lap",
									   "Map"="map")
			#"Map - Alt"="amap")
		}
		else{
			rv$graph_selection_sing <- list("Speed + HR"="dist", #contains speed + heartrate
									   "Altitude"="altitude",
									   "Lap"="lap",
									   "Map"="map")
			#"Map - Alt"="amap")
		}
	}
	update_graph_list_comp <- function(){
		#change list for the pickerUI
		if(input$sport_type == "Swimming"){
			rv$graph_selection_comp <- list("Pace"="pace",
									   "Swolf"="swolf",
									   "Distances"="dist")
		}
		else if(input$sport_type == "Running"){
			rv$graph_selection_comp <- list("Pace"="pace",
									   "Distance"="dist",
									   "Training Effect"="te",
									   "Heartrate"="hr")
		}
		else{
			rv$graph_selection_comp <- list("Speed"="speed",
									   "Distance"="dist",
									   "Training Effect"="te",
									   "Heartrate"="hr",
									   "Duration"="dur")
		}
	}
	deleteGraphs <- function(ins){
		for (i in ins){
			removeUI(selector = paste0('#',i))
		}
	}

	#read file matching to selected event
	observeEvent(input$event,{
					 if(is.character(input$event)){
						 name <- unlist(strsplit(input$event,' '))
						 d <- gsub("[.]","-",name[1])
						 t <- gsub(":","-",name[4])
						 rv$cur_file <- readRDS(paste('ACT_RDS/',d,'_',t,'_',input$sport_type,'.rds',sep=""))
					 }
										   })

	#Single Event - Event selection title
	output$date_range <- renderText({
		paste("Event range:",input$dates[1],"-",input$dates[2])
	})
	#Single Events - Event selection rendering
	output$files_sport <- renderUI({
		f <- grep(paste("*",input$sport_type,".rds",sep=""),files,value=T)
		f <- f[sapply(f, function(x) input$dates[1] <= x & x <= input$dates[2])]

		cleaned_f <- unlist(lapply(f,function(x) paste(gsub("-",".",substr(x,1,10)),
													   "- ",
													   gsub("-",":",substr(x,12,19)))))
		pickerInput("event", textOutput("date_range"),
					choices=cleaned_f,
					#choices=c("event1", "event2", "event3"),
					options = list(`actions-box` = TRUE),
					multiple = FALSE
		)
	})

	#print back the sport_type sent by selection
	output$sport_type <- renderText({
		print(input$sport_type)
	})

	#Choose graphs rendering
	output$graph_selection_sing <- renderUI({
		pickerInput("gs_sing","Choose graphs:",
					choices = rv$graph_selection_sing,
					options = list(`actions-box` = TRUE),
					selected = ifelse(input$sport_type != "Swimming",   #causes errors (jumps over nullcheck of input$event)
									  "map", "pace"),
					multiple = TRUE,
		)
	})
	output$graph_selection_comp <- renderUI({
		pickerInput("gs_comp","Choose graphs:",
					choices = rv$graph_selection_comp,
					options = list(`actions-box` = TRUE),
					selected = ifelse(input$sport_type != "Swimming",   #causes errors (jumps over nullcheck of input$event)
									  "map", "pace"),
					multiple = TRUE,
		)
	})


	#session stats of current file (Single Events)
	output$cur_stat_l <- renderUI({
		if(!is.null(input$event)){
			HTML(paste0("<b>Distance</b> <p style='color:green'>",rv$cur_file$session$total_distance," km</p><br/>",
						"<b>Total time</b> <p style='color:green'>",rv$cur_file$session$total_time,
						"</p><b>Time moving</b> <p style='color:green'>",rv$cur_file$session$total_time_moving,"</p><br/>"))
		}
	})
	output$cur_stat_r <- renderUI({
		if(!is.null(input$event)){
			HTML(ifelse(input$sport_type != "Swimming",
							  paste0(" <b>Training effect</b> <p style='color:green'>",rv$cur_file$session$training_effect,"</p><br/>",
									 "<b>Total ascent</b> <p style='color:green'>",round(rv$cur_file$session$total_ascent2,1),
									 " m</p><b>Total descent</b> <p style='color:green'>",round(rv$cur_file$session$total_descent2,1)," m<p><br/>"),
							  paste0(" <b>Pool length</b> <p style='color:green'>",rv$cur_file$session$pool_length," m</p><br/>",
									 "<b>Avg cadence</b> <p style='color:green'>",rv$cur_file$session$avg_cadence,
									 "</p><b>Total cycles</b> <p style='color:green'>",rv$cur_file$session$total_cycles,"<p><br/>")
							  ))
		}
	})
	output$cur_stat_b <- renderTable(
		if(!is.null(input$event) & is.list(rv$cur_file) & is.data.frame(rv$cur_file$record)){
			print(paste(input$event))
		    if(input$sport_type == "Swimming" & !is.null(rv$cur_file$record$swolf)){
		   	 data.frame("."=c("Average","Maximum"),
		   				"Speed"=c(rv$cur_file$session$avg_speed,rv$cur_file$session$max_speed),
		   				"Swolf"=c(rv$cur_file$session$avg_swolf,max(rv$cur_file$record$swolf,na.rm=T)))
		    }
		    else if(input$sport_type == "Running"){
		   	 data.frame("."=c("Average","Maximum"),
		   				"Pace"=c(rv$cur_file$session$avg_pace,rv$cur_file$session$max_pace),
		   				"Heartrate"=c(rv$cur_file$session$avg_hr,rv$cur_file$session$max_hr))
		    }
		    else{
		   	 data.frame("."=c("Average","Maximum"),
		   				"Speed kmh"=c(rv$cur_file$session$avg_speed,rv$cur_file$session$max_speed),
		   				"Heartrate"=c(rv$cur_file$session$avg_hr,rv$cur_file$session$max_hr))
		    }
		    #"Heartrate"=c(rv$cur_file$session$avg_hr,rv$cur_file$session$max_hr))
		}
	)


	#stats.rds output for Comparison area
	output$stats <- renderTable(
								#filters out stats of current sport_type, then removes the columns where all values are NA
								rv$substats
	)

	output$calendar <- renderFullcalendar({
	    fullcalendar(data2)
	})

	#Title for main page
	output$event_sing <- renderText({
		ifelse(is.null(input$event),"No Event found!",paste("Graphs -",input$event))
	})
	output$event_comp <- renderText({
		case_when(
			  input$stat_type == 'Comparison' ~ "Graphs",
			  TRUE ~ "No Event found!")
	})
}
