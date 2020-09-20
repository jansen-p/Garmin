library(tidyverse)
library(shinyWidgets)
library(ggmap)
library(scales)
library(fullcalendar)

source('src/plots.r')
source('src/helpers.r')

files <- list.files('ACT_RDS')
stats <- readRDS('ACT_RDS/stats.rds')

data <- data.frame(title = paste("Event", 1:3),
                  start = c("2020-09-01", "2020-09-01", "2020-09-15"),
                  #end = c("2020-09-02", "2020-09-04", "2020-09-18"),
                  color = c("red", "blue", "green"))

data2 <- data.frame(title = stats$sport[-1],
					start =unlist(lapply(strsplit(stats$filename,'_'), function (x) unlist(x)[1]))[-1],
					color = unlist(lapply(stats$sport, function(x) if(x=="Running") "Darkyellow" else if(x=="Hiking") "Green" else if(x=="Swimming") "Darkblue" else if(x=="Biking") "Brown"))
					)

ggplot_height <- 600

server <- function(input, output) {

	#file <- data.frame()
	rv <- reactiveValues(cur_file = data.frame(), #file for the current selected event
						 substats = data.frame(),
						 graph_selection = c(), #lists for graphs
						 map = c(), #stamenmap for ggmap
						 inserted_sing = c(),
						 inserted_comp = c()) #currently drawn graphs

	observeEvent(input$event,{
			if(input$sport_type != "Swimming" & is.character(input$event)){
				name <- unlist(strsplit(input$event,' '))
				rv$map <- dget(normalizePath(paste0('ACT_RDS/',gsub("[.]","-",name[1]),'_',gsub(":","-",name[4]),'_',input$sport_type,'_map.RData')))
			}
	})

	plot_ggmap <- function(color,title){
		ggmap(rv$map, extent='panel') +
			geom_path(aes(x = pos_long, y = pos_lat, colour = color),data = rv$cur_file$record, size = 1.4) +
			scale_colour_gradientn(colours = rainbow(3.5)) +
			ggtitle(paste("Map -",title)) + ylab('latitude') + xlab('longitude')
	}

	#draw graphs based on "Choose graphs" selection in Single Events
	observeEvent({input$gs_sing
		input$event
		input$sport_type
		input$height
		input$width
		input$dates},{
			sel <- input$gs_sing

			for (i in sel){
					if (! i %in% rv$inserted_sing){
						insertUI(selector = '#graphs_sing',
								 where = 'afterBegin',
								 if(is.character(input$event) & !is.null(input$event)){
										 if(input$sport_type == "Swimming"){
											 #swimming selection
											 if(i == "pace")
												 ui = tags$div(tags$p(renderPlot(s_swim_pace(rv$cur_file$record))), id = i)
											 else if(i == "ca_sw") #cadence/swolf
												 ui = tags$div(tags$p(renderPlot(s_swim_ca_sw(rv$cur_file$record))), id = i)
											 else if(i == "lap")
												 ui = tags$div(tags$p(renderDataTable(rv$cur_file$lap)),id = i)
										 }
										 else if(input$sport_type == "Running"){
											 if(i == "pace")
												 ui = tags$div(tags$p(renderPlot(s_run_pace(rv$cur_file$record))), id = i)
											 else if(i == "hr")
												 ui = tags$div(tags$p(renderPlot(s_run_hr(rv$cur_file$record))), id = i)
											 else if(i == "lap")
												 ui = tags$div(tags$p(renderDataTable(rv$cur_file$record)),id = i)
											 else if(i == "map")
												 ui = tags$div(tags$p(renderPlot({plot_ggmap(rv$cur_file$record$hr,"Heart Rate")},height=input$height,width=input$width)),id = i)
											 else if(i == "amap")
												 ui = tags$div(tags$p(renderPlot({plot_ggmap(rv$cur_file$record$altitude,"Altitude")},height=input$height,width=input$width)),id = i)
											 else if(i == "smap")
												 ui = tags$div(tags$p(renderPlot({plot_ggmap(rv$cur_file$record$speed,"Speed")},height=input$height,width=input$width)),id = i)
										 }
										 else{ #Biking, Hiking
											 if(i == "dist")
												 ui = tags$div(tags$p(renderPlot(s_r_dist(rv$cur_file$record))), id = i)
											 else if(i == "altitude")
												 ui = tags$div(tags$p(renderPlot(s_r_altitude(rv$cur_file$record))), id = i)
											 else if(i == "lap")
												 ui = tags$div(tags$p(renderDataTable(rv$cur_file$lap)),id = i)

											 else if(i == "map")
												 ui = tags$div(tags$p(renderPlot({plot_ggmap(rv$cur_file$record$hr,"Heart Rate")},height=input$height,width=input$width)),id = i)
											 else if(i == "amap")
											     ui = tags$div(tags$p(renderPlot({plot_ggmap(rv$cur_file$record$altitude,"Altitude")},height=input$height,width=input$width)),id = i)
											 else if(i == "smap")
											     ui = tags$div(tags$p(renderPlot({plot_ggmap(rv$cur_file$record$speed,"Speed")},height=input$height,width=input$width)),id = i)
										 }
								 }
						)
						rv$inserted_sing <<- c(rv$inserted_sing,i)
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
			print(paste(rv$inserted_comp,collapse=' '))

			for (i in sel){
				if (! i %in% rv$inserted_comp){
					insertUI(selector = '#graphs_comp',
						where = 'afterBegin',
						if(is.character(input$event) & !is.null(input$event)){
						   	 if(input$sport_type == "Swimming"){
						   		 if(i == "dist")
						   			 ui = tags$div(tags$p(renderPlot(c_swim_dist(rv$substats))), id = i)
						   		 else if(i == "swolf")
						   			 ui = tags$div(tags$p(renderPlot(c_swim_swolf(rv$substats))), id = i)
						   		 else if(i == "pace")
						   			 ui = tags$div(tags$p(renderPlot(c_swim_pace(rv$substats))), id = i)
						   	 }
						   	 else if(input$sport_type == "Running"){
						   		 if(i == "hr")
						   			 ui = tags$div(tags$p(renderPlot(c_run_hr(rv$substats))), id = i)
						   		 else if(i == "te")
						   			 ui = tags$div(tags$p(renderPlot(c_run_te(rv$substats))), id = i)
						   		 else if(i == "dist")
						   			 ui = tags$div(tags$p(renderPlot(c_run_dist(rv$substats))), id = i)
						   		 else if(i == "pace")
						   			 ui = tags$div(tags$p(renderPlot(c_run_pace(rv$substats))), id = i)
						   	 }
						   	 else{
						   		 if(i == "hr")
						   			 ui = tags$div(tags$p(renderPlot(c_r_hr(rv$substats))), id = i)
						   		 else if(i == "dist")
						   			 ui = tags$div(tags$p(renderPlot(c_r_dist(rv$substats))), id = i)
						   		 else if(i == "te")
						   			 ui = tags$div(tags$p(renderPlot(c_r_te(rv$substats))), id = i)
						   		 else if(i == "speed")
						   			 ui = tags$div(tags$p(renderPlot(c_r_speed(rv$substats))), id = i)
						   		 else if(i == "dur")
						   			 ui = tags$div(tags$p(renderPlot(c_r_dur(rv$substats))), id = i)
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
			rv$graph_selection_sing <- sing_set_list(input$sport_type)
			rv$graph_selection_comp <- comp_set_list(input$sport_type)
	})

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
									  "map", ""),
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
		}
	)


	#stats.rds output for Comparison area
	output$stats <- renderDataTable(
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
