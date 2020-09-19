library(httr)
library(jsonlite)
library(shiny)
library(reactlog)

reactlog_enable()
app <- system.file("Garmin/server", package = "shiny")
runApp(app)

shiny::reactlogShow()

#s <- readRDS('ACT_RDS/2020-04-25_14-27-59_Biking.rds')
#
#rec <- s$session
#
#typeof(s)
#is.data.frame(rec)

#points <- rec[,c('pos_lat','pos_long')]
#
#
#req <- function(p){
#	points_comb <- paste(p$pos_lat,p$pos_long,sep=',')
#	coords_lst <- paste(points_comb,collapse='|')
#
#	r <- GET("https://api.opentopodata.org/v1/eudem25m?",
#			 query = list(locations = coords_lst))
#	print(http_status(r)$category)
#	r
#}
#
#points_split <- split(points, (as.numeric(rownames(points))-1) %/% 100)
#response <- c()
#for (i in points_split){
#	r <- req(i)
#	response <- c(response,fromJSON(content(r,as="text",encoding="UTF-8"))$results$elevation)
#	#print(fromJSON(content(r,as="text"))$results$elevation)
#}
#s$record$altitude <- unlist(response)



#alt <- s$record$altitude
#asc <- 0
#dsc <- 0
#
#for(i in 1:(length(alt)-1)){
#	if(alt[i] < alt[i+1])
#		asc <- asc + alt[i+1]-alt[i]
#	else
#		dsc <- dsc + alt[i]-alt[i+1]
#}
#alt
#
#asc
#dsc
