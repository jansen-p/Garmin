conv_datetime <- function(data, f="%Y-%m-%d_%H-%M-%S"){
	as.POSIXct(substr(data,1,19), format=f)
}


s_swim_pace <- function(data){
	ggplot(data, aes(x=as.POSIXct(time,format="%H:%M:%S")))+
		geom_line(aes(y=as.POSIXct(speed,format="%M:%S")))+
		geom_point(aes(y=as.POSIXct(speed,format="%M:%S")))+
		scale_y_datetime(labels = date_format("%M:%S"))+
		xlab("Time [min:sec]")+ylab("Pace [min:sec]")
}
s_swim_ca_sw <- function(data){
	mult <- 3
	ggplot(data, aes(x=as.POSIXct(time,format="%H:%M:%S")))+
		geom_line(aes(y=swolf/mult), color='blue')+
		geom_point(aes(y=swolf/mult), color='blue')+
		geom_line(aes(y=cadence), color='red')+
		geom_point(aes(y=cadence), color='red')+
		scale_y_continuous(name = "Cadence (red)",
						   sec.axis = sec_axis(~.*mult, name="Swolf (blue)"))+
		xlab("Time [min:sec]")
}
s_run_pace <- function(data){
	ggplot(data, aes(x=as.POSIXct(time, format="%H:%M:%S")))+
		geom_line(aes(y=as.POSIXct(pace, format="%M:%S")), color='red')+
		geom_point(aes(y=as.POSIXct(pace, format="%M:%S")), color='red')+
		scale_y_datetime(labels = date_format("%M:%S"))+
		ylab("Pace [min/1km]")+
		xlab("Time [min:sec]")
}
s_run_hr <- function(data){
	ggplot(data, aes(x=as.POSIXct(time, format="%H:%M:%S")))+
		geom_line(aes(y=hr), color='red')+
		ylab("Heartrate [bpm]")+
		xlab("Distance")
}
s_r_dist <- function(data){
	mult <- 4
	ggplot(data, aes(x=distance))+
		geom_line(aes(y=hr/mult), color='red')+
		geom_line(aes(y=speed), color='blue')+
		scale_y_continuous(name = "Speed [km/h] (blue)",
						   sec.axis = sec_axis(~.*mult, name="Heart Rate [bpm] (red)"))+
		xlab("Distance [km]")
}
s_r_altitude <- function(data){
	ggplot(data, aes(x=distance))+
		geom_line(aes(y=altitude), color='blue')+
		ylab("Altitude [m]")+
		xlab("Distance [km]")
}






c_swim_dist <- function(data){
	ggplot(data, aes(x=conv_datetime(filename),y=total_distance))+
		geom_line(color='red')+geom_point(color='red')+facet_wrap(~pool_length)+
		labs(x="Time",y="Distance")
}
c_swim_swolf <- function(data){
	mult <- 1/4
	ggplot(data, aes(x=conv_datetime(filename)))+
		geom_line(aes(y=avg_swolf), color='blue')+
		geom_point(aes(y=avg_swolf), color='blue')+
		geom_line(aes(y=avg_cadence/mult), color='red')+
		geom_point(aes(y=avg_cadence/mult), color='red')+
		scale_y_continuous(
						   name = "Swolf (blue)",
						   sec.axis = sec_axis(~.*mult, name="Cadence (red)"))+
facet_wrap(~pool_length)+
ylab("Swolf")+
xlab("Time")
}
c_swim_pace <- function(data){
	ggplot(data, aes(x=conv_datetime(filename)))+
		geom_line(aes(y=as.POSIXct(avg_speed, format="%M:%S")), color='blue')+
		geom_point(aes(y=as.POSIXct(avg_speed, format="%M:%S")), color='blue')+
		geom_line(aes(y=as.POSIXct(max_speed, format="%M:%S")), color='red')+
		geom_point(aes(y=as.POSIXct(max_speed, format="%M:%S")), color='red')+
		scale_y_datetime(labels = date_format("%M:%S"))+
		ylab("Max. Pace (red)         Avg. Pace (blue)")+
		xlab("Time")
}
c_run_hr <- function(data){
	ggplot(data,aes(x=conv_datetime(filename)))+
		geom_line(aes(y=max_hr), color='red')+
		geom_point(aes(y=max_hr), color='red')+
		geom_line(aes(y=avg_hr), color='blue')+
		geom_point(aes(y=avg_hr), color='blue')+
		scale_y_continuous(name = "Avg. Heart Rate [bpm] (blue)",
						   sec.axis = sec_axis(~., name="Max. Heart Rate [bpm] (red)"))+
xlab("Time [hour:min:sec]")
}
c_run_te <- function(data){
	ggplot(data, aes(x=conv_datetime(filename),y=training_effect))+
		geom_line(color='red')+geom_point(color='red')+labs(x="Time",y="Training effect")
}
c_run_dist <- function(data){
	ggplot(data, aes(x=conv_datetime(filename),y=total_distance))+
		geom_line(color='blue')+geom_point(color='blue')+labs(x="Date",y="Distance [km]")
}
c_run_pace <- function(data){
	ggplot(data, aes(x=conv_datetime(filename)))+
		geom_line(aes(y=as.POSIXct(avg_pace, format="%M:%S")), color='blue')+
		geom_point(aes(y=as.POSIXct(avg_pace, format="%M:%S")), color='blue')+
		geom_line(aes(y=as.POSIXct(max_pace, format="%M:%S")), color='red')+
		geom_point(aes(y=as.POSIXct(max_pace, format="%M:%S")), color='red')+
		scale_y_datetime(labels = date_format("%M:%S"))+
		ylab("Max. Pace (red)         Avg. Pace (blue)")+
		xlab("Time")
}

c_r_hr <- function(data){
	ggplot(data,aes(x=conv_datetime(filename)))+
		geom_line(aes(y=max_hr), color='red')+geom_point(aes(y=max_hr), color='red')+
		geom_line(aes(y=avg_hr), color='blue')+geom_point(aes(y=avg_hr), color='blue')+
		scale_y_continuous(name = "Avg. Heart Rate [bpm] (blue)",
						   sec.axis = sec_axis(~., name="Max. Heart Rate [bpm] (red)"))+
xlab("Time [hour:min:sec]")
}
c_r_dist <- function(data){
	ggplot(data, aes(x=conv_datetime(filename),y=total_distance))+
		geom_line(color='blue')+geom_point(color='blue')+labs(x="Date",y="Distance [km]")
}
c_r_te <- function(data){
	ggplot(data, aes(x=conv_datetime(filename),y=training_effect))+
		geom_line(color='red')+geom_point(color='red')+labs(x="Time",y="Training effect")
}
c_r_speed <- function(data){
	ggplot(data, aes(x=conv_datetime(filename)))+
		geom_line(aes(y=as.numeric(as.character(max_speed))), color='red')+
		geom_point(aes(y=as.numeric(as.character(max_speed))), color='red')+
		geom_line(aes(y=as.numeric(as.character(avg_speed))), color='blue')+
		geom_point(aes(y=as.numeric(as.character(avg_speed))), color='blue')+
		scale_y_continuous(
						   name = "Avg. Speed [km/h] (blue)",
						   sec.axis = sec_axis(~., name="Max. Speed [km/h] (red)"))+
xlab("Time")
}
c_r_dur <- function(data){
	ggplot(data, aes(x=conv_datetime(filename)))+
		geom_line(aes(y=as.POSIXct(total_time, format="%H:%M:%S")), color='blue')+
		geom_point(aes(y=as.POSIXct(total_time, format="%H:%M:%S")), color='blue')+
		geom_line(aes(y=as.POSIXct(total_time_moving, format="%H:%M:%S")), color='red')+
		geom_point(aes(y=as.POSIXct(total_time_moving, format="%H:%M:%S")), color='red')+
		#scale_y_datetime(labels = date_format("%H:%M"))+
		ylab("Time moving (red)         Total time (blue) [h:m]")+
		xlab("Time")
}
