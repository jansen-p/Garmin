	sing_set_list <- function(type){
		#change list for the pickerUI
		if(type == "Swimming"){
			list("Pace"="pace", "Cadence+Swolf"="ca_sw", "Lap"="lap")
		}
		else if(type == "Running"){
			list("Pace"="pace", "Heartrate"="hr", "Altitude"="altitude", "Lap"="lap",
									   		"Map"="map", "Map Alt"="amap", "Map Speed"="smap")
		}
		else{
			list("Speed + HR"="dist", "Altitude"="altitude", "Lap"="lap", 
											"Map"="map", "Map Alt"="amap", "Map Speed"="smap")
		}
	}

	comp_set_list <- function(type){
		#change list for the pickerUI
		if(type == "Swimming"){
			list("Pace"="pace", "Swolf"="swolf", "Distances"="dist")
		}
		else if(type == "Running"){
			list("Pace"="pace", "Distance"="dist", "Training Effect"="te", "Heartrate"="hr")
		}
		else{
			list("Speed"="speed", "Distance"="dist", "Training Effect"="te", "Heartrate"="hr", "Duration"="dur")
		}
	}

	deleteGraphs <- function(ins){
		for (i in ins){
			removeUI(selector = paste0('#',i))
		}
	}
