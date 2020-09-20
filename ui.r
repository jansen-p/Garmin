library(fullcalendar)
library(shinythemes)
#launch: R -e "shiny::runApp('shiny_test')"

ui <- tagList(
	#shinythemes::themeSelector(),
	h3("hi"),
	titlePanel(div(print("Sports Visualization"),style="color:darkorange")),

	conditionalPanel(condition="input.tabs != 'cal'",
	h3(div(textOutput("sport_type"),style="color:red")),
			#choose sport and date range
			fluidRow(
	    		column(1,selectInput("sport_type", 
	    	             label = "",
	    	             choices = c("Running", 
	    	                         "Biking",
	    	                         "Swimming",
									 "Hiking"),
	    	             selected = "Swimming")
				),
				column(2,dateRangeInput("dates", "",start="2020-02-01"))
				)
	),
	navbarPage(
		theme=shinytheme("darkly"),
		fluid=TRUE,
		position="fixed-top",
	    div(""),
			tabPanel("Single Events",
				sidebarLayout(
	    			sidebarPanel(
						width=3,
						uiOutput("files_sport"),
						uiOutput("graph_selection_sing"),
						h3(div("Overall statistics"),style="color:darkred"),
						fluidRow(
							column(4,htmlOutput("cur_stat_l")),
							column(4,htmlOutput("cur_stat_r")),
						),
	    		  		tableOutput("cur_stat_b")
					),
	
					mainPanel(
						#tableOutput("kp"),
						navbarPage(
							div(""),
							tabPanel("Speed+HR"),
							tabPanel("Altitude"),
							tabPanel("Lap"),
							tabPanel("Map"),
							tabPanel("Map Alt"),
							tabPanel("Map Speed")
						),
						h2(div(textOutput("event_sing")),style="color:darkred"),
						textOutput("graphs_sing")
	    			)
				),
					sliderInput("width", "Plot Width", min = 300, max = 1500, value = 1000),
            		sliderInput("height", "Plot Height", min = 300, max = 1500, value = 600)
	  		),
			tabPanel("Comparison",
				sidebarLayout(
					sidebarPanel(
						width=2,
						uiOutput("graph_selection_comp")
					),
					mainPanel(
						h2(div(textOutput("event_comp")),style="color:darkred"),
						textOutput("graphs_comp"),
				 		dataTableOutput("stats") #temporary 
					)
				)
			),
			tabPanel("Record",
			),
			tabPanel("Calendar",
				column(6,
	    			fullcalendarOutput("calendar", width = "100%", height = "200px")
	  			)
			),
			navbarMenu("More",
  			  tabPanel("Summary"),
  			  "----",
  			  "Section header",
  			  tabPanel("Table")
  			)
	)
)
