library(shiny)
library(rglwidget)

shinyUI(navbarPage("NBLAST on-the-fly",




###################
# One against all #
###################
tabPanel("One against all",
	sidebarLayout(
		sidebarPanel(
		),

		mainPanel(
			rglwidgetOutput("view3d_one_against_all")
		)
	)
),




############
# Pairwise #
############
tabPanel("Pairwise comparison",
	sidebarLayout(
		sidebarPanel(
		),

		mainPanel(
			rglwidgetOutput("view3d_pairwise")
		)
	)
),




####################
# Upload a tracing #
####################
tabPanel("Upload a tracing",
	sidebarLayout(
		sidebarPanel(
		),

		mainPanel(
			rglwidgetOutput("view3d_tracing")
		)
	)
),




########
# GAL4 #
########
tabPanel("GAL4",
	sidebarLayout(
		sidebarPanel(
		),

		mainPanel(
			rglwidgetOutput("view3d_gal4")
		)
	)
),




#########
# About #
#########
tabPanel("About",
	sidebarLayout(
		sidebarPanel(
		),

		mainPanel(
		)
	)
)




))
