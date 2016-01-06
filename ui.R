library(shiny)
library(rglwidget)

shinyUI(navbarPage("NBLAST on-the-fly",




###################
# One against all #
###################
tabPanel("One against all",
	sidebarLayout(
		sidebarPanel(
			h3("Instructions"),
			HTML("Select a FlyCircuit neuron to compare against all FlyCircuit neurons, with NBLAST. If the checkbox below is ticked, both forwards and reverse scores will be calculated, normalised and averaged, rather than just using the forwards score. The query neuron will be <b><span style='color: black;'>plotted in black</span></b> in the 3D viewer to the right, alongside the top 10 hits (rainbow coloured from <span style='color: red;'>red = best</span> to <span style='color: #FF0099;'>pink = worst</span>)."),
			h3("Query:"),
			textInput("all_query", "", ""),
			checkboxInput("all_use_mean", label="Use mean scores", value=FALSE),
			submitButton("NBLAST")
		),

		mainPanel(
			HTML(paste0("<style>", paste0("tr:nth-child(", 2:11, ") { color: ", rainbow(10, alpha=NULL), "; }", collapse="\n"), "</style>")),
			h3("3D view"),
			rglwidgetOutput("view3d_one_against_all"),
			conditionalPanel(condition = "output.all_nblast_complete",
				h2("NBLAST results"),
				HTML("<a href='http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/www/how/'>What do these scores mean?</a>"),
				br(),
				downloadButton('all_download', 'Download all scores as CSV'),
				h3("Top 10 hits"),
				htmlOutput("all_vfb_viewer"),
				tableOutput("all_top10_hits"),
				h3("Top 10 clusters"),
				tableOutput("all_top10_clusters"),
				h3("Score distribution"),
				plotOutput("all_distribution")
			)
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
