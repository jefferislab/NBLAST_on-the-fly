library(shiny)
library(rglwidget)

# URL synching
hashProxy <- function(inputoutputID) {
	div(id=inputoutputID,class=inputoutputID,tag("div",""));
}

shinyUI(navbarPage("NBLAST on-the-fly",




###################
# One against all #
###################
tabPanel("One against all",
	includeCSS("errors.css"),
	sidebarLayout(
		sidebarPanel(
			includeHTML("url.js"),
			hashProxy("hash"),
			h3("Instructions"),
			HTML("Enter a FlyCircuit neuron id to compare against all FlyCircuit neurons, with NBLAST. If the checkbox below is ticked, both forwards and reverse scores will be calculated, normalised and averaged, rather than just using the forwards score. The query neuron will be <b><span style='color: black;'>plotted in black</span></b> in the 3D viewer to the right, alongside the top 10 hits (rainbow coloured from <span style='color: red;'>red = best</span> to <span style='color: #FF0099;'>pink = worst</span>)."),
			h3("Query:"),
			textInput("all_query", "", "fru-M-200266"),
			checkboxInput("all_use_mean", label="Use mean scores", value=FALSE),
			HTML("<i>Using the mean score is useful for finding exact matches, i.e. one in which the target is a good hit for the query and the query is a good hit for the target too. This is particularly useful for clustering neurons into types, rather than, for example, just finding neurons that go through the same tract but branch off differently.</i><br /><br />"),
			submitButton("NBLAST")
		),

		mainPanel(
			HTML(paste0("<style>", paste0("tr:nth-child(", 2:11, ") { color: ", rainbow(10, alpha=NULL), "; }", collapse="\n"), "</style>")),
			h3("3D view"),
			includeCSS("loader.css"),
			HTML("<div class='loader' style='position: absolute; left: 400px; top: 300px; z-index: -10000;'>Loading...</div>"),
			HTML("<div style='position: absolute; left: 220px; top: 270px; z-index: -10000; text-align: center; width: 400px; font-size: 30px;'>Loading...</div>"),
			rglwidgetOutput("view3d_one_against_all", width="800px", height="800px"),
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
			h3("Instructions"),
			HTML("Enter two FlyCircuit neuron ids to compare with NBLAST. The <span style='color: red;'>query neuron will be plotted in red</span> in the 3D viewer to the right, while the <span style='color: blue;'>target neuron will be drawn in blue</span>."),
			h3("Query:"),
			textInput("pairwise_query", "", "fru-M-200266"),
			h3("Target:"),
			textInput("pairwise_target","", "fru-F-900020"),
			submitButton("NBLAST")
		),

		mainPanel(
			h2("3D view"),
			includeCSS("loader.css"),
			HTML("<div class='loader' style='position: absolute; left: 400px; top: 300px; z-index: -10000;'>Loading...</div>"),
			HTML("<div style='position: absolute; left: 220px; top: 270px; z-index: -10000; text-align: center; width: 400px; font-size: 30px;'>Loading...</div>"),
			rglwidgetOutput("view3d_pairwise", width="800px", height="800px"),
			conditionalPanel(condition = "output.pairwise_nblast_complete",
				h2("NBLAST results"),
				textOutput("pairwise_query_target"),
				textOutput("pairwise_results"),
				HTML("<a href='http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/www/how/'>What do these scores mean?</a>")
			)
		)
	)
),




####################
# Upload a tracing #
####################
tabPanel("Upload a tracing",
	sidebarLayout(
		sidebarPanel(
			h3("Instructions"),
			HTML("Upload a tracing or skeletonised neuron to compare against all FlyCircuit cluster examplars (or all neurons, if checkbox below is ticked), using NBLAST. The query neuron will be <b><span style='color: black;'>plotted in black</span></b> in the 3D viewer to the right, alongside the top 10 hits (rainbow coloured from <span style='color: red;'>red = best</span> to <span style='color: #FF0099;'>pink = worst</span>)."),
			h3("Query"),
			HTML("<b><span style='color: red;'>This tracing must be registered to one of the template brains we support (see below).</span></b><br /><br />"),
			fileInput('tracing_file', "Your tracing (e.g. swc) or skeletonised neuron (nrrd file):"),
			selectInput('tracing_brain', 'Template brain (FCWB – FlyCircuit; JFRC2 – Janelia FlyLight; IS2 – Cambridge; T1 – Vienna)', c('Select a template brain', 'FCWB', 'JFRC2', 'IS2', 'T1')),
			checkboxInput("tracing_mirror", "Mirror?", value=FALSE),
			checkboxInput('tracing_all_neurons', label="Compare with all neurons, not just exemplars (WARNING: this will take a few minutes)", value=FALSE),
			checkboxInput('tracing_use_mean', label="Use mean scores", value=FALSE),
			HTML("<i>Using the mean score is useful for finding exact matches, i.e. one in which the target is a good hit for the query and the query is a good hit for the target too. This is particularly useful for clustering neurons into types, rather than, for example, just finding neurons that go through the same tract but branch off differently.</i><br /><br />"),
			submitButton("NBLAST")
		),

		mainPanel(
			h2("3D view"),
			includeCSS("loader.css"),
			HTML("<div class='loader' style='position: absolute; left: 400px; top: 300px; z-index: -10000;'>Loading...</div>"),
			HTML("<div style='position: absolute; left: 220px; top: 270px; z-index: -10000; text-align: center; width: 400px; font-size: 30px;'>Loading...</div>"),
			rglwidgetOutput("view3d_tracing", width="800px", height="800px"),
			conditionalPanel(condition = "output.tracing_nblast_complete",
				h2("NBLAST results"),
				HTML("<a href='http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/www/how/'>What do these scores mean?</a>"),
				br(),
				downloadButton('tracing_nblast_results_download', 'Download all scores as CSV'),
				h3("Top 10 hits"),
				htmlOutput("tracing_nblast_results_viewer"),
				tableOutput("tracing_nblast_results_top10"),
				h3("Score distribution"),
				plotOutput("tracing_nblast_results_plot")
			)
		)
	)
),




########
# GAL4 #
########
tabPanel("GAL4",
	sidebarLayout(
		sidebarPanel(
			h3("Instructions"),
			HTML("Enter a FlyCircuit neuron id to compare against all FlyLight GAL4 lines, with NBLAST."),
			h3("Query:"),
			textInput("gal4_query", "", "TPHMARCM-509F-montage_seg1"),
			h3("Num hits:"),
			sliderInput("gal4_n", "", 1, 100, 10, 1),
			submitButton("NBLAST")
		),

		mainPanel(
			HTML("<style> table{ background-color: #ffffff; } </style>"),
			HTML(paste0("<style>", paste0("tr:nth-child(", 2:11, ") { color: #000000; }", collapse="\n"), "</style>")),
			h2("NBLAST results"),
			h3("Top hits"),
			includeCSS("loader.css"),
			HTML("<div class='loader' style='position: absolute; left: 280px; top: 130px; z-index: -10000;'>Loading...</div>"),
			htmlOutput("gal4_view_all"),
			tableOutput("gal4_hits")
		)
	)
),




#########
# About #
#########
tabPanel("About / Help",
	HTML("This web app accompanies <a href='http://dx.doi.org/10.1101/006346'>Costa et al. (2014) NBLAST: Rapid, sensitive comparison of neuronal structure and construction of neuron family databases</a>. More information on other NBLAST resources is available <a href='http://jefferislab.org/si/nblast'>here</a>. NBLAST on-the-fly acts as a demonstration of the core NBLAST algorithm (package <a href='https://github.com/jefferislab/nat.nblast'>nat.nblast</a>), along with some features of the <a href='https://github.com/jefferis/nat'>NeuroAnatomy Toolbox</a> and its helper packages: <a href='https://github.com/jefferislab/nat.templatebrains'>nat.templatebrains</a> and <a href='https://github.com/jefferislab/nat.flybrains'>nat.flybrains</a>. Other resources available are listed <a href='http://jefferislab.org/si/nblast/www/'>here</a>. For further information on how we convert data between template brains, see <a href='http://jefferislab.org/si/bridging/'>here</a>."),
	h3("Video demos"),
	HTML("Video demos showing how to use this web app and other related resources are available <a href='http://jefferislab.org/si/nblast/www/demos/'>here</a>."),
	h3("More help"),
	HTML("If you require information not contained in the manuscript, you can use the <a href='https://groups.google.com/forum/#!forum/nat-user'>nat-user google group</a> shown below. Searching the archive is available to all. To post a question you will first need to request to join the group.<br />

<iframe id='forum_embed' src='javascript:void(0)' scrolling='no' frameborder='0' width='900' height='700'>
</iframe>

<script type='text/javascript'>
	document.getElementById('forum_embed').src =
		'https://groups.google.com/forum/embed/?place=forum/nat-user' +
		'&showsearch=true&showpopout=true&hideforumtitle=true&h1&fragments=true&parenturl=' +
		encodeURIComponent(window.location.href);
</script>"),
	h3("Local installation"),
	HTML("Instructions on how to install this app locally are <a href='https://github.com/jefferislab/NBLAST_on-the-fly'>here</a>, along with a video demo <a href='http://jefferislab.org/si/nblast/www/demos/#nblast-online'>here</a>."),
	h3("Source code"),
	HTML("The full code for this web app can be downloaded from <a href='https://github.com/jefferislab/NBLAST_online'>GitHub</a>."),
	h3("Preparing own data"),
	HTML("Protocols for <a href='http://cshprotocols.cshlp.org/content/2013/4/pdb.prot071720.full'>immunostaining and imaging fly brains</a>, as well as <a href='http://cshprotocols.cshlp.org/content/2013/4/pdb.prot071738.full'>registration of the resulting images</a> are available from Cold Spring Harbor Protocols. We recommend the use of <a href='http://fiji.sc/Simple_Neurite_Tracer'>Simple Neurite Tracer</a> for tracing neurons from the acquired images, detailed instructions for which are available from <a href='http://fiji.sc/Simple_Neurite_Tracer:_Step-By-Step_Instructions'>here</a>.")
)




))
