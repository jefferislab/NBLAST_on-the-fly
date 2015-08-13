library(nat)
library(nat.flybrains)
library(nat.nblast)
library(flycircuit)
library(shiny)
library(shinyRGL)
library(shinysky)
library(ggplot2)

# URL synching
hashProxy <- function(inputoutputID) {
  div(id=inputoutputID,class=inputoutputID,tag("div",""));
}


dps <- read.neuronlistfh(file.path(getOption('flycircuit.datadir'), 'dpscanon_f9dc90ce5b2ffb74af37db1e3a2cb35b.rds'))

neuron_names <- fc_neuron(names(dps))
neuron_ids <- fc_idid(names(dps))

shinyUI(fluidPage(
  HTML(
    '<nav class="navbar navbar-default navbar-fixed-top" role="navigation">
      <div class="container">
        <div class="navbar-header">
          <span class="navbar-brand">NBLAST on-the-fly</span>
        </div>
        <ul class="nav navbar-nav">
          <li>
            <a href="#one-against-all">One against all</a>
          </li>
          <li>
            <a href="#pairwise">Pairwise comparison</a>
          </li>
          <li>
            <a href="#tracing">Upload a tracing</a>
          </li>
          <li>
            <a href="#about">About</a>
          </li>
        </ul>
      </div>
    </nav>'
  ),
  
  
  ###################
  # One against all #
  ###################
  HTML('<a name="one-against-all"></a>'),
  h1("One against all", style="padding-top: 70px;"),
  sidebarLayout(
    sidebarPanel(
      ################
      # URL synching #
      ################
      includeHTML("url.js"),
      hashProxy("hash"),
      
      h3("Instructions"),
      HTML("Select a FlyCircuit neuron to compare against all FlyCircuit neurons, with NBLAST. If the checkbox below is ticked, both forwards and reverse scores will be calculated, normalised and averaged, rather than just using the forwards score. The query neuron will be <b><span style='color: black;'>plotted in black</span></b> in the 3D viewer to the right, alongside the top 10 hits (rainbow coloured from <span style='color: red;'>red = best</span> to <span style='color: #FF0099;'>pink = worst</span>)."),
      h3("Query:"),
      textInput("query_all", "", ""),
      br(),
      br(),
      checkboxInput('use_mean', label="Use mean scores", value=FALSE),
      br(),
      submitButton("NBLAST")
    ),
    
    mainPanel(
      HTML(paste0("<style>", paste0("tr:nth-child(", 2:11, ") { color: ", rainbow(10, alpha=NULL), "; }", collapse="\n"), "</style>")),
      h2("3D view"),
      ###################
      # Loading spinner #
      ###################
      includeCSS("loader.css"),
      HTML("<div class='loader' style='position: absolute; left: 400px; top: 300px; z-index: -10000;'>Loading...</div>"),
      
      webGLOutput("brain3d_all", width="800px", height="800px"),
      h2("NBLAST results"),
      textOutput("nblast_all_query"),
      HTML("<a href='http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/www/how/'>What do these scores mean?</a>"),
      conditionalPanel(condition = "output.nblast_all_complete",
                       downloadButton('nblast_results_all_download', 'Download all scores as CSV')
      ),
      h3("Top 10 hits"),
      tableOutput("nblast_results_all_top10"),
      h3("Top 10 clusters"),
      tableOutput("nblast_results_all_top10_clusters"),
      h3("Score distribution"),
      plotOutput("nblast_results_all")
    )
  ),
  
  div(style="height: 100vh;"),
  
  
  #######################
  # Pairwise comparison #
  #######################
  HTML('<a name="pairwise"></a>'),
  h1("Pairwise comparison", style="padding-top: 70px;"),
  sidebarLayout(  
    sidebarPanel(
      h3("Instructions"),
      HTML("Select two FlyCircuit neurons to compare with NBLAST. The <span style='color: red;'>query neuron will be plotted in red</span> in the 3D viewer to the right, while the <span style='color: blue;'>target neuron will be drawn in blue</span>."),
      h3("Query:"),
      textInput("query_one", "", ""),
      
      h3("Target:"),
      textInput("target_one","", ""),
      br(),
      br(),
      submitButton("NBLAST")
    ),
    
    mainPanel(
      h2("3D view"),
      ###################
      # Loading spinner #
      ###################
      includeCSS("loader.css"),
      HTML("<div class='loader' style='position: absolute; left: 400px; top: 300px; z-index: -10000;'>Loading...</div>"),
      
      webGLOutput("brain3d_one", width="800px", height="800px"),
      h2("NBLAST results"),
      textOutput("nblast_one_query_target"),
      textOutput("nblast_results_one"),
      HTML("<a href='http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/www/how/'>What do these scores mean?</a>")
    )
  ),
  
  div(style="height: 100vh;"),
  
  
  ################
  # User tracing #
  ################
  HTML('<a name="tracing"></a>'),
  h1("Upload a tracing", style="padding-top: 70px;"),
  sidebarLayout(
    sidebarPanel(
      h3("Instructions"),
      HTML("Upload a tracing to compare against all FlyCircuit cluster examplars (or all neurons, if checkbox below is ticked), using NBLAST. The query neuron will be <b><span style='color: black;'>plotted in black</span></b> in the 3D viewer to the right, alongside the top 10 hits (rainbow coloured from <span style='color: red;'>red = best</span> to <span style='color: #FF0099;'>pink = worst</span>)."),
      h3("Query"),
      fileInput('tracing_file', "Your tracing:"),
      selectInput('brain', 'Template brain (FCWB – FlyCircuit; JFRC2 – Janelia FlyLight; IS2 – Cambridge; T1 – Vienna)', c('Select a template brain', 'FCWB', 'JFRC2', 'IS2', 'T1')),
      checkboxInput("mirror", "Mirror?", value=FALSE),
      br(),
      checkboxInput('all_neurons', label="Compare with all neurons, not just exemplars (WARNING: this will take a few minutes)", value=FALSE),
      br(),
      checkboxInput('use_mean_tracing', label="Use mean scores", value=FALSE),
      submitButton("NBLAST")
    ),
    
    mainPanel(
      HTML(paste0("<style>", paste0("tr:nth-child(", 2:11, ") { color: ", rainbow(10, alpha=NULL), "; }", collapse="\n"), "</style>")),
      h2("3D view"),
      ###################
      # Loading spinner #
      ###################
      includeCSS("loader.css"),
      HTML("<div class='loader' style='position: absolute; left: 400px; top: 300px; z-index: -10000;'>Loading...</div>"),
      
      webGLOutput("brain3d_tracing", width="800px", height="800px"),
      h2("NBLAST results"),
      HTML("<a href='http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/www/how/'>What do these scores mean?</a>"),
      conditionalPanel(condition = "output.nblast_tracing_complete",
                       downloadButton('nblast_results_tracing_download', 'Download all scores as CSV')
      ),
      h3("Top 10 hits"),
      tableOutput("nblast_results_tracing_top10"),
      h3("Score distribution"),
      plotOutput("nblast_results_tracing")
    )
  ),
  
  div(style="height: 100vh;"),
  
  #########
  # About #
  #########
  HTML('<a name="about"></a>'),
  h1("About", style="padding-top: 70px;"),
  HTML("This web app accompanies <a href='http://dx.doi.org/10.1101/006346'>Costa et al. (2014) NBLAST: Rapid, sensitive comparison of neuronal structure and construction of neuron family databases</a>. More information on other NBLAST resources is available <a href='http://jefferislab.org/si/nblast'>here</a>. NBLAST on-the-fly acts as a demonstration of the core NBLAST algorithm (package <a href='https://github.com/jefferislab/nat.nblast'>nat.nblast</a>), along with some features of the <a href='https://github.com/jefferis/nat'>NeuroAnatomy Toolbox</a> and its helper packages: <a href='https://github.com/jefferislab/nat.templatebrains'>nat.templatebrains</a> and <a href='https://github.com/jefferislab/nat.flybrains'>nat.flybrains</a>. Other resources available are listed <a href='http://jefferislab.org/si/nblast/www/'>here</a>. For further information on how we convert data between template brains, see <a href='http://jefferislab.org/si/bridging/'>here</a>."),
  h3("Local installation"),
  HTML("Instructions on how to install this app locally are <a href='https://github.com/jefferislab/NBLAST_on-the-fly'>here</a>, and a video demo <a href='http://jefferislab.org/si/nblast/www/demos/#nblast-online'>here</a>."),
  h3("Video demos"),
  HTML("Video demos showing how to use this web app and other related resources are available <a href='http://jefferislab.org/si/nblast/www/demos/'>here</a>."),
  h3("Source code"),
  HTML("The full code for this web app can be downloaded from <a href='https://github.com/jefferislab/NBLAST_online'>GitHub</a>."),
  h3("Preparing own data"),
  HTML("Protocols for <a href='http://cshprotocols.cshlp.org/content/2013/4/pdb.prot071720.full'>immunostaining and imaging fly brains</a>, as well as <a href='http://cshprotocols.cshlp.org/content/2013/4/pdb.prot071738.full'>registration of the resulting images</a> are available from Cold Spring Harbor Protocols. We recommend the use of <a href='http://fiji.sc/Simple_Neurite_Tracer'>Simple Neurite Tracer</a> for tracing neurons from the acquired images, detailed instructions for which are available from <a href='http://fiji.sc/Simple_Neurite_Tracer:_Step-By-Step_Instructions'>here</a>."),
  
  div(style="height: 100vh;")
)
)

