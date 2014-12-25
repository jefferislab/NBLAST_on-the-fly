library(nat)
library(nat.flybrains)
library(nat.nblast)
library(flycircuit)
library(shiny)
library(shinyRGL)
library(shinysky)
library(ggplot2)
library(shinyIncubator)

dps <- read.neuronlistfh(file.path(getOption('flycircuit.datadir'), 'dpscanon_f9dc90ce5b2ffb74af37db1e3a2cb35b.rds'))

neuron_names <- fc_neuron(names(dps))
neuron_ids <- fc_idid(names(dps))

shinyUI(navbarPage("NBLAST on-the-fly",
  #######################
  # Pairwise comparison #
  #######################
  tabPanel("Pairwise comparison",
    sidebarLayout(  
      sidebarPanel(
        h3("Instructions"),
        HTML("Select two FlyCircuit neurons to compare with NBLAST. The <span style='color: red;'>query neuron will be plotted in red</span> in the 3D viewer to the right, while the <span style='color: blue;'>target neuron will be drawn in blue</span>."),
        h3("Query:"),
        textInput.typeahead(
          id="query_one",
          placeholder="Type a FlyCircuit neuron name",
          local=data.frame(name=neuron_names, id=neuron_ids),
          valueKey = "name",
          tokens=neuron_ids,
          template = HTML("<p class='repo-language'>{{id}}</p> <p class='repo-name'>{{name}}</p>")
        ),
        
        h3("Target:"),
        textInput.typeahead(
          id="target_one",
          placeholder="Type a FlyCircuit neuron name",
          local=data.frame(name=neuron_names, id=neuron_ids),
          valueKey = "name",
          tokens=neuron_ids,
          template = HTML("<p class='repo-language'>{{id}}</p> <p class='repo-name'>{{name}}</p>")
        ),
        br(),
        br(),
        submitButton("NBLAST")
      ),
      
      mainPanel(
        h2("3D view"),
        webGLOutput("brain3d_one", width="800px", height="600px"),
        h2("NBLAST results"),
        textOutput("nblast_results_one")
      )
    )
  ),
  
  
  ###################
  # One against all #
  ###################
  tabPanel("One against all",
    sidebarLayout(  
      sidebarPanel(
        h3("Instructions"),
        HTML("Select a FlyCircuit neuron to compare against all FlyCircuit neurons, with NBLAST. If the checkbox below is ticked, both forwards and reverse scores will be calculated, normalised and averaged, rather than just using the forwards score. The query neuron will be <b><span style='color: black;'>plotted in black</span></b> in the 3D viewer to the right, alongside the top 10 hits (rainbow coloured from <span style='color: red;'>red = best</span> to <span style='color: #FF0099;'>pink = worst</span>)."),
        h3("Query:"),
        textInput.typeahead(
          id="query_all",
          placeholder="Type a FlyCircuit neuron name",
          local=data.frame(name=neuron_names, id=neuron_ids),
          valueKey = "name",
          tokens=neuron_ids,
          template = HTML("<p class='repo-language'>{{id}}</p> <p class='repo-name'>{{name}}</p>")
        ),
        br(),
        br(),
        checkboxInput('use_mean', label="Use mean scores", value=FALSE),
        br(),
        submitButton("NBLAST")
      ),
      
      mainPanel(
        HTML(paste0("<style>", paste0("tr:nth-child(", 2:11, ") { color: ", rainbow(10, alpha=NULL), "; }", collapse="\n"), "</style>")),
        progressInit(),
        h2("3D view"),
        webGLOutput("brain3d_all", width="800px", height="600px"),
        h2("NBLAST results"),
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
    )
  ),
  
  
  ################
  # User tracing #
  ################
  tabPanel("Upload a tracing",
    sidebarLayout(  
      sidebarPanel(
        h3("Instructions"),
        HTML("Upload a tracing to compare against all FlyCircuit cluster examplars (or all neurons, if checkbox below is ticked), using NBLAST. The query neuron will be <b><span style='color: black;'>plotted in black</span></b> in the 3D viewer to the right, alongside the top 10 hits (rainbow coloured from <span style='color: red;'>red = best</span> to <span style='color: #FF0099;'>pink = worst</span>)."),
        h3("Query"),
        fileInput('tracing_file', "Your tracing:"),
        selectInput('brain', 'Template brain (FCWB – FlyCircuit; JFRC2 – Janelia FlyLight; IS2 – Cambridge; T1 – Vienna)', c('Select a template brain', 'FCWB', 'JFRC2', 'IS2', 'T1')),
        br(),
        checkboxInput('all_neurons', label="Compare with all neurons, not just exemplars (WARNING: this will take a few minutes)", value=FALSE),
        br(),
        submitButton("NBLAST")
      ),
      
      mainPanel(
        HTML(paste0("<style>", paste0("tr:nth-child(", 2:11, ") { color: ", rainbow(10, alpha=NULL), "; }", collapse="\n"), "</style>")),
        progressInit(),
        h2("3D view"),
        webGLOutput("brain3d_tracing", width="800px", height="600px"),
        h2("NBLAST results"),
        conditionalPanel(condition = "output.nblast_tracing_complete",
          downloadButton('nblast_results_tracing_download', 'Download all scores as CSV')
        ),
        h3("Top 10 hits"),
        tableOutput("nblast_results_tracing_top10"),
        h3("Score distribution"),
        plotOutput("nblast_results_tracing")
      )
    )
  ),
  
  tabPanel("About",
    mainPanel(
      HTML("This web app accompanies <a href='http://dx.doi.org/10.1101/006346'>Costa et al. (2014) NBLAST: Rapid, sensitive comparison of neuronal structure and construction of neuron family databases</a> and acts as a demonstration of the core NBLAST algorithm (package <a href='https://github.com/jefferislab/nat.nblast'>nat.nblast</a>), along with some features of the <a href='https://github.com/jefferis/nat'>NeuroAnatomy Toolbox</a> and its helper packages: <a href='https://github.com/jefferislab/nat.templatebrains'>nat.templatebrains</a> and <a href='https://github.com/jefferislab/nat.flybrains'>nat.flybrains</a>."),
      h3("Source code"),
      HTML("The full code for this web app can be downloaded from <a href='https://github.com/jefferislab/NBLAST_online'>GitHub</a>."),
      h3("Preparing own data"),
      HTML("Protocols for <a href='http://cshprotocols.cshlp.org/content/2013/4/pdb.prot071720.full'>immunostaining and imaging fly brains</a>, as well as <a href='http://cshprotocols.cshlp.org/content/2013/4/pdb.prot071738.full'>registration of the resulting images</a> are available from Cold Spring Harbor Protocols. We recommend the use of <a href='http://fiji.sc/Simple_Neurite_Tracer'>Simple Neurite Tracer</a> for tracing neurons from the acquired images, detailed instructions for which are available from <a href='http://fiji.sc/Simple_Neurite_Tracer:_Step-By-Step_Instructions'>here</a>.")
    )
  )
  
))
