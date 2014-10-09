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
        "Select two FlyCircuit neurons to compare with NBLAST. The query neuron will be plotted in red in the 3D viewer to the right, while the target neuron will be drawn in blue.",
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
        "Select a FlyCircuit neuron to compare against all FlyCircuit neurons, with NBLAST. The query neuron will be plotted in black in the 3D viewer to the right, alongside the top 10 hits (rainbow coloured from red = best to violet = worst).",
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
        submitButton("NBLAST")
      ),
      
      mainPanel(
        progressInit(),
        h2("3D view"),
        webGLOutput("brain3d_all", width="800px", height="600px"),
        h2("NBLAST results"),
        h3("Top 10 hits"),
        tableOutput("nblast_results_all_top10"),
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
        "Upload a tracing to compare against all FlyCircuit neurons, using NBLAST. The query neuron will be plotted in black in the 3D viewer to the right, alongside the top 10 hits.",
        h3("Query"),
        fileInput('tracing_file', "Your tracing:")
      ),
      
      mainPanel(
        progressInit(),
        h2("3D view"),
        webGLOutput("brain3d_tracing", width="800px", height="600px"),
        h2("NBLAST results"),
        h3("Top 10 hits"),
        tableOutput("nblast_results_tracing_top10"),
        h3("Score distribution"),
        plotOutput("nblast_results_tracing")
      )
    )
  ),
  
  tabPanel("About",
    mainPanel(
      "This web app accompanies ", a("Costa et al. (2014) NBLAST: Rapid, sensitive comparison of neuronal structure and construction of neuron family databases", href="http://dx.doi.org/10.1101/006346"), " and acts as a demonstration of the core NBLAST algorithm, along with some features of the ", a("NeuroAnatomy Toolbox", href="https://github.com/jefferis/nat"), " and its helper packages: ", a("nat.nblast,", href="https://github.com/jefferislab/nat.nblast")," ", a("nat.templatebrains", href="https://github.com/jefferislab/nat.templatebrains"), " and ", a("nat.flybrains.", href="https://github.com/jefferislab/nat.flybrains"),
      h2("Source code"),
      "The full code for this web app can be downloaded from ", a("GitHub.",href="https://github.com/jefferislab/NBLAST_online")
    )
  )
  
))
