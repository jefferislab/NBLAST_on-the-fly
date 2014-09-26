library(shiny)
library(shinyRGL)

dps <- read.neuronlistfh("http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/flycircuit/dpscanon_f9dc90ce5b2ffb74af37db1e3a2cb35b.rds", localdir=getOption('flycircuit.datadir'))

neuron_names <- fc_neuron(names(dps))
neuron_ids <- fc_idid(names(dps))

shinyUI(navbarPage("NBLAST on-the-fly",
  #######################
  # Pairwise comparison #
  #######################
  tabPanel("Pairwise comparison",
    sidebarLayout(  
      sidebarPanel(
        h3("Query:"),
        textInput.typeahead(
          id="query_one",
          placeholder="Type a FlyCircuit neuron name",
          local=data.frame(name=neuron_names, id=neuron_ids),
          valueKey = "name",
          tokens=1:length(neuron_names),
          template = HTML("<p class='repo-language'>{{id}}</p> <p class='repo-name'>{{name}}</p>")
        ),
        
        h3("Target:"),
        textInput.typeahead(
          id="target_one",
          placeholder="Type a FlyCircuit neuron name",
          local=data.frame(name=neuron_names, id=neuron_ids),
          valueKey = "name",
          tokens=1:length(neuron_names),
          template = HTML("<p class='repo-language'>{{id}}</p> <p class='repo-name'>{{name}}</p>")
        ),
        br(),
        br(),
        submitButton("NBLAST")
      ),
      
      mainPanel(
        h2("3D view"),
        webGLOutput("brain3d_one"),
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
        h3("Query:"),
        textInput.typeahead(
          id="query_all",
          placeholder="Type a FlyCircuit neuron name",
          local=data.frame(name=neuron_names, id=neuron_ids),
          valueKey = "name",
          tokens=1:length(neuron_names),
          template = HTML("<p class='repo-language'>{{id}}</p> <p class='repo-name'>{{name}}</p>")
        ),
        br(),
        br(),
        submitButton("NBLAST")
      ),
      
      mainPanel(
        h2("3D view"),
        webGLOutput("brain3d_all"),
        h2("NBLAST results"),
        h3("Top 10 hits"),
        tableOutput("nblast_results_all_top10"),
        h3("Score distribution"),
        plotOutput("nblast_results_all")
      )
    )
  )
  
))
