library(shiny)
library(shinyRGL)

dps <- read.neuronlistfh("http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/flycircuit/dpscanon_f9dc90ce5b2ffb74af37db1e3a2cb35b.rds", localdir=getOption('flycircuit.datadir'))

neuron_names <- fc_neuron(names(dps))
neuron_ids <- fc_idid(names(dps))

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Compare neuron similarities with NBLAST"),
  
  sidebarPanel(
    h3("Query:"),
    textInput.typeahead(
      id="query",
      placeholder="Type a FlyCircuit neuron name",
      local=data.frame(name=neuron_names, id=neuron_ids),
      valueKey = "name",
      tokens=1:length(neuron_names),
      template = HTML("<p class='repo-language'>{{id}}</p> <p class='repo-name'>{{name}}</p>")
    ),
    
    h3("Target:"),
    textInput.typeahead(
      id="target",
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
    webGLOutput("brain3d"),
    h2("NBLAST results"),
    textOutput("nblast_results")
  )
))
