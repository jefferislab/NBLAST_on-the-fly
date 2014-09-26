library(nat)
library(nat.flybrains)
library(nat.nblast)
library(flycircuit)
library(shiny)
library(shinyRGL)
library(shinysky)

# Load dps object for plotting neurons
dps <- read.neuronlistfh("http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/flycircuit/dpscanon_f9dc90ce5b2ffb74af37db1e3a2cb35b.rds", localdir=getOption('flycircuit.datadir'))

# Attach the all-by-all score matrix and load into memory
allbyall <- fc_attach_bigmat("allbyallblastcanon_f9dc90ce5b2ffb74af37db1e3a2cb35b")
allbyallmem <- allbyall[, ]

# Define a function for a frontal view of the brain
frontalView<-function(zoom=0.6){
  um=structure(c(1, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1), .Dim = c(4L, 4L))
  rgl.viewpoint(userMatrix=um,zoom=zoom)
}

shinyServer(function(input, output) {

output$brain3d <- renderWebGL({
  query_neuron <- input$query
  target_neuron <- input$target
  if(is.null(query_neuron) || is.null(target_neuron)) {
    # Dummy plot
    plot3d(FCWB)
    frontalView()
  } else {
    clear3d()
    plot3d(dps[fc_gene_name(query_neuron)], col='red')
    plot3d(dps[fc_gene_name(target_neuron)], col='blue')
    plot3d(FCWB)
    frontalView()
  }
})

output$nblast_results <- renderText({
  query_neuron <- input$query
  target_neuron <- input$target
  if(is.null(query_neuron) || is.null(target_neuron)) {
    ""
  } else {
    fc_nblast(fc_gene_name(query_neuron), fc_gene_name(target_neuron), scoremat=allbyallmem)
  }
})

})