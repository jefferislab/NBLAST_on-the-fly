library(nat)
library(nat.flybrains)
library(nat.nblast)
library(flycircuit)
library(shiny)
library(shinyRGL)
library(shinysky)
library(ggplot2)

# Load dps object for plotting neurons
dps <- read.neuronlistfh(file.path(getOption('flycircuit.datadir'), 'dpscanon_f9dc90ce5b2ffb74af37db1e3a2cb35b.rds'))

# Attach the all-by-all score matrix and load into memory
allbyall <- fc_attach_bigmat("allbyallblastcanon_f9dc90ce5b2ffb74af37db1e3a2cb35b")

# Load the affinity propagation results
apres16k.p0 <- load_fcdata("apres16k.p0")
apresdf <- as.data.frame(apres16k.p0)
exemplars <- levels(apresdf$exemplar)

# Define a function for a frontal view of the brain
frontalView<-function(zoom=0.6){
  um=structure(c(1, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1), .Dim = c(4L, 4L))
  rgl.viewpoint(userMatrix=um,zoom=zoom)
}

shinyServer(function(input, output) {

#######################
# Pairwise comparison #
#######################
output$brain3d_one <- renderWebGL({
  query_neuron <- input$query_one
  target_neuron <- input$target_one
  if(query_neuron == "" || target_neuron == "") {
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

output$nblast_results_one <- renderText({
  query_neuron <- input$query_one
  target_neuron <- input$target_one
  if(query_neuron == "" || target_neuron == "") {
    ""
  } else {
    paste0("Raw score: ", fc_nblast(fc_gene_name(query_neuron), fc_gene_name(target_neuron), scoremat=allbyall), "    |    Normalised score: ", fc_nblast(fc_gene_name(query_neuron), fc_gene_name(target_neuron), scoremat=allbyall) / fc_nblast(fc_gene_name(query_neuron), fc_gene_name(query_neuron), scoremat=allbyall))
  }
})



###################
# One against all #
###################
output$brain3d_all <- renderWebGL({
  query_neuron <- input$query_all
  if(query_neuron == "") {
    # Dummy plot
    plot3d(FCWB)
    frontalView()
  } else {
    clear3d()
    plot3d(dps[fc_gene_name(query_neuron)], col='red')
    plot3d(FCWB)
    frontalView()
  }
})

output$nblast_results_all <- renderPlot({
  query_neuron <- input$query_all
  if(is.null(query_neuron)) {
    NULL
  } else {
    scores <- fc_nblast(fc_gene_name(query_neuron), scoremat=allbyall)
    output$nblast_results_all_top10 <- renderTable({ data.frame(scores=sort(scores, decreasing=TRUE)[2:11], normalised_scores=sort(scores/fc_nblast(fc_gene_name(query_neuron), fc_gene_name(query_neuron), scoremat=allbyall), decreasing=TRUE)[2:11]) })
    nblast_results <- data.frame(scores=scores)
    p <- ggplot(nblast_results, aes(x=scores)) + geom_histogram(binwidth=diff(range(nblast_results$scores))/100) + xlab("NBLAST score") + ylab("Frequency density") + geom_vline(xintercept=0, colour='red')
    p
  }
})



################
# User tracing #
################
output$brain3d_tracing <- renderWebGL({
  query_neuron <- input$tracing_file
  if(is.null(query_neuron)) {
    # Dummy plot
    plot3d(FCWB)
    frontalView()
  } else {
    if(grepl("\\.swc", query_neuron$name)) tracing_neuron <- nat:::read.neuron.swc(query_neuron$datapath)
    else tracing_neuron <- read.neuron(query_neuron$datapath)
    clear3d()
    plot3d(tracing_neuron, col='red')
    plot3d(FCWB)
    frontalView()
  }
})

output$nblast_results_tracing <- renderPlot({
  query_neuron <- input$tracing_file
  if(is.null(query_neuron)) {
    NULL
  } else {
    if(grepl("\\.swc", query_neuron$name)) tracing_neuron <- nat:::read.neuron.swc(query_neuron$datapath)
    else tracing_neuron <- read.neuron(query_neuron$datapath)
    message(system.time(scores <- nblast(dotprops(tracing_neuron), dps[exemplars])))
    output$nblast_results_tracing_top10 <- renderTable({ data.frame(scores=sort(scores, decreasing=TRUE)[1:10], normalised_scores=sort(scores/nblast(dotprops(tracing_neuron), dotprops(tracing_neuron)), decreasing=TRUE)[1:10]) })
    nblast_results <- data.frame(scores=scores)
    p <- ggplot(nblast_results, aes(x=scores)) + geom_histogram(binwidth=diff(range(nblast_results$scores))/100) + xlab("NBLAST score") + ylab("Frequency density") + geom_vline(xintercept=0, colour='red')
    p
  }
})

})
