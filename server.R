library(nat)
library(nat.flybrains)
library(nat.nblast)
library(flycircuit)
library(shiny)
library(shinyRGL)
library(shinysky)
library(ggplot2)
library(shinyIncubator)

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

# Overwrite RGL's inRows function to reduce the number of digits from 7 to 5
inRows <- function(values, perrow, leadin="\t", digits=5) {
  if (is.matrix(values)) values <- t(values)
  values <- c(values)
  if (is.numeric(values)) values <- formatC(values, digits = digits, width = 1)
  len <- length(values)
  if (len%%perrow != 0) values <- c(values, rep("PADDING", perrow - len%%perrow))
  values <- matrix(values, ncol = perrow, byrow = TRUE)
  lines <- paste(leadin, apply(values, 1, function(row) paste(row, collapse = ", ")))
  lines[length(lines)] <- gsub(", PADDING", "", lines[length(lines)])
  paste(lines, collapse = ",\n")
}
environment(inRows) <- asNamespace('rgl')
assignInNamespace('inRows', inRows, ns='rgl')

# Functions for converting gene names to FlyCircuit.tw URLs and making links
flycircuit_url <- function(gene_name) {
  idid <- fc_idid(gene_name)
  paste0("http://flycircuit.tw/modules.php?name=clearpage&op=detail_table&idid=", idid)
}

flycircuit_link <- function(gene_name) {
  url <- flycircuit_url(gene_name)
  paste0("<a target='_blank' href='", url, "'>View on FlyCircuit.tw</a>")
}

cluster_link <- function(gene_name) {
  cluster <- apresdf[gene_name, 'cluster']
  url <- paste0("http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/clusters/clusters/", cluster, "/")
  paste0("<a target='_blank' href='", url, "'>", cluster, "</a>")
}

link_cluster <- function(cluster) {
  url <- paste0("http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/clusters/clusters/", cluster, "/")
  paste0("<a target='_blank' href='", url, "'>", cluster, "</a>")
}
  

shinyServer(function(input, output, session) {

#######################
# Pairwise comparison #
#######################
output$brain3d_one <- renderWebGL({
  query_neuron <- input$query_one
  target_neuron <- input$target_one
  if(nzchar(query_neuron) && nzchar(target_neuron)) {
    clear3d()
    plot3d(dps[fc_gene_name(query_neuron)], col='red')
    plot3d(dps[fc_gene_name(target_neuron)], col='blue')
  }
  plot3d(FCWB)
  frontalView()
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
  query_neuron <- query_neuron()
  if(nzchar(query_neuron)) {
    clear3d()
    plot3d(dps[fc_gene_name(query_neuron)], col='black', lwd=2)
    scores <- sort(nblast_scores(), decreasing=TRUE)
    plot3d(dps[names(scores[2:11])], col=rainbow(10))
  }
  plot3d(FCWB)
  frontalView()
})

query_neuron <- reactive({
  query_neuron <- input$query_all
  if(query_neuron == "") return("")
  query_neuron
})

nblast_scores <- reactive({
  query_neuron <- query_neuron()
  if(query_neuron == "") return(NULL)
  scores <- list()
  withProgress(session, min=1, max=10, expr={
    setProgress(message="NBLAST in progress")
    for(i in 1:10) {
      chunk <- split(1:length(dps), cut(1:length(dps), 10))[[i]]
      scores[[i]] <<- fc_nblast(fc_gene_name(query_neuron), names(dps)[chunk], scoremat=allbyall)
      setProgress(value=i)
    }
  })
  scores <- unlist(scores)
  scores
})

output$nblast_results_all <- renderPlot({
  scores <- nblast_scores()
  if(is.null(scores)) return(NULL)
  nblast_results <- data.frame(scores=scores)
  p <- ggplot(nblast_results, aes(x=scores)) + geom_histogram(binwidth=diff(range(nblast_results$scores))/100) + xlab("NBLAST score") + ylab("Frequency density") + geom_vline(xintercept=0, colour='red')
  p
})

output$nblast_results_all_top10 <- renderTable({
  scores <- nblast_scores()
  if(is.null(scores)) return(NULL)
  data.frame(scores=sort(scores, decreasing=TRUE)[2:11], normalised_scores=sort(scores/fc_nblast(fc_gene_name(query_neuron()), fc_gene_name(query_neuron()), scoremat=allbyall), decreasing=TRUE)[2:11], flycircuit=sapply(names(sort(scores, decreasing=TRUE)[2:11]), flycircuit_link), cluster=sapply(names(sort(scores, decreasing=TRUE)[2:11]), cluster_link))
}, sanitize.text.function = force)

output$nblast_results_all_top10_clusters <- renderTable({
  scores <- nblast_scores()
  if(is.null(scores)) return(NULL)
  scores <- sort(scores, decreasing=TRUE)
  clusters <- apresdf[names(scores), 'cluster']
  unique_clusters <- unique(clusters[-1])[1:10]
  scores <- scores[names(scores) != fc_gene_name(query_neuron())]
  names(unique_clusters) <- sapply(unique_clusters, function(x) names(scores[which(x == clusters)[1]]))
  clusters <- unique_clusters
    
  data.frame(cluster=sapply(clusters[1:10], link_cluster), scores=scores[names(clusters)[1:10]], normalised_scores=scores[names(clusters)[1:10]]/fc_nblast(fc_gene_name(query_neuron()), fc_gene_name(query_neuron()), scoremat=allbyall))
}, sanitize.text.function = force, include.rownames=FALSE)



################
# User tracing #
################
output$brain3d_tracing <- renderWebGL({
  query_neuron <- tracing()
  if(!is.null(query_neuron)) {
    plot3d(query_neuron, col='red')
  }
  plot3d(FCWB)
  frontalView()
})

tracing <- reactive({
  template_brain <- input$brain
  if(template_brain == 'Select a template brain') return(NULL)
  
  isolate({
  query_neuron <- input$tracing_file
  if(is.null(query_neuron)) return(NULL)
  if(grepl("\\.swc", query_neuron$name)) tracing_neuron <- nat:::read.neuron.swc(query_neuron$datapath)
  else tracing_neuron <- read.neuron(query_neuron$datapath)
  })

  message(template_brain)
  if(template_brain != "FCWB") {
    template_brain <- get(template_brain)
    tracing_neuron <- xform_brain(tracing_neuron, sample=template_brain, reference=FCWB)
  }
  tracing_neuron
})

nblast_scores_tracing <- reactive({
  query_neuron <- tracing()
  if(is.null(query_neuron)) return(NULL)
  scores <- list()
  withProgress(session, min=1, max=10, expr={
    setProgress(message="NBLAST in progress", detail="This may take a few minutes")
    for(i in 1:10) {
      chunk <- split(1:length(exemplars), cut(1:length(exemplars), 10))[[i]]
      scores[[i]] <<- nblast(dotprops(query_neuron), dps[exemplars[chunk]])
      setProgress(value=i)
    }
  })
  unlist(scores)
})

output$nblast_results_tracing <- renderPlot({
  scores <- nblast_scores_tracing()
  if(is.null(scores)) return(NULL)
  nblast_results <- data.frame(scores=scores)
  p <- ggplot(nblast_results, aes(x=scores)) + geom_histogram(binwidth=diff(range(nblast_results$scores))/100) + xlab("NBLAST score") + ylab("Frequency density") + geom_vline(xintercept=0, colour='red')
  p
})

output$nblast_results_tracing_top10 <- renderTable({
  query_neuron <- tracing()
  scores <- nblast_scores_tracing()
  if(is.null(scores)) return(NULL)
  data.frame(scores=sort(scores, decreasing=TRUE)[1:10], normalised_scores=sort(scores/nblast(dotprops(query_neuron), dotprops(query_neuron)), decreasing=TRUE)[1:10], flycircuit=sapply(names(sort(scores, decreasing=TRUE)[1:10]), flycircuit_link), cluster=sapply(names(sort(scores, decreasing=TRUE)[1:10]), cluster_link))
}, sanitize.text.function = force)

})
