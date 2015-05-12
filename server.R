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

# Load VFB ID lookup table
vfb_ids <- read.table("http://www.virtualflybrain.org/public_resources/fc_name_mapping.csv", sep=",", header=TRUE)

# Load the affinity propagation results
apres16k.p0 <- load_fcdata("apres16k.p0")
apresdf <- as.data.frame(apres16k.p0)
exemplars <- levels(apresdf$exemplar)

# Define a function for a frontal view of the brain
frontalView<-function(zoom=0.6){
  um=structure(c(1, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1), .Dim = c(4L, 4L))
  rgl.viewpoint(userMatrix=um,zoom=zoom)
}

# Overwrite the normal FCWB surface with our downsampled one
FCWB.surf <- read.hxsurf("FCWB.smooth.surf")
# We need to overwrite the function from nat.templatebrains as otherwise this will pick the surface from the package
plot3d.templatebrain <- function(x, col='grey', alpha=0.3, ...) {
  plot3d(get(paste0(deparse(substitute(x)), ".surf")), col=col, alpha=alpha, ...)
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
flycircuit_url <- function(neuron_name) {
  idid <- fc_idid(neuron_name)
  paste0("http://flycircuit.tw/modules.php?name=clearpage&op=detail_table&idid=", idid)
}

flycircuit_link <- function(neuron_name) {
  url <- flycircuit_url(fc_gene_name(neuron_name))
  paste0("<a target='_blank' href='", url, "'>View on FlyCircuit.tw</a>")
}

vfb_url <- function(neuron_name) {
  vfb_id <- vfb_ids[vfb_ids$Name == neuron_name, 'vfbid']
  paste0("http://www.virtualflybrain.org/site/tools/view_stack/3rdPartyStack.htm?json=FlyCircuit2012/", neuron_name, "/wlz_meta/tiledImageModelData.jso&type=THIRD_PARTY_STACK&tpbid=", vfb_id)
}

vfb_link <- function(neuron_name) {
  url <- vfb_url(neuron_name)
  paste0("<a target='_blank' href='", url, "'>View in VFB stack browser</a>")
}

cluster_link <- function(neuron_name) {
  cluster <- apresdf[fc_gene_name(neuron_name), 'cluster']
  url <- paste0("http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/clusters/clusters/", cluster, "/")
  paste0("<a target='_blank' href='", url, "'>", cluster, "</a>")
}

link_cluster <- function(cluster) {
  url <- paste0("http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/clusters/clusters/", cluster, "/")
  paste0("<a target='_blank' href='", url, "'>", cluster, "</a>")
}

## Annotation data
library(flycircuit)

type_for_neuron<-function(n) {
  gns=fc_gene_name(n)
  subset(annotation)
  tdf=subset(annotation, annotation_class=='NeuronType')
  tdf$gene_name=fc_gene_name(tdf$neuron_idid)
  
  
  ntypes=rep("",length(dps))
  names(ntypes)=names(dps)
  ntypes[tdf$gene_name]=tdf$text
  ntypes[gns]
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
    plot3d(dps[fc_gene_name(names(scores[2:11]))], col=rainbow(10))
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
  withProgress(min=1, max=10, message="NBLAST in progress", expr={
    for(i in 1:10) {
      chunk <- split(1:length(dps), cut(1:length(dps), 10))[[i]]
      if(!input$use_mean) {
        scores[[i]] <- fc_nblast(fc_gene_name(query_neuron), names(dps)[chunk], scoremat=allbyall)
      } else {
        scores[[i]] <- fc_nblast(fc_gene_name(query_neuron), names(dps)[chunk], scoremat=allbyall, normalisation='mean')
      }
      setProgress(value=i)
    }
  })
  scores <- unlist(scores)
  names(scores) <- fc_neuron(names(scores))
  scores
})

output$nblast_results_all <- renderPlot({
  scores <- nblast_scores()
  if(is.null(scores)) return(NULL)
  nblast_results <- data.frame(scores=scores)
  p <- ggplot(nblast_results, aes(x=scores)) + geom_histogram(binwidth=diff(range(nblast_results$scores))/100) + xlab("NBLAST score") + ylab("Frequency density") + geom_vline(xintercept=0, colour='red')
  p
})

output$nblast_results_all_download <- downloadHandler(
  filename = function() {  paste0(input$query_all, '_nblast_results_', Sys.Date(), '.csv') },
  content = function(file) {
    scores <- nblast_scores()
    score_table <- data.frame(neuron=names(scores), raw=scores, norm=scores/fc_nblast(fc_gene_name(query_neuron()), fc_gene_name(query_neuron()), scoremat=allbyall))
    score_table$type=type_for_neuron(score_table$neuron)
    colnames(score_table) <- c("Neuron", "Raw NBLAST score", "Normalised NBLAST score", "Neuron Type")
    write.csv(score_table, file, row.names=FALSE)
  }
)

output$nblast_all_complete <- reactive({
  scores <- nblast_scores()
  return(ifelse(is.null(scores), FALSE, TRUE))
})
outputOptions(output, 'nblast_all_complete', suspendWhenHidden=FALSE)

output$nblast_results_all_top10 <- renderTable({
  scores <- nblast_scores()
  if(is.null(scores)) return(NULL)
  if(!input$use_mean) {
    data.frame(scores=sort(scores, decreasing=TRUE)[2:11], 
               normalised_scores=sort(scores/fc_nblast(fc_gene_name(query_neuron()), fc_gene_name(query_neuron()), scoremat=allbyall), decreasing=TRUE)[2:11],
               flycircuit=sapply(names(sort(scores, decreasing=TRUE)[2:11]), flycircuit_link), 
               vfb=sapply(names(sort(scores, decreasing=TRUE)[2:11]), vfb_link), 
               cluster=sapply(names(sort(scores, decreasing=TRUE)[2:11]), cluster_link),
               type=type_for_neuron(names(sort(scores, decreasing=TRUE)[2:11])))
  } else {
    data.frame(scores=sort(scores, decreasing=TRUE)[2:11], flycircuit=sapply(names(sort(scores, decreasing=TRUE)[2:11]), flycircuit_link), vfb=sapply(names(sort(scores, decreasing=TRUE)[2:11]), vfb_link), cluster=sapply(names(sort(scores, decreasing=TRUE)[2:11]), cluster_link))
  }
}, sanitize.text.function = force)

output$nblast_results_all_top10_clusters <- renderTable({
  scores <- nblast_scores()
  if(is.null(scores)) return(NULL)
  scores <- sort(scores, decreasing=TRUE)
  clusters <- apresdf[fc_gene_name(names(scores)), 'cluster']
  unique_clusters <- unique(clusters[-1])[1:10]
  scores <- scores[names(scores) != query_neuron()]
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
    plot3d(query_neuron, col='black', lwd=2)
    scores <- nblast_scores_tracing()
    scores <- sort(scores, decreasing=TRUE)
    plot3d(dps[names(scores)[1:10]])
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
  
  tracing_neuron <- dotprops(tracing_neuron, resample=1)

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
withProgress(min=1, max=10, message="NBLAST in progress", detail="This may take a few minutes", expr={
    for(i in 1:10) {
      if(!input$all_neurons) {
        chunk <- split(1:length(exemplars), cut(1:length(exemplars), 10))[[i]]
        scores[[i]] <- nblast(dotprops(query_neuron), dps[exemplars[chunk]])
      } else {
        chunk <- split(1:length(dps), cut(1:length(dps), 10))[[i]]
        scores[[i]] <- nblast(dotprops(query_neuron), dps[chunk])
      }
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
  names(scores) <- fc_neuron(names(scores))
  data.frame(scores=sort(scores, decreasing=TRUE)[1:10], normalised_scores=sort(scores/nblast(dotprops(query_neuron), dotprops(query_neuron)), decreasing=TRUE)[1:10], flycircuit=sapply(names(sort(scores, decreasing=TRUE)[1:10]), flycircuit_link), vfb=sapply(names(sort(scores, decreasing=TRUE)[1:10]), vfb_link), cluster=sapply(names(sort(scores, decreasing=TRUE)[1:10]), cluster_link))
}, sanitize.text.function = force)

output$nblast_results_tracing_download <- downloadHandler(
  filename = function() {  paste0(input$tracing_file$name, '_nblast_results_', Sys.Date(), '.csv') },
  content = function(file) {
    scores <- nblast_scores_tracing()
    score_table <- data.frame(neuron=names(scores), raw=scores, norm=scores/nblast(dotprops(tracing()), dotprops(tracing())))
    colnames(score_table) <- c("Neuron", "Raw NBLAST score", "Normalised NBLAST score")
    write.csv(score_table, file, row.names=FALSE)
  }
)

output$nblast_tracing_complete <- reactive({
  scores <- nblast_scores_tracing()
  return(ifelse(is.null(scores), FALSE, TRUE))
})
outputOptions(output, 'nblast_tracing_complete', suspendWhenHidden=FALSE)

})
