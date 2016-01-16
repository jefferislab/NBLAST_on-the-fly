library(shiny)
library(rglwidget)
library(nat)
library(nat.nblast)
library(nat.flybrains)
library(flycircuit)
library(ggplot2)
library(downloader)
library(vfbr)

options(rgl.useNULL=TRUE)

source("helper.R")

shinyServer(function(input, output) {




###################
# One against all #
###################
all_scores <- reactive({
	query_neuron <- fc_gene_name(input$all_query)
	if (is.na(query_neuron)) return(NULL)
	scores <- list()

	withProgress({
		for (i in 1:10) {
			chunk <- split(1:length(dps), cut(1:length(dps), 10))[[i]]
			if(!input$all_use_mean) {
				scores[[i]] <- fc_nblast(query_neuron, names(dps)[chunk], scoremat=allbyall)
				} else {
					scores[[i]] <- fc_nblast(query_neuron, names(dps)[chunk], scoremat=allbyall, normalisation='mean')
				}
			setProgress(value=i)
		}
	}, message="NBLASTing...")

	scores <- unlist(scores)
	names(scores) <- fc_neuron(names(scores))
	scores <- sort(scores, decreasing=TRUE)
	scores
})


output$all_nblast_complete <- reactive({
	scores <- all_scores()
	return(ifelse(is.null(scores), FALSE, TRUE))
})
outputOptions(output, 'all_nblast_complete', suspendWhenHidden=FALSE)


output$view3d_one_against_all <- renderRglwidget({
	clear3d()
	plot3d(FCWB)
	frontalView()

	query_neuron <- input$all_query
	if(query_neuron != "" & !fc_gene_name(query_neuron) %in% names(dps)) stop("Invalid neuron name! Valid names include fru-M-200266, Gad1-F-400113, Trh-M-400076, VGlut-F-800287, etc.")
	query_neuron <- fc_gene_name(query_neuron)

	if (!is.na(query_neuron)) {
		plot3d(query_neuron, col='black', lwd=2, soma=TRUE)
		scores <- all_scores()
		plot3d(fc_gene_name(names(scores[2:11])), soma=TRUE)
	}
	rglwidget()
})


output$all_download <- downloadHandler(
	filename = function() { paste0(input$all_query, '_nblast_results_', Sys.Date(), '.csv') },
	content = function(file) {
		query_neuron <- fc_gene_name(input$all_query)
		scores <- all_scores()
		score_table <- data.frame(neuron=names(scores), raw=scores, norm=scores/fc_nblast(query_neuron, query_neuron, scoremat=allbyall))
		score_table$type=sapply(score_table$neuron, function(x) paste0(type_for_neuron(x), collapse=", "))
		colnames(score_table) <- c("Neuron", "Raw NBLAST score", "Normalised NBLAST score", "Neuron Type")
		write.csv(score_table, file, row.names=FALSE)
	}
)


output$all_vfb_viewer <- renderText({
	scores <- all_scores()
	if(is.null(scores)) return(NULL)
	top10n <- names(scores[2:11])
	vfb_link(top10n)
})


output$all_top10_hits <- renderTable({
	query_neuron <- fc_gene_name(input$all_query)
	scores <- all_scores()
	if(is.null(scores)) return(NULL)
	top10 <- scores[2:11]
	top10n <- names(top10)
	self_score <- fc_nblast(query_neuron, query_neuron, scoremat=allbyall)[1]

	sdf <- data.frame(scores=top10, normalised_scores=top10/self_score, flycircuit=sapply(top10n, flycircuit_link), vfb=sapply(top10n, vfb_link), cluster=sapply(top10n, cluster_link), type=sapply(top10n, function(x) link_for_neuron_type(type_for_neuron(x))))

	if(!input$all_use_mean) {
		sdf
	} else {
		sdf$normalised_scores=NULL
		sdf
	}
}, sanitize.text.function = force)


output$all_top10_clusters <- renderTable({
	query_neuron <- fc_gene_name(input$all_query)
	scores <- all_scores()
	if(is.null(scores)) return(NULL)
	clusters <- apresdf[fc_gene_name(names(scores)), 'cluster']
	unique_clusters <- unique(clusters[-1])[1:10]
	scores <- scores[names(scores) != query_neuron]
	names(unique_clusters) <- sapply(unique_clusters, function(x) names(scores[which(x == clusters)[1]]))
	clusters <- unique_clusters
	self_score <- fc_nblast(query_neuron, query_neuron, scoremat=allbyall)[1]
	data.frame(cluster=sapply(clusters[1:10], link_cluster), scores=scores[names(clusters)[1:10]], normalised_scores=scores[names(clusters)[1:10]]/self_score)
}, sanitize.text.function = force, include.rownames=FALSE)


output$all_distribution <- renderPlot({
	scores <- all_scores()
	if(is.null(scores)) return(NULL)
	nblast_results <- data.frame(scores=scores)
	p <- ggplot(nblast_results, aes(x=scores)) + geom_histogram(binwidth=diff(range(nblast_results$scores))/100) + xlab("NBLAST score") + ylab("Frequency density") + geom_vline(xintercept=0, colour='red')
	p
})




############
# Pairwise #
############
output$view3d_pairwise <- renderRglwidget({
	clear3d()
	plot3d(FCWB)
	frontalView()

	query_neuron <- input$pairwise_query
	target_neuron <- input$pairwise_target
	if ((query_neuron != "" & !fc_gene_name(query_neuron) %in% names(dps)) | (target_neuron != "" & !fc_gene_name(target_neuron) %in% names(dps))) stop("Invalid neuron name! Valid names include fru-M-200266, Gad1-F-400113, Trh-M-400076, VGlut-F-800287, etc.")

	if(nzchar(query_neuron) & nzchar(target_neuron)) {
		query_neuron <- fc_gene_name(query_neuron)
		target_neuron <- fc_gene_name(target_neuron)
		plot3d(query_neuron, col='red', soma=TRUE)
		plot3d(target_neuron, col='blue', soma=TRUE)
	}

	rglwidget()
})


output$pairwise_query_target <- renderText({
	query_neuron <- input$pairwise_query
	target_neuron <- input$pairwise_target
	if(nzchar(query_neuron) & nzchar(target_neuron)) {
		paste0("Query neuron: ", query_neuron, ", target neuron: ", target_neuron)
	} else {
		NULL
	}
})


output$pairwise_results <- renderText({
	query_neuron <- input$pairwise_query
	target_neuron <- input$pairwise_target
	if(query_neuron == "" | target_neuron == "") {
		""
	} else {
		paste0("Raw score: ", fc_nblast(fc_gene_name(query_neuron), fc_gene_name(target_neuron), scoremat=allbyall), "    |    Normalised score: ", fc_nblast(fc_gene_name(query_neuron), fc_gene_name(target_neuron), scoremat=allbyall) / fc_nblast(fc_gene_name(query_neuron), fc_gene_name(query_neuron), scoremat=allbyall))
	}
})


output$pairwise_nblast_complete <- reactive({
	query_neuron <- input$pairwise_query
	target_neuron <- input$pairwise_target
	return(ifelse(query_neuron == "" || target_neuron == "", FALSE, TRUE))
})
outputOptions(output, 'pairwise_nblast_complete', suspendWhenHidden=FALSE)




####################
# Upload a tracing #
####################
output$view3d_tracing <- renderRglwidget({
	clear3d()
	plot3d(kcs20[8:12])
	rglwidget()
})




########
# GAL4 #
########
output$gal4_hits <- renderTable({
	query_neuron <- input$gal4_query
	if(!fc_gene_name(query_neuron) %in% names(dps)) stop("Invalid neuron name! Valid names include fru-M-200266, Gad1-F-400113, Trh-M-400076, VGlut-F-800287, etc.")
	query_neuron <- fc_gene_name(input$gal4_query)
	if(is.na(query_neuron)) return(NULL)

	scores <- vfb_nblast(query_neuron, target="GMR-Gal4", n=input$gal4_n)
	if(is.null(scores)) return(NULL)
	gmr_stack_links <- links_for_gmr(scores$id, input$gal4_query)
	names(gmr_stack_links) <- rownames(scores)
	data.frame(line=gmr_stack_links, score=scores$score)
}, sanitize.text.function = force, include.rownames=FALSE)




#########
# About #
#########




})
