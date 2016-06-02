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

# URL synching
url_fields_to_sync <- c("all_query")

shinyServer(function(input, output) {

################
# URL synching #
################
firstTime <- TRUE

newHash <- function() {
	newHash <- paste(collapse=",", Map(function(field) { paste(sep="=", field, input[[field]]) }, url_fields_to_sync))
	return(
		if (!firstTime) {
			newHash
		} else {
			if (is.null(input$hash)) {
				NULL
			} else {
				firstTime <<- FALSE;
				isolate(input$hash)
			}
		}
	)
}

output$hash <- renderText(newHash())




###################
# One against all #
###################
all_scores <- reactive({
	query_neuron <- fc_gene_name(gsub("[^A-z,0-9,-]", "", input$all_query))
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

	query_neuron <- gsub("[^A-z,0-9,-]", "", input$all_query)
	if(query_neuron != "" & !fc_gene_name(query_neuron) %in% names(dps)) stop("Invalid neuron name! Valid names include fru-M-200266, Gad1-F-400113, Trh-M-400076, VGlut-F-800287, etc.")
	query_neuron <- fc_gene_name(query_neuron)

	if (!is.na(query_neuron)) {
		plot3d(query_neuron, col='black', lwd=2, soma=TRUE)
		scores <- all_scores()
		plot3d(fc_gene_name(names(scores[2:11])), soma=TRUE)
	}
	rglwidget()
})


output$one_query <- renderText({
	query_neuron <- gsub("[^A-z,0-9,-]", "", input$all_query)
	if(nzchar(query_neuron)) {
		paste0("Query neuron: ", query_neuron)
	} else {
		NULL
	}
})


output$all_download <- downloadHandler(
	filename = function() { paste0(gsub("[^A-z,0-9,-]", "", input$all_query), '_nblast_results_', Sys.Date(), '.csv') },
	content = function(file) {
		query_neuron <- fc_gene_name(gsub("[^A-z,0-9,-]", "", input$all_query))
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
	query_neuron <- fc_gene_name(gsub("[^A-z,0-9,-]", "", input$all_query))
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
	query_neuron <- fc_gene_name(gsub("[^A-z,0-9,-]", "", input$all_query))
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

	query_neuron <- gsub("[^A-z,0-9,-]", "", input$pairwise_query)
	target_neuron <- gsub("[^A-z,0-9,-]", "", input$pairwise_target)
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
	query_neuron <- gsub("[^A-z,0-9,-]", "", input$pairwise_query)
	target_neuron <- gsub("[^A-z,0-9,-]", "", input$pairwise_target)
	if(nzchar(query_neuron) & nzchar(target_neuron)) {
		paste0("Query neuron: ", query_neuron, ", target neuron: ", target_neuron)
	} else {
		NULL
	}
})


output$pairwise_results <- renderText({
	query_neuron <- gsub("[^A-z,0-9,-]", "", input$pairwise_query)
	target_neuron <- gsub("[^A-z,0-9,-]", "", input$pairwise_target)
	if(query_neuron == "" | target_neuron == "") {
		""
	} else {
		paste0("Raw score: ", fc_nblast(fc_gene_name(query_neuron), fc_gene_name(target_neuron), scoremat=allbyall), "    |    Normalised score: ", fc_nblast(fc_gene_name(query_neuron), fc_gene_name(target_neuron), scoremat=allbyall) / fc_nblast(fc_gene_name(query_neuron), fc_gene_name(query_neuron), scoremat=allbyall))
	}
})


output$pairwise_nblast_complete <- reactive({
	query_neuron <- gsub("[^A-z,0-9,-]", "", input$pairwise_query)
	target_neuron <- gsub("[^A-z,0-9,-]", "", input$pairwise_target)
	return(ifelse(query_neuron == "" || target_neuron == "", FALSE, TRUE))
})
outputOptions(output, 'pairwise_nblast_complete', suspendWhenHidden=FALSE)




####################
# Upload a tracing #
####################
tracing <- reactive({
	template_brain <- input$tracing_brain
	if(template_brain == 'Select a template brain') return(NULL)

	isolate({
	query_neuron <- input$tracing_file
	if(is.null(query_neuron)) return(NULL)
	if(grepl("\\.nrrd", query_neuron$name)) {
		# TODO come up with a heuristic to choose the number of neighbours (k)
		# based on the voxel dimensions
		tracing_neuron <- dotprops_from_nrrd(query_neuron$datapath, k=10)
	} else {
		if (grepl("\\.swc", query_neuron$name))
			tracing_neuron <- nat:::read.neuron.swc(query_neuron$datapath)
		else tracing_neuron <- read.neuron(query_neuron$datapath)
		tracing_neuron <- dotprops(tracing_neuron, k=5, resample=1)
	}
	})

	if(template_brain != "FCWB") {
		template_brain <- get(template_brain)
		tracing_neuron <- xform_brain(tracing_neuron, sample=template_brain, reference=FCWB)
	}
	if(input$tracing_mirror)
		tracing_neuron <- mirror_brain(tracing_neuron, FCWB)
	tracing_neuron
})

tracing_nblast_scores <- reactive({
	query_neuron <- tracing()
	if(is.null(query_neuron)) return(NULL)
	scores <- list()
	withProgress(min=1, max=10, message="NBLAST in progress", expr={
		for(i in 1:10) {
			if(!input$tracing_all_neurons) {
				chunk <- split(1:length(exemplars), cut(1:length(exemplars), 10))[[i]]
				if(input$tracing_use_mean) {
					scores[[i]] <- (nblast(dotprops(query_neuron), dps[exemplars[chunk]], normalised=TRUE) + nblast(dps[exemplars[chunk]], dotprops(query_neuron), normalised=TRUE)) / 2
				} else {
					scores[[i]] <- nblast(dotprops(query_neuron), dps[exemplars[chunk]])
				}
			} else {
				chunk <- split(1:length(dps), cut(1:length(dps), 10))[[i]]
				if(input$tracing_use_mean) {
					scores[[i]] <- (nblast(dotprops(query_neuron), dps[chunk], normalised=TRUE) + nblast(dps[chunk], dotprops(query_neuron), normalised=TRUE)) / 2
				} else {
					scores[[i]] <- nblast(dotprops(query_neuron), dps[chunk])
				}
			}
			setProgress(value=i)
		}
	})
	unlist(scores)
})

output$tracing_nblast_results_plot <- renderPlot({
	scores <- tracing_nblast_scores()
	if(is.null(scores)) return(NULL)
	nblast_results <- data.frame(scores=scores)
	p <- ggplot(nblast_results, aes(x=scores)) + geom_histogram(binwidth=diff(range(nblast_results$scores))/100) + xlab("NBLAST score") + ylab("Frequency density") + geom_vline(xintercept=0, colour='red')
	p
})

output$tracing_nblast_results_viewer <- renderText({
	scores <- tracing_nblast_scores()
	if(is.null(scores)) return(NULL)
	top10 <- sort(scores, decreasing=TRUE)[1:10]
	top10n <- fc_neuron(names(top10))
	vfb_link(top10n)
})

output$tracing_nblast_results_top10 <- renderTable({
	query_neuron <- tracing()
	scores <- tracing_nblast_scores()
	if(is.null(scores)) return(NULL)
	names(scores) <- fc_neuron(names(scores))
	data.frame(scores=sort(scores, decreasing=TRUE)[1:10],normalised_scores=sort(scores/nblast(dotprops(query_neuron), dotprops(query_neuron)), decreasing=TRUE)[1:10], flycircuit=sapply(names(sort(scores, decreasing=TRUE)[1:10]), flycircuit_link), vfb=sapply(names(sort(scores, decreasing=TRUE)[1:10]), vfb_link), cluster=sapply(names(sort(scores, decreasing=TRUE)[1:10]), cluster_link), type=sapply(names(sort(scores, decreasing=TRUE)[1:10]), function(x) link_for_neuron_type(type_for_neuron(x))))
}, sanitize.text.function = force)

output$tracing_nblast_results_download <- downloadHandler(
	filename = function() {  paste0(input$tracing_file$name, '_nblast_results_', Sys.Date(), '.csv') },
	content = function(file) {
		scores <- tracing_nblast_scores()
	score_table <- data.frame(neuron=names(scores), raw=scores, norm=scores/nblast(dotprops(tracing()), dotprops(tracing())), type=sapply(names(scores), function(x) paste0(type_for_neuron(x), collapse=", ")))
		colnames(score_table) <- c("Neuron", "Raw NBLAST score", "Normalised NBLAST score", "Type")
		write.csv(score_table, file, row.names=FALSE)
	}
)

output$tracing_nblast_complete <- reactive({
	scores <- tracing_nblast_scores()
	return(ifelse(is.null(scores), FALSE, TRUE))
})
outputOptions(output, 'tracing_nblast_complete', suspendWhenHidden=FALSE)

output$view3d_tracing <- renderRglwidget({
	clear3d()
	query_neuron <- tracing()
	if(!is.null(query_neuron)) {
		plot3d(query_neuron, col='black', lwd=2, soma=TRUE)
		scores <- tracing_nblast_scores()
		scores <- sort(scores, decreasing=TRUE)
		plot3d(dps[names(scores)[1:10]], soma=TRUE)
	}
	plot3d(FCWB)
	frontalView()
	rglwidget()
})




########
# GAL4 #
########
output$gal4_hits <- renderTable({
	query_neuron <- gsub("[^A-z,0-9,-]", "", input$gal4_query)
	if(query_neuron == "") return(NULL)
	query_neuron <- fc_gene_name(input$gal4_query)
	if(is.na(query_neuron))  stop("Invalid neuron name! Valid names include fru-M-200266, Gad1-F-400113, Trh-M-400076, VGlut-F-800287, etc.")

	scores <- vfb_nblast(query_neuron, target="GMR-Gal4", n=input$gal4_n)
	if(is.null(scores)) return(NULL)
	gmr_stack_links <- links_for_gmr(scores$id, input$gal4_query, linktext = thumbnail_images(scores$id))
	names(gmr_stack_links) <- rownames(scores)
	data.frame(n=seq.int(length(gmr_stack_links)),
						 Line=flylight_links(scores$id),
						 Score=scores$score,
						 Stack=gmr_stack_links,
						 Download.Registered=gmr_download_links(scores$id))
}, sanitize.text.function = force, include.rownames=FALSE)

output$gal4_view_all <- renderText({
	query_neuron <- fc_gene_name(input$gal4_query)
	if(is.na(query_neuron)) return(NULL)

	scores <- vfb_nblast(query_neuron, target="GMR-Gal4", n=input$gal4_n)

	link_for_all_gmrs(scores$id, input$gal4_query)
})


#########
# About #
#########




})
