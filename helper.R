# Load dps object for plotting neurons
dps <- read.neuronlistfh(file.path(getOption('flycircuit.datadir'), 'dpscanon_f9dc90ce5b2ffb74af37db1e3a2cb35b.rds'))

options(nat.default.neuronlist='dps')

# Attach the all-by-all score matrix and load into memory
allbyall <- fc_attach_bigmat("allbyallblastcanon_f9dc90ce5b2ffb74af37db1e3a2cb35b")

# Load VFB ID lookup table
vfb_ids=readRDS('vfb_ids.rds')

# Load VFB annotation ID lookup table
vfb_annotations <- read.table("annotation_map.tsv", header=TRUE, sep="\t", quote = "")

# Load the affinity propagation results
apres16k.p0 <- load_fcdata("apres16k.p0")
apresdf <- as.data.frame(apres16k.p0)
exemplars <- levels(apresdf$exemplar)

# Define a function for a frontal view of the brain
frontalView<-function(zoom=0.6){
	um=structure(c(1, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1), .Dim = c(4L, 4L))
	rgl.viewpoint(userMatrix=um,zoom=zoom)
}

# We will use this downsampled FCWB surface instead of the normal one
FCWB.surf <- read.hxsurf("FCWB.smooth.surf")

# Functions for converting gene names to FlyCircuit.tw URLs and making links
flycircuit_url <- function(neuron_name) {
	neuron_name <- fc_neuron(neuron_name)
	paste0("http://flycircuit.tw/modules.php?name=clearpage&op=detail_table&neuron=", neuron_name)
}

flycircuit_link <- function(neuron_name) {
	url <- flycircuit_url(neuron_name)
	paste0("<a target='_blank' href='", url, "'>View on FlyCircuit.tw</a>")
}

vfb_url <- function(neuron_name, style=c("dev", "old")) {
	style=match.arg(style, c("dev", "old"))
	vfb_id <- as.character(vfb_ids[vfb_ids$Name %in% neuron_name, 'vfbid'])
	if(style=='old'){
		paste0("http://www.virtualflybrain.org/site/tools/view_stack/3rdPartyStack.htm?json=FlyCircuit2012/", neuron_name, "/wlz_meta/tiledImageModelData.jso&type=THIRD_PARTY_STACK&tpbid=", vfb_id)
	} else {
		paste0("http://vfbsandbox3.inf.ed.ac.uk/site/stacks/index.htm?add=", paste0(vfb_id, collapse=','), "&clear=true")
	}
}

vfb_link <- function(neuron_name) {
	url <- vfb_url(neuron_name)
	paste0("<a target='_blank' href='", url, "'>View in Virtual Fly Brain stack browser</a>")
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
tdf <- annotation[annotation$annotation_class%in%c('NeuronType','ALGlomerulus'), ]
tdf$gene_name <- fc_gene_name(tdf$neuron_idid)

type_for_neuron<-function(n) {
	gns=fc_gene_name(n)
	type=tdf[tdf$gene_name == gns, 'text']
	unique(type)
}

link_for_neuron_type <- function(type, style=c("dev", "old")) {
	style <- match.arg(style, c("dev", "old"))
	links <- sapply(type, function(x) {
		ffbt <- vfb_annotations[vfb_annotations$a.text == x, 'class_id']
		if(style == "old") url <- paste0("http://www.virtualflybrain.org/site/tools/anatomy_finder/index.htm?id=", ffbt)
		else url <- paste0("http://vfbsandbox3.inf.ed.ac.uk/site/tools/anatomy_finder/index.htm?id=", ffbt)
		link <- ifelse(length(ffbt) == 0, paste0("<span style='color: black;'>", x, "</span>"), paste0("<a target='_blank' href='", url, "'>", x, "</a>"))
		link
	})
	paste0(links, collapse="<span style='color: black;'>, </span>")
}

links_for_gmr <- function(gmrs, query) {
	gmr_ids <- gmr_vfbid(gmrs)
	query_id <- as.character(vfb_ids[vfb_ids$Name %in% query, 'vfbid'])
	hrefs <- sapply(gmr_ids, function(x) vfb_stack_url(c(x, query_id), clear=TRUE))
	links <- paste0("<a href='", hrefs, "' target='_blank'>", gmrs, "</a>")
	links[is.na(gmr_ids)] <- gmrs[is.na(gmr_ids)]
	links
}

link_for_all_gmrs <- function(gmrs, query) {
	gmr_ids <- gmr_vfbid(gmrs)
	query_id <- as.character(vfb_ids[vfb_ids$Name %in% query, 'vfbid'])
	hrefs <- vfb_stack_url(c(rev(gmr_ids[!is.na(gmr_ids)]), query_id), clear=TRUE)
	links <- paste0("<a href='", hrefs, "' target='_blank'>View all these hits on Virtual Fly Brain.</a>")
	links
}


# Wrapper function for dotprops.character to handle some checks/restrictions that are quite specific to shiny usage
dotprops_from_nrrd<-function(f, ...) {
	ni <- read.im3d(f, ReadData = F)

	imsize=prod(unlist(attr(ni,'datablock')[c("n","size")]))
	if(imsize > 150e6)
		stop("Nrrd image files must be <= 150 Mb uncompressed. Try downsampling to ~ 1 x 1 x 1 µm voxel size.")

	# read the image
	im=read.im3d(f, ReadByteAsRaw = TRUE)
	coords=ind2coord(im)
	if(nrow(coords) > 1e5)
		stop("Nrrd image contains > 100,000 non-zero voxels. Please use a skeletonised/binarised image as produced by http://fiji.sc/Skeletonize3D")

	dotprops(coords, ...)
}
