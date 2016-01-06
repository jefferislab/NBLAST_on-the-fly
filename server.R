library(shiny)
library(rglwidget)
library(nat)

options(rgl.useNULL=TRUE)

shinyServer(function(input, output) {




###################
# One against all #
###################
output$view3d_one_against_all <- renderRglwidget({
	clear3d()
	plot3d(kcs20[1:3])
	rglwidget()
})




############
# Pairwise #
############
output$view3d_pairwise <- renderRglwidget({
	clear3d()
	plot3d(kcs20[4:7])
	rglwidget()
})




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
output$view3d_gal4 <- renderRglwidget({
	clear3d()
	plot3d(kcs20[13:18])
	rglwidget()
})




#########
# About #
#########




})
