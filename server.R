library(nat)
library(nat.flybrains)
library(nat.nblast)
library(flycircuit)
library(shiny)
library(shinyRGL)

# Define a function for a frontal view of the brain
frontalView<-function(zoom=0.6){
  um=structure(c(1, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1), .Dim = c(4L, 4L))
  rgl.viewpoint(userMatrix=um,zoom=zoom)
}

shinyServer(function(input, output) {

output$brain3d <- renderWebGL({
  plot3d(FCWB)
  frontalView()
})

})