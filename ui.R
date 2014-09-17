library(shiny)
library(shinyRGL)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Compare neuron similarities with NBLAST"),
  
  sidebarPanel(),
  
  mainPanel(
    h2("3D view"),
    webGLOutput("brain3d"),
    h2("NBLAST results")
    )
))
