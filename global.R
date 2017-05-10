# load in the libraries we need to run the application
# shiny
require(shiny)
require(shinythemes)
require(shinydashboard)
# data wrangling
require(tidyr)
require(dplyr)
# mapping
require(leaflet)
require(sp)
require(tigris)
require(acs)
require(tigris)
require(geosphere)
# plotting
require(plotly)
require(shiny)
require(ggplot2)
require(RColorBrewer)
require(GGally)
require(networkD3)
require(igraph)
require(gridExtra)
require(RColorBrewer)
# misc
#library(gtable)
#library(grid)
require(stringr)

# source the files that we need to run the application
source('./source/plotScript.R')
source('./source/mapScript.R')
source('./source/popupPlot.R')
