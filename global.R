# load in the libraries we need to run the application
# shiny
library(shiny)
library(shinythemes)
library(shinydashboard)
# data wrangling
library(tidyr)
library(dplyr)
# mapping
library(leaflet)
library(sp)
library(tigris)
library(acs)
library(tigris)
library(geosphere)
# plotting
library(plotly)
library(shiny)
library(ggplot2)
library(RColorBrewer)
library(GGally)
library(networkD3)
library(igraph)
library(gridExtra)
library(RColorBrewer)
# misc
#library(gtable)
#library(grid)
library(stringr)

# source the files that we need to run the application
source('./source/plotScript.R')
source('./source/mapScript.R')
source('./source/popupPlot.R')
