

############
# SHINY UI #
############

ui <- tagList(
  
  navbarPage(theme = shinytheme("sandstone"),
             "LA County Voters",
             tabPanel("Census Data",
                      conditionalPanel("input.conditionedPanels==1",
                                       sidebarPanel(width = 4,
                                                    selectInput("x_axis", "Select X Axis", 
                                                                colnames(census), selected = "language"),
                                                    selectInput("y_axis", "Select Y Axis", 
                                                                colnames(census), selected = "Over.18"),
                                                    selectInput("color", "Select color variable", 
                                                                factor_names, selected = "gender"),
                                                    selectInput("size", "Select size variable", 
                                                                factor_names, selected = "language"),
                                       strong("Hover over plot for additional information"))
                      ),
                      
                      conditionalPanel("input.conditionedPanels==2",
                                       sidebarPanel(width = 4,
                                                    selectizeInput("scatter_cols", 
                                                                   "Select variables to plot", numeric_names,
                                                                   multiple = T, 
                                                                   selected = c("Employed", "Unemployed")),
                                                    selectInput("scatter_color", "Select color variable", factor_names,
                                                                selected = "gender"))
                      ),
                      
                      conditionalPanel("input.conditionedPanels==3",
                                       sidebarPanel(width = 4,
                                                    selectizeInput("parallel_cols", "Select parallel variables to plot",
                                                                   colnames(e), multiple = T,
                                                                   selected = c("Education..High.School.Diploma", 
                                                                                "Education..Higher.Education.Degree",
                                                                                "Education..No.Diploma.or.Degree")),
                                                    strong("We are interested in understanding how demographic information 
                                                            corresponds to voting behaviour. 
                                                            As a first step, we non-parametrically split voters into 
                                                            what we determined to be 12 distinct groups using census 
                                                            block information
                                                            and K-Means Clusterings. The clusters are repesented by the 
                                                            different colored
                                                            lines in the parallel coordinat plot")
                                              
                                       )
                      ), 
                      conditionalPanel("input.conditionedPanels==4",
                                       sidebarPanel(width = 4,
                                                    selectInput("city_subset", "Select city to subset by",
                                                                total$city, multiple = T,
                                                                selected = c("ALHAMBRA"))
                                       )
                      ),
                      
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Bubble Plot",
                                   plotOutput("bubble_plot", hover = hoverOpts(id ="bubble_plot_hover")),
                                   verbatimTextOutput("bubble_hover_info"),
                                   value = 1),
                          tabPanel("Scatter Plot",
                                   plotOutput("scatter_plot"),
                                   value = 2),
                          tabPanel("Parallel Coordinates",
                                   plotlyOutput("parallel_plot"),
                                   value = 3),
                          tabPanel("Network Diagram",
                                   sankeyNetworkOutput("network"),
                                   value = 4),
                          id = "conditionedPanels"
                        )
                      )
             ),
             
             ##############################################
             # MATT FILL IN YOUR MAP UI STUFF HERE        #
             # FILL FREE TO CHANGE BUT MAKE SURE YOU KEEP #
             # THE ORIGINAL TABPANEL RIGTH BELOW THIS     #
             # i.e. tabPanel("Maps", . . .)               #
             ##############################################
             
             
             tabPanel("Maps",
                      
                            # tabPanel("Census Map", value = 1, leafletOutput("census_map")),
                            # tabPanel("Voter - Drop Off Location Map", leafletOutput("voter_map"),
                            #            value = 2),
                            # tabPanel("Voter - Residence Location Map", leafletOutput("residence_map"),
                            #            value = 3),
                            #           
                                      
                            #############################################
                            
                            bootstrapPage(
                              div(class='mapPanel',
                            # tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                            tags$style(type = "text/css", ".mapPanel {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                            leafletOutput('map', width = '100%', height = '100%'), 
                            
                            absolutePanel(top='5%', right ='5%', # Convert this to tabs
                                          
                                          # select the data to graph
                                          radioButtons('map_graph', 'Select Map Type', 
                                                       c('Voters - By Dropoff' = 'dropoffCenter', 
                                                         'Voters - By City' = 'voterCityCenter',
                                                         'Census Map' = 'census'),
                                                       inline = T),
                                          
                                          # add radio buttons for the map selection
                                          radioButtons('map_type', 'Select Map Background', 
                                                       c('Simple' = 'positron', 
                                                         'Road Centric' = 'toner',
                                                         'Detailed' = 'mapnik'),
                                                       inline = T),
                                          
                                          # implement city selection
                                          htmlOutput("selectUI"), 
                                          
                                          # insert census UI when census map type is selected
                                          uiOutput('censusUI')
                                          
                            ),
                            
                            absolutePanel(
                              top='5%', left='5%', width = '25%', height = '80%',
                              
                              conditionalPanel(
                                condition = "input.map_graph == 'voterCityCenter'",
                                
                                absolutePanel(
                                  conditionalPanel(
                                    condition = "input.map_graph == 'voterCityCenter'", # add condition for length of cities
                                    htmlOutput('voter_statistics')
                                  ) # close inner condition
                                  
                                  , style = "width: 100%; 
                                  height: 100%;
                                  border-radius: 25px;
                                  overflow-y: auto; 
                                  background: #ffffff; 
                                  opacity: .75;
                                  display: inline-block;
                                  border-style: solid;
                                  border-color: black;
                                  border-width: 1px;
                                  padding: 20px;"
                                ) # close placement of 
                                ) # close the outer condition
                                ) # close the outer absolute panel
                              ) # div container
                            ) # bootstrap page
             ), # close the maps tabpanel     
           ################################################           
                      

             tabPanel("Voter Information",
                      mainPanel(
                        tags$div(class="header", checked=NA,
                                 list(
                                   tags$p("To check your LA County voter status,"),
                                   tags$a(href="https://www.lacounty.gov/government/elections-voting/check-voter-registration-online", "Click Here!")
                                 )
                        ), 
                        br(),br(),
                        tags$div(class="header", checked=NA,
                                 list(
                                   tags$p("To learn about LA County Election Day voting services,"),
                                   tags$a(href="https://www.lavote.net/home/voting-elections/voting-options/voting-accessibility/election-day-services", "Click Here!")
                                 )
                        ),
                        br(),br(),
                        tags$div(class="header", checked=NA,
                                 list(
                                   tags$p("For information about voting by mail,"),
                                   tags$a(href="https://www.lavote.net/home/voting-elections/voting-options/vote-by-mail","Click Here!")
                                 )
                        )
                      ))
  )
)

