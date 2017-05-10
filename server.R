
################
# SHINY SERVER #
################

############################# - segmented code

server <-  function(input, output) {
  
  # requires census  
  output$bubble_plot <- renderPlot({
    ggplot(census, aes_string(x=input$x_axis,
                              y=input$y_axis,
                              color=input$color,
                              size=input$size)) +
      geom_point() + scale_color_brewer(palette = "Greens") +
      scale_size_discrete() +
      theme_bw()
  })
  
  options(warn=-1)
  # requires census
  output$bubble_hover_info <- renderPrint({
    if(!is.null(input$bubble_plot_hover)){
      hover=input$bubble_plot_hover
      points <- nearPoints(census, hover, threshold = 50, maxpoints = 1, addDist = TRUE)
      if(nrow(points)>0){
        for(col in colnames(census)){
          cat(col, ": ", points[1,col], "\n", sep="")
        }
      }
    } else{
      cat(" ")
    }
  })
  
  # requires census
  output$scatter_plot <- renderPlot({
    validate(
      need(length(input$scatter_cols) >= 2, label = "At least 2 variables")
    )
    gg1 <- makePairs(census[,input$scatter_cols])
    
    mega <- data.frame(gg1$all, Type=rep(census[,input$scatter_color], length=nrow(gg1$all)))
    
    ggplot(mega, aes_string(x = "x", y = "y")) + 
      facet_grid(xvar ~ yvar, scales = "free") + 
      geom_point(aes(colour=Type), na.rm = TRUE, alpha=0.8) + 
      stat_density(aes(x = x, y = ..scaled.. * diff(range(x)) + min(x)), 
                   data = gg1$densities, position = "identity", 
                   colour = "grey20", geom = "line") +
      theme_bw() + scale_color_brewer(palette = "Greens") + 
      xlab("") + ylab("")
  })
  
  
  # requires cluster_data
  output$parallel_plot <- renderPlotly({
    
    
    e$clust <- as.factor(e$cluster_12)
    pc <- ggparcoord(e, columns = which(colnames(e) %in% input$parallel_cols), groupColumn = 25,
                     scale = "std", #centerObsID = 1, scaleSummary = "median",
                     missing = "exclude", showPoints = FALSE,
                     splineFactor = FALSE, alphaLines = 1, boxplot = F,
                     shadeBox = NULL, mapping = NULL) +
      scale_fill_manual(name="Cluster",
                        values=c('1' = "darkolivegreen", '2' = "darkolivegreen1", 
                                 '3' = "darkolivegreen2", '4' = "darkolivegreen3", '5' = "green1",
                                 '6' = "green", '7' = "lightgreen", '8' = "green2", 
                                 '9' = "darkolivegreen4", '10' = "green3", 
                                 '11' = "green4", '12' = "palegreen1")) +
      scale_color_manual(name="Cluster",
                         values=c('1' = "darkolivegreen", '2' = "darkolivegreen1", 
                                  '3' = "darkolivegreen2", '4' = "darkolivegreen3", '5' = "green1",
                                  '6' = "green", '7' = "lightgreen", '8' = "green2", 
                                  '9' = "darkolivegreen4", '10' = "green3", 
                                  '11' = "green4", '12' = "palegreen1")) +
      
      theme(panel.background = element_rect(fill = "white"), 
            panel.grid.major = element_line(colour = 'grey'),
            axis.text = element_text(size = 8), legend.position = "none") +
      xlab("Variables") + ylab("Scaled Values")
    
    ggplotly(pc, textfont = list(color = '#000000', size = 12))
    
  })
  
  
  # requires total
  output$network <- renderSankeyNetwork({
    
    b <- total[total$voter_city == input$city_subset,]
    r <- data.frame(start=b$voter_city,
                    end=b$dropoff_city, duration=b$duration)
    r <- na.omit(r)
    df.g <- graph.data.frame(d = r, directed = FALSE)
    wc <- cluster_walktrap(df.g)
    members <- membership(wc)
    df_d3 <- igraph_to_networkD3(df.g, group = members)
    
    sankeyNetwork(Links = df_d3$links, Nodes = df_d3$nodes, Source = "source",
                  Target = "target", Value = "value", NodeID = "name",
                  units = "TWh", 
                  fontSize = 12, nodeWidth = 50, NodeGroup = "name")
    
    
  })
  
  
  # ============================================================================================================================
  # -----------------------------------------------------------------                                               MAP CREATION
  # ============================================================================================================================

  get_choices <- function(map_graph){
    if (map_graph == 'voterCityCenter') {
      return(unique(sort(voter_lookup$voter_city)))
    }
    else if (map_graph == 'census'){
      return(list(
        'Select All Dropoff Locations' = c('Select All Dropoff Locations'),
        'By City' = unique(sort(voter_lookup$dropoff_city))
      ))
    } else {
      return(list(
        'By Group' = c('Select All Voters', 'Select All Dropoff Locations'),
        'By City' = unique(sort(voter_lookup$dropoff_city))
      ))
    } # close the if/else conditions
  } # Close the get_choices function 
  
  output$selectUI <- renderUI({ 
    selectInput('voter_cities', 'Select Point of Interest:',
                choices = if(is.null(input$map_graph)) { NULL } else { c(get_choices(input$map_graph)) }, 
                selectize = T, 
                multiple = T
    ) 
  })
  
  observe({
    
    if (input$map_graph == 'census') {
      
      output$censusUI <- renderUI({selectInput('selectCensusData', 'Select Census Data:',
                                               choices = 'Percent of Households above $200k'
      )}) } else {
        output$censusUI <- renderText({ '' })
      } # close condition if/else
    
  }) # Close the observe (insert census dropdown)
  
  # set cities added and removed from selection used to color and draw on the map
  selections <- reactiveValues(
    new_cities=NULL,
    city_colors = data.frame(city = NA, color_id = NA, color = NA)
  )
  
  
  previous_ <- reactive({ 
    
    print (input$map_graph ) # to trigger this reaction - Clears the selection
    isolate(previous_cities <- selections$previous_cities)
    selections$previous_cities <- input$voter_cities
    return(previous_cities)
  })
  
  new_ <- reactive({ 
    previous_cities <- previous_()
    new_cities <- input$voter_cities[!(input$voter_cities %in% previous_cities )]
    return(new_cities)
  })
  
  removed_ <- reactive({ 
    previous_cities <- previous_()
    removed_cities <- previous_cities[!(previous_cities %in% input$voter_cities )]
    return(removed_cities)
  })
  
  
  
  observe({
    print (paste0('The observed input: ', paste0(input$voter_cities, collapse = ', ')))
    print (paste0('The prior input: ', paste0(previous_(), collapse = ', ')))
    print (paste0('The new additions: ', paste0(new_(), collapse = ', ')))
    print (paste0('The removed locations: ', paste0(removed_(), collapse = ', ')))
  })
  
  
  
  # -----------------------------------------------------------------  Create The Base Map/Change
  # =============================================================================================
  
  
  
  output$map <- renderLeaflet({
    
    # fill this in to be dropoff locations - colored in grey
    m <- leaflet() %>%
      setView(lat = 34.052235, lng = -118.243683, zoom = 10) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Positron") 
    
    
  })
  
  
  observe({
    
    mapList <- list(
      'positron' = providers$CartoDB.Positron, 
      'toner' = providers$Stamen.TonerLite, 
      'mapnik' = providers$OpenStreetMap.Mapnik
    )
    
    m <- leafletProxy('map') %>% clearTiles() %>% addProviderTiles(mapList[[input$map_type]], group = "Toner Lite") 
    
  })
  
  
  # this function handles changes between map selections (reset to base values)
  observe({
    
    # DON'T REMOVE - Triggers reaction
    print (input$map_graph)
    # fill this in to be dropoff locations - colored in grey
    m <- leafletProxy('map')  %>% clearMarkers() %>% clearShapes() %>% clearControls()
    # Here we need to take care of groups for cities and dropoffs - (split between both or one group)
    
    
  })
  
  
  
  
  # --------------------------------------------  Create Voter City Centric Dropoff Graph Objects
  # =============================================================================================
  
  voter_circles <- reactive({ 
    
    voter_ids <- voter_lookup %>% 
      filter(voter_city %in% new_()) %>%
      dplyr::select(vid)
    
    voter_circles <- g_d %>% filter(graph_item == 'Voter Circles', vid %in% unlist(voter_ids))
    return(voter_circles) 
    
  })
  
  dropoff_circles <- reactive({ 
    
    dropoff_cities <- voter_lookup %>% 
      filter(voter_city %in% new_()) %>%
      dplyr::select(dropoff_city) %>% 
      distinct(dropoff_city) 
    
    dropoff_circles <- g_d %>% filter(graph_item == 'Dropoff Circles', city %in% unlist(dropoff_cities)) 
    return(dropoff_circles)
    
  })
  
  voterDropoff_circles <- reactive({ 
    
    voterDropoff_circles <- g_d %>% filter(graph_item == 'voterDropoff Circles', city %in% new_())
    
    return(voterDropoff_circles) 
    
  })
  
  
  dropoffDropoff_circles <- reactive({ 
    
    dropoffDropoff_circles <- g_d %>% filter(graph_item == 'dropoffDropoff Circles', city %in% new_())
    return(dropoffDropoff_circles) 
    
  })
  
  
  # PURELY TESTING
  observe({
    print('At Testing:')
    print(head(voterDropoff_circles() %>% filter(city %in% 'Select All Voters')))
  })
  
  
  # can i get rid of this?
  unvisited_dropoff_circles <- reactive({ 
    
    dropoff_cities <- voter_lookup %>% 
      filter(voter_city %in% input$voter_cities) %>%
      dplyr::select(dropoff_city) %>% 
      distinct(dropoff_city) 
    
    univisited_dropoff_circles <- g_d %>% filter(graph_item == 'Dropoff Circles', !(city %in% unlist(dropoff_cities))) 
    return(univisited_dropoff_circles)
    
  })
  
  # city circles
  city_circles <- reactive({ 
    
    city_circles <- g_d %>% filter(graph_item == 'City Circles', city %in% unlist(new_())) 
    return(city_circles)
    
  })
  
  city_dropoff_lines <- reactive({ 
    
    lines = g_d %>% filter(graph_item == 'City to Dropoff Lines')
    voter_cities <- lines %>% filter((city %in% unlist(new_())) & location == 'city')
    
    if (nrow(lines) > 0) {
      line_points <- rbind(voter_cities, lines[lines$vid %in% (voter_cities$vid + 1), ]) %>%
        arrange(vid)
    } else {
      line_points <- NULL
    }
    
    return(line_points)
  })
  
  
  
  
  # ----------------------------------------------------------------  Make City Color Assignments
  # =============================================================================================
  
  
  ## assign colors to selected cities
  city_color_assignments <- reactive({
    
    # really cool colors that i stole from some guy on the line
    super_colors <- c("#00008F", "#0000EA", "#0047FF", "#00A2FF", "#00FEFF", "#5AFFA5", "#B5FF4A", "#FFED00", "#FF9200", "#FF3700", "#DB0000", "#800000")
    # # assign colors to cities randomly
    assignments <- lapply(new_(), function(x) sample(super_colors, 1))
    names(assignments) <- new_()
    
    # add colors in to dataframe
    colorFrame <- isolate(selections$city_colors)
    colorFrame <- rbind(colorFrame, c())
    
    return(assignments)
    
  })
  
  
  
  # -----------------------------------------------  Map Voter City Centric Dropoff Graph Objects
  # =============================================================================================
  # Lealflet proxy
  observe({
    
    if ( 'voterCityCenter' %in% input$map_graph) {
      # subset the spatial data
      city_boundaries <- subset(boundary.data.adj, boundary.data.adj$CITY_NAME %in% selections$new_cities)
      
      # set the city colors - Want to keep color of cities previously selected?
      # randomize new selections
      # pal <- colorNumeric( # colors stolen from online (12 set from Tim i think) - https://www.r-bloggers.com/the-paul-tol-21-color-salute/
      #  colors <- c("#00008F", "#0000EA", "#0047FF", "#00A2FF", "#00FEFF", "#5AFFA5", "#B5FF4A", "#FFED00", "#FF9200", "#FF3700", "#DB0000", "#800000"), 
      # dropoff_data$color_id)
      
      # clear groups that are unselected
      all_voter_cities <- unique(voter_lookup$voter_city)
      all_dropoff_cities <- unique(voter_lookup$dropoff_city)
      m <- leafletProxy('map') %>%
        clearGroup(removed_()) %>%
        clearGroup( paste0('dropoff_', unique(unvisited_dropoff_circles()$city)) ) %>%
        clearGroup( paste0('univisited_dropoff_', unique(dropoff_circles()$city)) )
      
      
      # add city polygons
      if (length(new_() > 0)) {
        for (i in c(1:length(new_()))) {
          
          # subset the spatial data
          city_boundary <- subset(boundary.data.adj, boundary.data.adj$CITY_NAME == new_()[i])
          m <- addPolygons(m, data=city_boundary, 
                           fillColor = 'skyblue', 
                           fillOpacity = .5,
                           stroke = T, 
                           color = 'black',
                           weight = .5, 
                           group = new_()[i]
          ) # close polygon function
        } # close city loop
      } # close if statement
      
      
      # place circles for voters
      if (length(new_() > 0)) {
        for (i in c(1:nrow(voter_circles()))) {
          m <- addCircleMarkers(m, 
                                lng = voter_circles()$long[i], 
                                lat = voter_circles()$lat[i],
                                color = 'black',
                                fillColor = city_color_assignments()[[voter_circles()$city[i]]], 
                                weight = .5, 
                                radius = 4, 
                                fillOpacity = .5, 
                                group = voter_circles()$city[i]
          ) # close marker function
        } # close voter loop
      } # close if statement
      
      # place icons for visited dropoff cities
      if (length(new_()) > 0) {
        for (i in c(1:nrow(dropoff_circles()))) {
          m <- addMarkers(m,
                          lng = dropoff_circles()$long[i], 
                          lat = dropoff_circles()$lat[i],
                          icon = icon_index[[dropoff_circles()$city[i]]][[1]],  #Scale icon by size?
                          group = paste0('dropoff_', dropoff_circles()$city[i])
          ) # close marker function
        } # close dropoff loop
      } # close if statement
      
      
      # place icons for unvisited dropoff cities
      m <- addCircles(m,
                      lng = unvisited_dropoff_circles()$long, 
                      lat = unvisited_dropoff_circles()$lat,
                      color = 'black',
                      fillColor = 'grey', 
                      weight = .5, 
                      radius = 75, 
                      fillOpacity = .5, 
                      group = paste0('univisited_dropoff_', unvisited_dropoff_circles()$city)
      )
      
      
      # place lines between dropoff and cities
      for (i in c(1:nrow(city_dropoff_lines())/2)) {
        line_segment <- city_dropoff_lines()[c(2*i - 1, 2*i), ]
        
        setLineWeight <- function(x) {
          if (is.na(x)) {return(NA)}
          else if (x < 2) { return(2) } 
          else { return (x) }
        }
        line_weight <- setLineWeight(10 * line_segment$scale[1])
        
        m <- addPolylines(m, 
                          lng = line_segment$long, 
                          lat = line_segment$lat, 
                          color = city_color_assignments()[[line_segment$city[1]]], 
                          weight =  line_weight, 
                          fillOpacity = .3, 
                          group = line_segment$city[1])
      }
    } # Close the if statement for voter by city
    
  })
  
  # --------------------------------------------  Map Voter Dropoff Centric Dropoff Graph Objects
  # =============================================================================================
  
  observe({
    
    if (length(removed_) > 0) {
      # clear groups that are unselected
      all_voter_cities <- unique(voter_lookup$voter_city)
      all_dropoff_cities <- unique(voter_lookup$dropoff_city)
      m <- leafletProxy('map')  %>%
        # Here we need to take care of groups for cities and dropoffs - (split between both or one group)
        clearGroup( removed_() )
    }
    
    
    if ( 'dropoffCenter' %in% input$map_graph && length(new_()) > 0) {
      
      # set the city colors - Want to keep color of cities previously selected?
      # randomize new selections
      # pal <- colorNumeric( # colors stolen from online (12 set from Tim i think) - https://www.r-bloggers.com/the-paul-tol-21-color-salute/
      #  colors <- c("#00008F", "#0000EA", "#0047FF", "#00A2FF", "#00FEFF", "#5AFFA5", "#B5FF4A", "#FFED00", "#FF9200", "#FF3700", "#DB0000", "#800000"), 
      # dropoff_data$color_id)
      
      
      # place circles for voters
      if (!(new_() %in% c('Select All Voters', 'Select All Dropoff Locations'))) {
        for (i in c(1:nrow(voterDropoff_circles()))) {
          m <- addCircleMarkers(m, 
                                lng = voterDropoff_circles()$long[i], 
                                lat = voterDropoff_circles()$lat[i],
                                color = 'black',
                                fillColor = city_color_assignments()[[voterDropoff_circles()$city[i]]], 
                                weight = .25, 
                                radius = 2, 
                                stroke = T, 
                                fillOpacity = 1, 
                                group = voterDropoff_circles()$city[i]
          ) # close marker function
        } # close voter loop
      } # close if statement
      
      # place circles for dropoff locations (3 Mile Radius)
      if(!(new_() %in% c('Select All Voters', 'Select All Dropoff Locations'))) {
        for (i in c(1:nrow(dropoffDropoff_circles()))) {
          m <- addCircles(m, 
                          lng = dropoffDropoff_circles()$long[i], 
                          lat = dropoffDropoff_circles()$lat[i],
                          color = 'black',
                          fillColor = city_color_assignments()[[dropoffDropoff_circles()$city[i]]], 
                          weight = 1, 
                          radius = 4828,  # approximately 3 miles in meters
                          fillOpacity = .15, 
                          stroke = T, 
                          group = dropoffDropoff_circles()$city[i]
          ) # close marker function
        } # close voter loop
      } # close if statement
      
      # place circles for all voters
      if(new_() %in% 'Select All Voters') {
        for (i in c(1:nrow(voterDropoff_circles()))) {
          m <- addCircleMarkers(m, 
                                lng = voterDropoff_circles()$long[i], 
                                lat = voterDropoff_circles()$lat[i],
                                color = 'black',
                                fillColor = city_color_assignments()[[voterDropoff_circles()$color_id[i]]], 
                                weight = .25, 
                                radius = 2,  # approximately 3 miles in meters
                                fillOpacity = 1, 
                                stroke = T, 
                                group = 'Select All Voters'
          ) # close marker function
        } # close voter loop
      } # close if statement
      
      # place circles for all dropoff centers
      if(new_() %in% 'Select All Dropoff Locations') {
        for (i in c(1:nrow(dropoffDropoff_circles()))) {
          m <- addCircles(m, 
                          lng = dropoffDropoff_circles()$long[i], 
                          lat = dropoffDropoff_circles()$lat[i],
                          color = 'black',
                          fillColor = city_color_assignments()[[dropoffDropoff_circles()$color_id[i]]], 
                          weight = 1, 
                          radius = 4828,  # approximately 3 miles in meters
                          fillOpacity = .15, 
                          stroke = T, 
                          group = 'Select All Dropoff Locations'
          ) # close marker function
        } # close voter loop
      } # close if statement
      
      
    } # close the map selection if statement
  })
  
  
  # --------------------------------------------                          Dropoff Statistics HTML
  # =============================================================================================
  
  # Create sidebar summary statistics for voter dropoffs
  output$voter_statistics <- renderUI({
    if (is.null(input$map_graph)) { print ('pass') }
    else if (input$map_graph %in% 'voterCityCenter') {
      
      # for each dropoff location generate statistics
      # get list of dropoff locations
      dropoff_text_summary <- function(city) {
        
        n_city_voters <- length(query_data$voter_city[query_data$voter_city == city])
        
        # list of dropoff locations
        subset_data <- query_data %>% filter(voter_city %in% city) %>%
          group_by(dropoff_city) %>%
          dplyr::summarize(
            number_voters = n(), 
            mean_distance = round(mean(distance),2),
            address = dplyr::first(dropoff_address)
          ) %>%
          dplyr::mutate(
            percent_voters = round(100*number_voters/n_city_voters,2)
          ) %>% 
          dplyr::arrange(
            desc(number_voters)
          )
        
        arrange_text <- function(row) {
          paste(
            paste('<strong>', row['dropoff_city'], '</strong>'),   # dropoff city
            paste(row['address']),                                 # address
            paste('Number of Voters: ', row['number_voters']),     # number of voters
            paste('Percent of Voters: ', row['percent_voters'], '%'), 
            paste('Average Miles Driven: ', row['mean_distance']),
            '<br/>',
            sep='<br/>')
        } 
        
        dropoff_summary <- apply(subset_data, 1, arrange_text)
        
        dropoff_summary <- paste0(
          h1(city), '<br/',
          HTML(dropoff_summary), 
          '-----------------------<br/>',
          sep = '<br/>'
        )
        
        return(HTML(dropoff_summary))
        
      }
      
      dropoff_list <- lapply(input$voter_cities, dropoff_text_summary)
      return(dropoff_list)
      
    } # Close the voterCityCenter if statement
    
  })
  
  
  # --------------------------------------------                                       CENSUS MAP
  # =============================================================================================
  
  
  observe({
    
    if (input$map_graph == 'census') {
      # UI FOR CENSUS INFORMATION TO SELECT
      m <- leafletProxy('map') 
      # statically assigned for now
      
      censusData <- income200k # lookup function with input$selectCensusData
      
      
      # merge the data together
      map_joined_Data <- geo_join(censusShapeFiles, income200k, 'GEOID', 'GEOID')
      map_joined_Data <- map_joined_Data[map_joined_Data$ALAND>0, ]
      
      # make the map
      popup <- paste0('GEOID:', censusData$GEOID, '<br>', 'Percent of the Households above $200k: ', # Lookup function here as well
                      round(censusData$percent, 2))
      
      # Custom palete?
      pal <- leaflet::colorNumeric(
        palette = 'YlGnBu', # lookup function here
        domain = censusData$percent # this must be consistent
      ) 
      
      
      # add the shapefiles
      m <- m %>% addPolygons(data = map_joined_Data, 
                             fillColor = ~pal(percent), 
                             color = '#b2aeae', 
                             fillOpacity = 0.7, 
                             weight = 1, 
                             smoothFactor = 0.2, 
                             popup = popup, 
                             group = 'census') %>%
        addLegend(pal = pal, 
                  values = censusData$percent, 
                  position = 'bottomright', 
                  title = 'Percent of Households<br>above $200k', # lookup function here as well
                  labFormat = labelFormat(suffix = '%'))
      
    } # close map selection
    
  }) 
  
  
  observe({
    
    m <- leafletProxy('map') 
    
    if ( 'census' %in% input$map_graph && length(new_()) > 0) {
      
      
      # select the census shape files
      # select the census information
      # merge the data 
      # construct the popup
      # assign the color
      # add to the map
      # render the map
      # add the dropoff points / dropoff points
      
      # for each selection of point - clear/layer replace the map
      # (maybe not popups)
      
      print(dropoffDropoff_circles()$scale)
      
      if(!(new_() %in% c('Select All Dropoff Locations'))) {
        for (i in c(1:nrow(dropoffDropoff_circles()))) {
          m <- addCircleMarkers(m, 
                                lng = dropoffDropoff_circles()$long[i], 
                                lat = dropoffDropoff_circles()$lat[i],
                                color = 'black',
                                fillColor = city_color_assignments()[[dropoffDropoff_circles()$city[i]]], 
                                weight = 1, 
                                radius = 1000*dropoffDropoff_circles()$scale[[i]],  # approximately 3 miles in meters
                                fillOpacity = .25, 
                                stroke = T, 
                                group = dropoffDropoff_circles()$city[i]
          ) # close marker function
        } # close voter loop
      } # close if statement
      
      if(new_() %in% 'Select All Dropoff Locations') {
        for (i in c(1:nrow(dropoffDropoff_circles()))) {
          m <- addCircleMarkers(m, 
                                lng = dropoffDropoff_circles()$long[i], 
                                lat = dropoffDropoff_circles()$lat[i],
                                color = 'black',
                                fillColor = city_color_assignments()[[dropoffDropoff_circles()$color_id[i]]], 
                                weight = 1, 
                                radius = 1000*dropoffDropoff_circles()$scale[[i]],  # approximately 3 miles in meters
                                fillOpacity = .25, 
                                stroke = T, 
                                group = 'Select All Dropoff Locations'
          ) # close marker function
        } # close voter loop
      } # close if statement
      
    } # close the check to see if new_() > 0  
    
  }) # Close the Census map observer
  

  
  ##################################
  # MATT ADD RENDER MAP STUFF HERE #
  ##################################
  
  
  
}

# shinyApp(ui, server)
