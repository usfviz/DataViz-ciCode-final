
# take in voter city selections, dropoff or city
# plot the mean distance travelled to the top 5 locations
# plot a piechart of breakdown

# --------------------------------------------------------------------------------------------------- Dropoff Plots


dropoffRingPlot <- function(voterCitySelections, colorFrame, dropoffCity) {
  # going to be a problem with select all -- maybe
  
  # change the colors on a previously created plot
  dropoffData <- query_data %>% select(dropoff_city, voter_city) %>%
    filter(dropoff_city == dropoffCity) %>%
    group_by(voter_city) %>% 
    dplyr::summarise(numVotes = n()) %>% 
    mutate(percentVotes = numVotes/sum(numVotes), 
           ymax = cumsum(percentVotes), 
           ymin = c(0, head(ymax, n=-1)),
           xmin = 3, 
           xmax = 4) %>%
    arrange(percentVotes)
  
  # match colors
  dropoffDataGraph <- merge(dropoffData, colorFrame, by.x = 'voter_city', by.y = 'color_id', all.x = T)
  dropoffDataGraph$color[is.na(dropoffDataGraph$color)] <- '#f5f5f5'
  
  # plotColors <- lapply(c(1:nrow(dropoffDataGraph)), function(x) dropoffDataGraph$color[x])
  plotColors <- dropoffDataGraph$color
  names(plotColors) <- dropoffDataGraph$voter_city
  
  p = ggplot(dropoffDataGraph, aes(fill=voter_city, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
    geom_rect(colour="grey30") +
    coord_polar(theta="y") + 
    scale_fill_manual(values = plotColors) + 
    xlim(c(0, 4)) +
    theme_bw() +
    theme(panel.grid=element_blank()) +
    theme(axis.text=element_blank()) +
    theme(axis.ticks=element_blank()) +
    theme(legend.position = 'none', 
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title = element_blank())
  
  return(p)
  
}


dropoffBarPlot <- function(voterCitySelections, colorFrame, hoverValue) {

  print('here')
  colorFrame <- rbind(colorFrame, c('Overall Mean Dist.', 'Overall Mean Dist.', '#f5f5f5'))
  print('input')
  print(voterCitySelections)
  print('colors')
  print(colorFrame)
  print('selected city')
  print(hoverValue)
  
  # subset the data to just to the relevant dropoff location and the 
  # relevant colors
  
  overallMean <- query_data %>% select(dropoff_city, voter_city, distance) %>%
    filter(dropoff_city == hoverValue) %>% 
    group_by(dropoff_city) %>% summarise(distance = mean(distance)) %>%
    ungroup() %>%
    mutate(voter_city = 'Overall Mean Dist.')
  dropoffData <- query_data %>% select(dropoff_city, voter_city, distance) %>%
    filter(dropoff_city == hoverValue, voter_city %in% voterCitySelections) 
  
  dropoffData <- rbind(dropoffData, overallMean)
  dropoffData <- dropoffData %>% group_by(voter_city) %>%
    dplyr::summarise(meanDistance = mean(distance)) %>%
    mutate(meanDistance = round(meanDistance, 2)) %>%
    arrange(meanDistance) %>%
    mutate(voter_city = factor(voter_city, levels = getArrangement(voter_city), ordered = T))

  dropoffData <- dropoffData[c(1:5), ]
  dropoffData <- dropoffData[complete.cases(dropoffData), ]
  
  # match colors
  dropoffDataGraph <- merge(dropoffData, colorFrame, by.x = 'voter_city', by.y = 'color_id')
  dropoffDataGraph <- dropoffDataGraph[complete.cases(dropoffDataGraph), ]
  
  # plotColors <- lapply(c(1:nrow(dropoffDataGraph)), function(x) dropoffDataGraph$color[x])
  plotColors <- dropoffDataGraph$color
  names(plotColors) <- dropoffDataGraph$voter_city
  
  p <- ggplot(data=dropoffData, aes(x=voter_city,y=meanDistance,fill=voter_city, label=meanDistance)) +
    geom_bar(stat="identity", width = .1, color = 'black', alpha = .75) + 
    geom_text(hjust = -0.2, size = 5,
              position = position_dodge(width = .5)) + 
    coord_flip(ylim = c(0,1.25*max(dropoffData$meanDistance, na.rm= T))) +
    scale_fill_manual(values = plotColors) +
    theme_bw() +
    theme(panel.grid=element_blank()) +
    theme(axis.text.x=element_blank()) +
    theme(axis.ticks=element_blank()) +
    theme(axis.title=element_blank()) + 
    theme(legend.position = 'none', 
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title = element_blank())

  return(p)
}


dropoffTitleSummary <- function(voterCitySelections, dropoffCity) {
  
  titleData <- query_data %>% select(dropoff_city, dropoff_address, voter_city, distance) %>%
    filter(dropoff_city == dropoffCity) %>% 
    mutate(lessThan = ifelse(distance <= 3, 1, 0)) %>%
    dplyr::summarize(address = first(dropoff_address), 
              distance = mean(distance), 
              nVoters = n(), 
              percentLess = sum(lessThan), 
              numCities = n_distinct(voter_city)) %>%
    mutate(percentLess = percentLess/nVoters)
  
    paste0( 
    paste(
    paste0('<font size ="+3"><strong>', dropoffCity, '</strong></font>', '<br/>'),  # dropoff city
    paste0(make_html(titleData$address))  # dropoff address
    ), # header 
    '<hr>',
    "<b>Dropoff Loc. Summary Statistics</b>",
    paste('<table cellspacing=0 cellpadding = 0>',
      paste0(fixed_width('Number of Voters: ', titleData$nVoters, 6)),   # number of voters
      paste0(fixed_width('Travelled less than 3 miles: ', paste0(round(100*titleData$percentLess,1), '%'), 6)),   # percent less 
      paste0(fixed_width('Number of Cities Dropping Here: ', titleData$numCities, 6)),   # number of cities
      paste0(fixed_width('Mean Distance Travelled: ', round(titleData$distance,2), 6)), # distance travelled
    '<table/>') # summary statistics
    , '<br/>' ) # close html text
    
}


# --------------------------------------------------------------------------------------------------- VoterCity Plots



voterRingPlot <- function(colorFrame, voter.city) {
  
  # would like to interpolate a color palette from a single color
  pal <- colorFrame
  
  # change the colors on a previously created plot
  dropoffData <- query_data %>% select(dropoff_city, voter_city) %>%
    filter(voter_city == voter.city) %>%
    group_by(dropoff_city) %>% 
    dplyr::summarise(numVotes = n()) %>% 
    mutate(percentVotes = numVotes/sum(numVotes), 
           ymax = cumsum(percentVotes), 
           ymin = c(0, head(ymax, n=-1)),
           xmin = 3, 
           xmax = 4, 
           color = sample(pal, n_distinct(dropoff_city), replace = T)) %>%
    arrange(percentVotes)
  
  # match colors
  # dropoffDataGraph <- merge(dropoffData, colorFrame, by.x = 'voter_city', by.y = 'color_id', all.x = T)
  # dropoffDataGraph$color[is.na(dropoffDataGraph$color)] <- '#f5f5f5'
  
  # plotColors <- lapply(c(1:nrow(dropoffDataGraph)), function(x) dropoffDataGraph$color[x])
  plotColors <- dropoffData$color
  names(plotColors) <- dropoffData$voter_city
  
  p = ggplot(dropoffData, aes(fill=dropoff_city, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
    geom_rect(colour="grey30") +
    coord_polar(theta="y") + 
    scale_fill_manual(values = plotColors) + 
    xlim(c(0, 4)) +
    theme_bw() +
    theme(panel.grid=element_blank()) +
    theme(axis.text=element_blank()) +
    theme(axis.ticks=element_blank()) +
    theme(legend.position = 'none', 
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title = element_blank())
  
  return(p)
  
}


voterBarPlot <- function(colorFrame, hoverValue) {
  
  # colorFrame <- rbind(colorFrame, c('Overall Mean Dist.', 'Overall Mean Dist.', '#f5f5f5'))
  
  # subset the data to just to the relevant dropoff location and the 
  # relevant colors
  
  # would like to interpolate a color palette from a single color
  pal <- colorFrame
  
  # overall data - summarized by distance
  overallMean <- query_data %>% select(dropoff_city, voter_city, distance) %>%
    filter(voter_city == hoverValue) %>% 
    group_by(voter_city) %>% summarise(distance = mean(distance)) %>%
    ungroup() %>%
    mutate(dropoff_city = 'Overall Mean Dist.')
  
  # dropoff-wise data to stack
  dropoffData <- query_data %>% select(dropoff_city, voter_city, distance) %>%
    filter(voter_city == hoverValue) 
  
  print('here')
  print(overallMean)
  print(dropoffData)
  
  # merge, summarise and arrange
  dropoffData <- rbind(dropoffData, overallMean)
  dropoffData <- dropoffData %>% group_by(dropoff_city) %>%
    dplyr::summarise(meanDistance = mean(distance), voter_city = first(voter_city)) %>%
    mutate(meanDistance = round(meanDistance, 2)) %>%
    arrange(meanDistance) %>%
    mutate(dropoff_city = factor(dropoff_city, levels = getArrangement(dropoff_city), ordered = T), 
           color = sample(pal, n_distinct(dropoff_city), replace = T))

  # dropoffDataGraph <- merge(dropoffData, colorFrame, by.x = 'voter_city', by.y = 'color_id')
  # dropoffDataGraph <- dropoffDataGraph[complete.cases(dropoffDataGraph), ]
  
  # plotColors <- lapply(c(1:nrow(dropoffDataGraph)), function(x) dropoffDataGraph$color[x])
  plotColors <- dropoffData$color
  names(plotColors) <- dropoffData$dropoff_city
  
  print('thisisdropoffstuff')
  print(str(dropoffData))
  print(plotColors)
  
  p <- ggplot(data=dropoffData, aes(x=dropoff_city,y=meanDistance,fill=dropoff_city, label=meanDistance)) +
    geom_bar(stat="identity", width = .1, color = 'black', alpha = .75) + 
    geom_text(hjust = -0.2, size = 5,
              position = position_dodge(width = .5)) + 
    coord_flip(ylim = c(0,1.25*max(dropoffData$meanDistance, na.rm= T))) +
    scale_fill_manual(values = plotColors) +
    theme_bw() +
    theme(panel.grid=element_blank()) +
    theme(axis.text.x=element_blank()) +
    theme(axis.ticks=element_blank()) +
    theme(axis.title=element_blank()) + 
    theme(legend.position = 'none', 
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title = element_blank())
  
  return(p)
}

voterTitleSummary <- function(voter.city) {
  
  titleData <- query_data %>% select(dropoff_city, dropoff_address, voter_city, distance) %>%
    filter(voter_city == voter.city) %>% 
    mutate(lessThan = ifelse(distance <= 3, 1, 0)) %>%
    dplyr::summarize(distance = mean(distance), 
                     nVoters = n(), 
                     percentLess = sum(lessThan), 
                     numCities = n_distinct(dropoff_city)) %>%
    mutate(percentLess = percentLess/nVoters)
  
  paste0( 
    paste(
      paste0('<font size ="+3"><strong>', voter.city, '</strong></font>')  # dropoff city
    ), # header 
    '<hr>',
    "<b>Voter City Summary Statistics</b>",
    paste('<table cellspacing=0 cellpadding = 0>',
          paste0(fixed_width('Number of Voters: ', titleData$nVoters, 6)),   # number of voters
          paste0(fixed_width('Travelled less than 3 miles: ', paste0(round(100*titleData$percentLess,1), '%'), 6)),   # percent less 
          paste0(fixed_width('Number of Dropoff Loc. Used: ', titleData$numCities, 6)),   # number of cities
          paste0(fixed_width('Mean Distance Travelled: ', round(titleData$distance,2), 6)), # distance travelled
          '<table/>') # summary statistics
    , '<br/>', '<br/>') # close html text
  
}

# --------------------------------------------------------------------------------------------------- Generate Plots

createPlot <- function(voterCitySelections, colorFrame, hoverValue, plotType) {
  
  plotList = list()
  
  if (plotType == 'dropoff') {
    
    plotList[['doughnut']] <- dropoffRingPlot(voterCitySelections, colorF, hoverValue)
    plotList[['bar']] <- dropoffBarPlot(voterCitySelections, colorF, hoverValue)
    
  } else if (plotType == 'voterCity') {
    print ('pass')
  }
  
  return(plotList)
}

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: TESTING



# createPlot(colorF$city, colorF, "West Hollywood", 'dropoff')

# dropoffBarPlot(colorF$city, colorF, "West Hollywood")
# dropoffRingPlot('West Hollywood', colorF, "West Hollywood")





















# unique(query_data$dropoff_city)
# voter_circles$city
# 
# # Create test data.
# dat = data.frame(count=c(10, 60, 30), category=c("A", "B", "C"))
# dat
# # Add addition columns, needed for drawing with geom_rect.
# dat$fraction = dat$count / sum(dat$count)
# dat
# dat = dat[order(dat$fraction), ]
# dat$ymax = cumsum(dat$fraction)
# dat$ymin = c(0, head(dat$ymax, n=-1))
# 
# 
# 
# 
# 
# # Create test data.
# dat = data.frame(count=c(10, 60, 30), category=c("A", "B", "C"))
# 
# # Add addition columns, needed for drawing with geom_rect.
# dat$fraction = dat$count / sum(dat$count)
# dat = dat[order(dat$fraction), ]
# dat$ymax = cumsum(dat$fraction)
# dat$ymin = c(0, head(dat$ymax, n=-1))
# 
# p1 = ggplot(dat, aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
#   geom_rect() +
#   coord_polar(theta="y") +
#   xlim(c(0, 4)) +
#   labs(title="Basic ring plot")
# 
# p2 = ggplot(dat, aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
#   geom_rect(colour="grey30") +
#   coord_polar(theta="y") +
#   xlim(c(0, 4)) +
#   theme_bw() +
#   theme(panel.grid=element_blank()) +
#   theme(axis.text=element_blank()) +
#   theme(axis.ticks=element_blank()) +
#   labs(title="Customized ring plot")
# 
# 
# library(gridExtra)
# png("ring_plots_1.png", height=4, width=8, units="in", res=120)
# grid.arrange(p
# 
