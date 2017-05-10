
# ----------------------------------                     READ IN CITY BOUNDARY SHAPEFILES
# =======================================================================================



boundary.data <- rgdal::readOGR(dsn='./data/DPW_CITY_BOUNDARIES', layer='DPW_CITY_BOUNDARIES',
                         GDAL1_integer64_policy = TRUE)
# boundary.data.adj <- spTransform(boundary.data, CRS("+proj=longlat'"))
boundary.data.adj <- sp::spTransform(boundary.data, CRSobj = CRS("+proj=longlat +ellps=clrk66"))

# ------> boundary.data.adj


# ----------------------------------                            READ IN CENSUS SHAPEFILES
# =======================================================================================

# the shapefiles for the census tracts
censusShapeFiles <- readRDS('./data/census/censusTracts.RDS')
#censusShapeFiles <- readOGR(dsn="CENSUS/CENSUS_SHAPE_FILES.new", layer = 'tracts')
class(censusShapeFiles)

# census Data
income200k <- readRDS('./data/census/censusData200K.RDS')

# ----------------------------------                                 STRUCTURE QUERY DATA
# =======================================================================================

voter_data <- read.csv('./data/voter_data_v4.csv', stringsAsFactors = F)

# remove NA values
voter_data <- voter_data[complete.cases(voter_data),]
# voter_data <- voter_data[c(1:1000), ]

# to normalize city names
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

# normalize voter city names (they are all caps)
voter_data$voter_city <- unlist(lapply(tolower(voter_data$voter_city), simpleCap))

# create queary for data
query_data <- voter_data

# ------> query_data


# ----------------------------------                                           MAKE ICONS
# =======================================================================================


dropoffIcons <- lapply(list.files('./data/icons/'), function(x) {
  makeIcon(
    iconUrl = paste0('./data/icons/', x),
    iconWidth = 20,
    iconHeight = 20
  ) 
})

dropoff_cities <- unique(query_data$dropoff_city)
icon_index <- lapply(dropoff_cities, function(x) sample(dropoffIcons,1))
names(icon_index) <- dropoff_cities

# ------> dropoffIcons


# ----------------------------------                               STRUCTURE MAP OBJECTS
# =======================================================================================

voter_lookup <- voter_data[c('vid', 'voter_city', 'dropoff_city')]

# stack voter and dropoff latitudes and longitudes
voter_data <- voter_data %>% dplyr::select(vid, voter_lat, voter_long, dropoff_lat, dropoff_long, dropoff_city, voter_city, city_coord) %>%
  mutate(voter = paste(voter_long, voter_lat), 
         dropoff = paste(dropoff_long, dropoff_lat), 
         city = city_coord) %>% # vanity
  dplyr::select(-voter_long, -voter_lat, -dropoff_long, -dropoff_lat, -city_coord) %>%
  gather(location, coordinates, -vid, -dropoff_city, -voter_city) %>%
  gather(city_type, city, -vid, -location, -coordinates) %>%
  # remove redundant duplications
  filter(!(location != 'dropoff' & city_type == 'dropoff_city'), 
         !(location == 'dropoff' & city_type == 'voter_city')) %>%
  dplyr::select(-city_type) %>%
  arrange(vid)

# peel out individual latitudes and longitudes
voter_data$long <- unlist(lapply(voter_data$coordinates, function(x) as.numeric(unlist(strsplit(x, ' '))[[1]])))
voter_data$lat <- unlist(lapply(voter_data$coordinates, function(x) as.numeric(unlist(strsplit(x, ' '))[[2]])))

# drop coordinates
voter_data$coordinates <- NULL
seed_data <- voter_data %>% arrange(vid, location)

# create city lookup for ease of color_id
cities <- sort(unique(seed_data$city))
city_lookup <- lapply(c(1:length(cities)), c)
names(city_lookup) <- cities

# ----------------------------------------------------------------------------------- CITY CENTRIC GRAPH OBJECTS

# ----------------------------------------------------------- VOTER CIRCLES
voter_circles <- seed_data %>% 
  filter(location == 'voter') %>%
  mutate(color_id = unlist(city, function (x) city_lookup[[x]]), 
         scale = 1, 
         popup = 'voter address? voter statistics?', 
         graph_item = 'Voter Circles')



# ----------------------------------------------------------- CITY CIRCLES
city_circles <- seed_data %>% dplyr::filter(location == 'city') %>%
  group_by(city) %>%
  dplyr::summarize(vid = NA,
            location = 'city', 
            lat = first(lat), 
            long = first(long),
            n_voters = n()) %>%
  mutate(color_id = unlist(lapply(city, function (x) city_lookup[[x]])), 
         scale = n_voters/sum(n_voters), 
         popup = 'city address? city statistics?', 
         graph_item = 'City Circles') %>%
  dplyr::select(-n_voters)


# ----------------------------------------------------------- DROPOFF CIRCLES

dropoff_circles <- seed_data %>% filter(location == 'dropoff')  %>%
  group_by(city) %>%
  dplyr::summarize(vid = NA,
            location = 'city', 
            lat = first(lat), 
            long = first(long),
            n_voters = n()) %>%
  mutate(color_id = unlist(lapply(city, function (x) city_lookup[[x]])), 
         scale = n_voters/sum(n_voters), 
         popup = 'dropoff address? dropoff statistics?', 
         graph_item = 'Dropoff Circles') %>%
  dplyr::select(-n_voters)


# ----------------------------------------------------------- CITY-DROPOFF LINES
city_dropoff_lines <- seed_data %>% filter(location != 'voter') %>%
  arrange(vid, location)

# this data must be ordered - city dropoff city dropoff etc. 
city_dropoff_groups <- city_dropoff_lines %>% 
  group_by(vid) %>%
  dplyr::summarize(line_to = paste(city, collapse = ' to '), 
            city = first(city))

city_dropoff_statistics <- city_dropoff_groups %>% 
  group_by(city, line_to) %>% 
  dplyr::summarize(n_voters = n()) %>%
  mutate(color_id = unlist(lapply(city, function (x) city_lookup[[x]])), 
         scale = n_voters/sum(n_voters), 
         popup = NA) %>%
  ungroup() %>%
  dplyr::select(-n_voters, -city)

city_dropoff_lines <- merge(city_dropoff_lines, city_dropoff_groups[, c(1,2)], by = 'vid')
city_dropoff_lines <- city_dropoff_lines %>% group_by(location, city, line_to) %>%
  dplyr::summarize(long = first(long), lat = first(lat))

city_dropoff_lines <- merge(city_dropoff_lines, city_dropoff_statistics, by = 'line_to')
city_dropoff_lines <- city_dropoff_lines %>% 
  arrange(line_to, location) %>%
  dplyr::select(-line_to) %>%
  mutate(vid = seq_along(location), 
         graph_item = 'City to Dropoff Lines')


# ----------------------------------------------------------------------------------- CITY CENTRIC GRAPH OBJECTS

# ----------------------------------------------------------- VOTER BY DROPOFF LOCATIONS

# targets for voter circles grouped/colored by dropoff city rather than voter city
voters <- seed_data %>% filter(location == 'voter' ) %>% dplyr::select(-city)
dropoffCities <- seed_data %>% filter(location == 'dropoff') %>% dplyr::select(city, vid)
votersByDropoff <- merge(voters, dropoffCities, by = 'vid')

voterDropoff_circles <- votersByDropoff %>% 
  mutate(color_id = unlist(city, function (x) city_lookup[[x]]), 
         scale = 1, 
         popup = 'voter address? voter statistics?', 
         graph_item = 'voterDropoff Circles')

selectAllVoters <- voterDropoff_circles
selectAllVoters$city <- 'Select All Voters'

voterDropoff_circles <- rbind(voterDropoff_circles, selectAllVoters)



# ----------------------------------------------------------- DROPOFF LOCATIONS

str(query_data)
query_data$dropoff_address[1]

remove_city_zip <- function(x) {
  splitted <- stringr::str_split_fixed(x, ',', n=2)
  splitted[2] <- sub(' ', '', splitted[2])
  paste(splitted[[1]], splitted[[2]], sep = '<br/>')
}

get_address <- function(vid) {
  dropoff_address <- query_data %>% filter(vid == vid) %>% dplyr::select(dropoff_address)
  dropoff_address <- remove_city_zip(dropoff_address)
}

get_distance <- function(city) {
  meanDistance <- mean(query_data %>% filter(dropoff_city == city) %>% dplyr::select(distance))
}

PdropoffRank <- function(city, rank) {
  voterRanks <- mean(query_data %>% filter(dropoff_city == city) %>% dplyr::select(rank))
  nVoters <- length(voterRanks)
  if (rank == 1) {
    p <- sum(voterRanks <= 3)/nVoters
    return(paste0(round(100 * p, 2), '%'))
  } else if (rank == 2) {
    p <- sum(voterRanks > 3 && voterRanks <= 9)/nVoters
    return(paste0(round(100 * p, 2), '%'))
  } else {
    p <- sum(voterRanks > 9)/nVoters
    return(paste0(round(100 * p, 2), '%'))
  }
}

# summary for dropoff locations - backburner for now
dropoffDropoffLabel <- function(vid, n_voters, city) {
  paste(sep = "<br/>",
        paste0("<u><b>", city, "</b></u>"), # dropoff city
        unlist(lapply(vid, get_address)), # dropoff address
        '&nbsp;',
        paste0("<b>Voter Dropoff Statistics:</b>", paste0(rep('&nbsp;', 22), collapse = '')), 
        paste0('<table cellspacing=0 cellpadding = 0>',
               paste0(fixed_width('Number of votes dropped: ', n_voters, 4)), # Number of voters
               paste0(fixed_width('Average miles traveled: ', unlist(lapply(vid, get_address)), 4)),  # Average Distance travelled per voter
               paste0(fixed_width('Travelled less than 3 miles: ', unlist(lapply(city, get_distance)), 4)), # voters within 3 miles
               paste0(fixed_width('Is a top three location:', unlist(lapply(vid, PdropoffRank(city, 1))), 4)), # Percent of voters who dropped off (top 3)
               paste0(fixed_width('Is a 4-9th closest location:',unlist(lapply(vid, PdropoffRank(city, 2))), 4)),  # Percent of voters who dropped off (4 - 9)
               paste0(fixed_width('Is a far location:', unlist(lapply(vid, PdropoffRank(city, 3))), 4)), 
               '<table/>')
  )
}

# targets for dropoff circles this only differs from above by the popup value
dropoffDropoff_circles <- seed_data %>% filter(location == 'dropoff')  %>%
  group_by(city) %>%
  dplyr::summarize(vid = NA,
                   location = 'city', 
                   lat = first(lat), 
                   long = first(long),
                   n_voters = n()) %>%
  mutate(color_id = unlist(lapply(city, function (x) city_lookup[[x]])), 
         scale = n_voters/sum(n_voters), 
         popup = 'TBD!', 
         graph_item = 'dropoffDropoff Circles') %>%
  dplyr::select(-n_voters)

selectAllDropoff <- dropoffDropoff_circles
selectAllDropoff$city <- 'Select All Dropoff Locations'

voterDropoff_circles <- rbind(voterDropoff_circles, selectAllDropoff)





# dropoff_data$popup_label2 <- paste(sep = "<br/>",
#                                    paste0("<u><b>", dropoff_data$dropoff_city, "</b></u>"), # dropoff city
#                                    unlist(lapply(dropoff_data$dropoff_address, make_html)), # dropoff address
#                                    '&nbsp;',
#                                    paste0("<b>Voter Dropoff Statistics:</b>", paste0(rep('&nbsp;', 22), collapse = '')), 
#                                    paste0('<table cellspacing=0 cellpadding = 0>',
#                                           paste0(fixed_width('Number of votes dropped: ', dropoff_data$voters, 4)), # Number of voters
#                                           paste0(fixed_width('Average miles traveled: ', dropoff_data$dist, 4)),  # Average Distance travelled per voter
#                                           paste0(fixed_width('Travelled less than 3 miles: ', dropoff_data$voters_within_three, 4)), # voters within 3 miles
#                                           paste0(fixed_width('Is a top three location:', dropoff_data$p_small, 4)), # Percent of voters who dropped off (top 3)
#                                           paste0(fixed_width('Is a 4-9th closest location:', dropoff_data$p_medium, 4)),  # Percent of voters who dropped off (4 - 9)
#                                           paste0(fixed_width('Is a far location:', dropoff_data$p_large, 4)), 
#                                           '<table/>')
# )  # Percent of voters who dropped off the rest


# ----------------------------------------------------------- STACK GRAPH ITEMS TOGETHER
g_d <- rbind(voter_circles, city_circles, dropoff_circles, city_dropoff_lines, voterDropoff_circles , dropoffDropoff_circles)

# Create city graph variables
#city_data <- voter_data %>% filter(location == 'city_coord') %>% group_by(voter_city) %>%
#  summarise(long = first(long), lat = first(lat), voters = n()) %>%
#  mutate(scale = voters/sum(voters))
#
## Remove duplicate dropoff locations and create scale
#dropoff_data <- voter_data %>% filter(location == 'dropoff') %>% group_by(dropoff_city) %>%
#  summarise(vid = max(vid), location = max(location), long = max(long), lat = max(lat),
#            voters = n(), voter_city = max(voter_city)) %>%
#  mutate(scale = voters/sum(voters))
#
## remove dropoff information
#voter_data <- voter_data[voter_data$location== 'voter', ]
#
## add color ids to dropoff locations
#dropoff_data$color_id <- c(1:nrow(dropoff_data))
#dropoff_data$color_id <- dropoff_data$color_id %% 12
#
## add popup labels to dropoff locations
#dropoff_data$popup_label <- paste(sep = "<br/>",
#                                  paste0("<b>", dropoff_data$dropoff_city, "</b>"),
#                                  paste0(dropoff_data$voters, ' voters'))
#
#nrow(voter_data)
#nrow(dropoff_data)
#nrow(full_data)
## merge information to voters
#full_data <- merge(voter_data, dropoff_data[c('dropoff_city', 'voters', 'scale', 'color_id', 'popup_label')], by = 'dropoff_city')
#full_data <- rbind(full_data, dropoff_data)
#
## set the voter data coloring
#voter_city <- unique(voter_data$voter_city)
#voter_color_id <- lapply(c(78:c(length(voter_city) + 77)), c)
#names(voter_color_id) <- voter_city
#
## re-assign the color id to the vote location
#v <- full_data[full_data$location == 'voter', 'voter_city']
#full_data[full_data$location == 'voter', 'color_id'] <- unlist(lapply(v, function(x) voter_color_id[[x]] %% 12))
#
## create alphabetic organization
#full_data <- full_data %>% arrange(voter_city)
#
## ---------------------------------------------------- SUBSET DATA
#
## full_data <- full_data[1:1000, ]
#
## ---------------------------------------------------- CREATE MAP
#
#lookup <- voter_data[c('dropoff_city', 'vid')]
#dropoff_data$vid <- NULL
#double_data <- merge(lookup, dropoff_data, by = 'dropoff_city')
#double_data <- rbind(full_data[full_data$location != 'dropoff', ], double_data)
#double_data <- double_data %>% arrange(vid, location)
#
#return(list('line_data' = double_data, 'full_data' = full_data))
  
