

voter_to_census <- read.csv("./data/voter_to_census.csv")
voter <- read.csv("./data/voter_data_v3.csv")
census <- read.csv("./data/census_aggregate.csv")

total <- merge(x = voter_to_census, y = voter, by = "X", all.x = TRUE)
summary(total)

t <- na.omit(total)


#   ##############
#   # SET UP MAP #
#   ##############
#   
#   # set the API key
#   APIkey <- '1d48054ea6b0766bdc867ec19a4295b437b0a59e'
#   api.key.install(key = APIkey)
#   
#   # get the spatial data - call to tigris - makes the census shapes
#   tracts <- tracts(state = 'CA', county = 037, cb = TRUE)
#   
#   # get the tabular data
#   geo <- geo.make(state = 'CA', county = 037, tract = '*')
#   
#   # there is limited census api availability. Have not found education information yet
#   income <- acs.fetch(endyear = 2012, table.number = 'B19001', geography = geo,
#                       col.names = 'pretty')
#   
#   
#   # convert to a dataframe
#   income_df <- data.frame(paste0(str_pad(income@geography$state, 2, 'left', pad = '0'),
#                                  str_pad(income@geography$county, 3, 'left', pad = '0'), 
#                                  str_pad(income@geography$tract, 6, 'left', pad = '0')), 
#                           income@estimate[,c('Household Income: Total:', 
#                                              'Household Income: $200,000 or more')],
#                           stringsAsFactors = FALSE)
#   
#   income_df <- dplyr::select(income_df, 1:3)
#   rownames(income_df) <- 1:nrow(income_df)
#   names(income_df) <- c('GEOID', 'total', 'over_200')
#   income_df$percent <- 100 * (income_df$over_200/income_df$total)
#   
#   # merge the spatial and tabular data
#   income_merged <- geo_join(tracts, income_df, "GEOID", "GEOID")
#   income_merged <- income_merged[income_merged$ALAND>0, ]


####################
# DEFINE FUNCTIONS #
####################

place_points <- function(m, d, group_name) {
  # function to add points onto a voter map
  # sets voters and dropoff locations as distict 
  # seperated by city
  v <- d[d$location == 'voter',]
  m = addCircles(map = m, 
                 lng = v$long, lat = v$lat, 
                 fillColor = v$cols, 
                 color = v$cols, 
                 radius = v$size, 
                 weight = 1,
                 group = group_name)
  # clusterOptions = markerClusterOptions()) - SUPER SLOW
  # add dropoff stuff
  m = addCircles(map = m, 
                 lng = d$long, lat = d$lat, 
                 popup = d$description,
                 fillColor = d$cols, 
                 color = d$col, 
                 radius = d$size, 
                 weight = 1,
                 group = group_name, 
                 fillOpacity = .65)
  # this creates clusters, but it is slow and not what I want
  return(m)
  
}

makePairs <- function(data) 
{
  grid <- expand.grid(x = 1:ncol(data), y = 1:ncol(data))
  grid <- subset(grid, x != y)
  all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
    xcol <- grid[i, "x"]
    ycol <- grid[i, "y"]
    data.frame(xvar = names(data)[ycol], yvar = names(data)[xcol], 
               x = data[, xcol], y = data[, ycol], data)
  }))
  all$xvar <- factor(all$xvar, levels = names(data))
  all$yvar <- factor(all$yvar, levels = names(data))
  densities <- do.call("rbind", lapply(1:ncol(data), function(i) {
    data.frame(xvar = names(data)[i], yvar = names(data)[i], x = data[, i])
  }))
  list(all=all, densities=densities)
}


create_coordinate <- function(variable, df) {
  
  return(list(range = c(min(df[variable]), max(df[variable])),
              visible = TRUE, 
              colorscale = 'Hot',
              label = variable, # add lookup here
              values = as.vector(unlist(df[variable]))))
}

create_dimension <- function(df, variable_vector) {
  dimension <- lapply(variable_vector, create_coordinate, df)
  return(dimension)
}

# pass sample size 
# pass in columns to plot
create_paracoord <- function(data_, coords_, sampleSize_) {
  para_data <- paraCoordData_[sample(c(1:nrow( data_ )), sampleSize_ ),]
  
  dimensions <- create_dimension(para_data, coords_ )
  
  plop <- plot_ly(type = 'parcoords', dimensions = dimensions)
  return(plop)
}


my_k <- function(x, k) {
  kmeans(x, k, nstart = 10, iter.max = 20, algorithm="Hartigan-Wong")
}

get_wss <- function(x, d) {
  sum(my_k(x, d)$withinss)
}


wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i, iter.max = 30)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

#---------------------------------------------------------------------------------------------------------#

#####################
# CLEAN CENSUS DATA #
#####################

c <- census %>% dplyr::select(-X, -status, -house_number, -street, -StreetType, 
                              -city, -zip, -birth_date, -gender, 
                              -language, -pid)

nrow(c)
# Have to normalize Total Count, Male Count, Female Count, Median Earnings, Mean Earnings
# Everything else is in percents
c <- c %>% dplyr::select(-vid) %>% group_by(ctract) %>% summarise_all(max)
nrow(c)
ID <- c$ctract
c$ctract <- NULL

census <- census[,-c(1,2,14)] # remove 'X.1', 'ctract', 'pid'

# remove some unecessary values
colnames(c) # get rid of Mean Income, Commute..Other, Workers..Over.16, Employed, Labor.Force.Participation
c <- c %>% dplyr::select(-Commute..Other, -Workers.Over.16, -Employed, -Labor.Force.Participation)
c <- c %>% mutate(Male = Male / Total) %>% dplyr::select(-Female, -Income..Mean)
colnames(c)

census <- census[(complete.cases(census)),]
c <- c[(complete.cases(c)),]

#------------------------------------------------------------------------------------------------#

##############################
#CREATE CLUSTERS FOR VISUALS #
##############################


# Not needed - estimates the number of clusters (we have determined that it is 12)
# clusters <- c(1:12)
# wss <- unlist(lapply(clusters, get_wss, x = scale(c)))
# gap <- clusGap(x = scale(c), d.power = 2, FUN = my_k, K.max = 20)
# gap_stat <- gap$Tab[,3]

# create clusters
c$cluster_12 <- my_k(scale(c), 12)$cluster

size_12 <- c %>% group_by(cluster_12) %>% dplyr::summarize(cluster_size = n())#summarize(cluster_size = dplyr::n())
cluster_12 <- c %>% group_by(cluster_12) %>% summarize_all(mean) # summarize the clusters to one line 
cluster_12$size <- size_12$cluster_size
cluster_12 <- cluster_12 %>% arrange(desc(size))

e <- data.frame(cluster_12)
e$Tf <- NULL
e$Female <- NULL
e$cluster <- NULL
e$cluster_5 <- NULL
e$size <- NULL


# subset the columns of e to those in cluster12
CN <- colnames(e)
CN <- CN[!(CN %in% c('cluster_12'))]
e[CN] <- round(e[CN], 2)
CN <- c('Income..Mean', 'Income..Median', 'Total')
#e[CN] <- round(e[CN], 0)


factor_names <- names(sapply(census, is.factor))[sapply(census, is.factor)]
numeric_names <- names(sapply(census, is.numeric))[sapply(census, is.numeric)]


# We only need latitude and longitude for voters and dropoff
voter <- voter %>% dplyr::select(vid, voter_lat, voter_long, dropoff_lat, dropoff_long, dropoff_city)

# check on this and likely drop...thought I handled these
voter[!complete.cases(voter),]
voter <- voter[complete.cases(voter),]

# for each latitude and longitude we need information (size, color, etc.) - long format
voter_data <- voter %>% 
  mutate(voter_coordinates = paste(voter_long,  voter_lat),
         dropoff_coordinates = paste(dropoff_long, dropoff_lat)) %>%
  dplyr::select(-voter_long, -voter_lat, -dropoff_long, -dropoff_lat) %>%
  gather(location, coordinates, -vid, -dropoff_city) %>%
  mutate(location = gsub("_coordinates", "", location))

head(voter_data)

voter_data$long <- unlist(lapply(voter_data$coordinates, function(x) as.numeric(unlist(strsplit(x, ' '))[[1]])))
voter_data$lat <- unlist(lapply(voter_data$coordinates, function(x) as.numeric(unlist(strsplit(x, ' '))[[2]])))

sum(duplicated(voter_data)) # no duplications

# -----------------------------------------------------------------------------------------------------#

############################
# ADD GRAPHING INFORMATION #
############################

tol11qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", 
                   "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")
colnames(voter_data)

# Create dropoff location summary data - count, color, and shape
dropoff_data <- voter_data %>% dplyr::filter(location == 'dropoff') %>% 
  dplyr::group_by(dropoff_city) %>% 
  dplyr::summarise_all(max)
voter_data <- voter_data[voter_data$location == 'voter', ]
voter_data <- rbind(voter_data, dropoff_data)

dropoff_information <- voter_data %>% dplyr::filter(location == 'voter') %>%
  dplyr::group_by(dropoff_city) %>% 
  dplyr::summarise(size = 100000 * n()/length(unique(voter_data$vid)), voters = n()) %>% 
  arrange(desc(size)) %>%
  mutate(cols = rep(tol11qualitative, 7), shapes = rep(c(11:17), 11), 
         description = paste(sep = "<br/>",  # for popup labels - responds to HTML code
                             paste0("<b>", dropoff_city, "</b>"),
                             paste0(voters, ' voters'))) %>%
  dplyr::select(-voters)

# Merge voting information values to voter data
voter_data <- merge(voter_data, dropoff_information, by = 'dropoff_city', all.x = TRUE)
voter_data$size[voter_data$location == 'voter'] <- 5 # set the vote size
voter_data <- arrange(voter_data, dropoff_city)


# -----------------------------------------------------------------------------------------------------#

#############################
# CONTROL THE GRAPHING DATA #
#############################

# subset
voter_data <- voter_data %>% dplyr::filter( location == 'dropoff')