setwd("~/Documents/Data Incubator/project")

library(jsonlite)
library(ggmap)
library(data.table)

# -----------------------------------------------------------------------------
# Parse Data Files
# -----------------------------------------------------------------------------

# a function to parse JSON file, and format data
# file - a path to a JSON file
# return a dataframe with desired information
parse_file_into_data <- function(file){
  # read JSON file
  file_read <- fromJSON(file)
  raw_data <- file_read$Products$List
  
  # get attributes names
  all_attr <- NULL
  all_attr_id <- NULL
  for (i in 1:nrow(raw_data)){
    all_attr <- rbind(all_attr, list(data.frame(raw_data$ProductAttributes[i])$Name))
  }
  
  # get lable url
  all_lable_url <- sapply(1:nrow(raw_data), function(x){unlist(raw_data$Labels[x])[3]})
  
  # create a new dataframe to store desired information
  data <- data.frame(Id = raw_data$Id, 
                     Name = raw_data$Name, 
                     Attr = all_attr, 
                     Appellation.Id = raw_data$Appellation$Id,
                     Appellation.Name = raw_data$Appellation$Name,
                     Appellation.Region.Id = raw_data$Appellation$Id,
                     Lable.Url = all_lable_url,
                     Type = raw_data$Type,
                     Varietal.Id = raw_data$Varietal$Id,
                     Varietal.Name = raw_data$Varietal$Name,
                     WineType.Id = raw_data$Varietal$WineType$Id,
                     WineType.Name = raw_data$Varietal$WineType$Name,
                     Vineyard.Id = raw_data$Vineyard$Id,
                     Vineyard.Name = raw_data$Vineyard$Name,
                     Vineyard.ImageUrl = raw_data$Vineyard$ImageUrl,
                     Vintage = raw_data$Vintage,
                     CommunityHighestScore = raw_data$Community$Reviews$HighestScore,
                     RatingHighestScore = raw_data$Ratings$HighestScore,
                     PriceMax = raw_data$PriceMax,
                     PriceMin = raw_data$PriceMin,
                     PriceRetail = raw_data$PriceRetail,
                     stringsAsFactors = FALSE)
  return(data)
}




# a function to parse all data files
# path - path to data files (JSON)
parse_all_files <- function(path){
  # get a list of all files
  file_list <- list.files(path)
  for (file in file_list){
    # parse file
    tmp_data <- parse_file_into_data(paste(path, file, sep="/"))
    if (!exists("parsed_data")){
      parsed_data <- tmp_data
    }
    else{
      print(file)
      parsed_data <- rbind(parsed_data, tmp_data)
    }
    rm(tmp_data)
  }
  return(parsed_data)
}


# add geographic information to data
add_geo_to_data <- function(data_in){
  # add latitude and longitude for vineyards
  parsed_data <- cbind(data_in, data.frame(Longitude=rep(NA, nrow(data_in)), Latitude=rep(NA, nrow(data_in)), Country=rep(NA,nrow(data_in))))
  
  unique_vineyards <- unique(parsed_data$Vineyard.Name)
  unique_vineyards <- unique_vineyards[!is.na(unique_vineyards)]
  
  # count vineyards
  vy_count <- data.frame(vy=unique_vineyards, count=rep(0, length(unique_vineyards)))
  for (i in 1:length(unique_vineyards)){
    vy_count[i,2] = sum(parsed_data$Vineyard.Name==as.character(vy_count[i,1]), na.rm=TRUE)
  }
  # sort by number of wines
  setorder(vy_count, -count, vy)
  
  quota <- min(as.numeric(geocodeQueryCheck(userType = "free")), nrow(vy_count))
  vy_list <- as.character(vy_count[,1])[1:quota]
  
  # request geo information
  geo_info <- geocode(vy_list, output = "more", override_limit=TRUE, messaging = TRUE)
  # save to a file
  save(geo_info, file="geo_info.RData")
  
  
  # save geo information to data
  for (i in 1:quota){
    index <- which(parsed_data$Vineyard.Name == vy_list[i])
    parsed_data$Longitude[index] <- geo_info[i,]$lon
    parsed_data$Latitude[index] <- geo_info[i,]$lat
    parsed_data$Country[index] <- as.character(geo_info[i,]$country)
  }
  
  return(parsed_data)
}


# parse all data files 
wdata_raw <- parse_all_files("~/Documents/Data Incubator/project/data")

# save to a file
save(wdata_raw, file="wine_data_no_loc.RData")

# get location information
wdata <- add_geo_to_data(wdata_raw)

# add a new column with 1 - used as wine count
count_col <- data.frame(Count=rep(1,nrow(wdata)))
wdata <- cbind(wdata, count_col)

# make corrections of location
wdata$Country[which(wdata$Vineyard.Name=="Dom Perignon")] = "France"
# Rue de l'Abbaye | Eglise Saint-Sindulphe de Abbaye Saint-Pierre d'Hautvillers, 51160 Hautvillers, France 
wdata$Latitude[which(wdata$Vineyard.Name=="Dom Perignon")] = 49.081884
wdata$Longitude[which(wdata$Vineyard.Name=="Dom Perignon")] = 3.941437

wdata$Country[which(wdata$Vineyard.Name=="Dona Paula")] = "Argentina"
# Av. Colón 531, 5500 Mendoza, Argentina
wdata$Latitude[which(wdata$Vineyard.Name=="Dona Paula")] = -32.893456
wdata$Longitude[which(wdata$Vineyard.Name=="Dona Paula")] = -68.847299

wdata$Country[which(wdata$Vineyard.Name=="Gaja")] = "Italy"
# Via Torino, 18, 12050 Barbaresco CN, Italy
wdata$Latitude[which(wdata$Vineyard.Name=="Gaja")] = 44.725942
wdata$Longitude[which(wdata$Vineyard.Name=="Gaja")] = 8.080658

# remove duplicate records
unique_id <- unique(wdata$Id)
row_rm <- c()
for (i in 1:length(unique_id))
{
  id_index <- which(wdata$Id==unique_id[i])
  if (length(id_index) > 1)
  {
    # write down row numbers to remove
    # only keep the first record
    row_rm <- c(row_rm, id_index[-1])
  }
}
wdata <- wdata[-row_rm, ]

# save file for later use
save(wdata, file="wine_data.RData")



# -----------------------------------------------------------------------------
# Compute Quantity
# -----------------------------------------------------------------------------

# load data file
load("wine_data.RData")

# a function to compute average/aggregated col_quant and group by col_grp
# wine_data - parsed wine data
# col_grp - column name for group
# col_quant - column name for the quantity we want to compute
# limit - only include groups with more than this number of wines
# is_avg - TRUE: compute average; FALSE: compute sum
# return a dataframe with [group, quant]
calc_avg_group <- function(wine_data, col_grp, col_quant, limit = 10, is_avg = TRUE){
  # get unique groups
  all_grp_raw <- unique(wine_data[[col_grp]])
  all_grp <- all_grp_raw[!is.na(all_grp_raw)]
  # count number of wines within each group
  count <- sapply(all_grp, function(x){sum(wine_data$Count[wine_data[[col_grp]]==x], na.rm=TRUE)})
  grp_count <- data.frame(group=all_grp, count=count, row.names = c(1:length(all_grp)))
  # only include group whose wine number exceeds limit
  valid_grp <- as.character(grp_count$group[which(grp_count$count > limit)])
  
  # compute the quantity group by col_grp
  calc_func <- if (is_avg) get("mean") else get("sum")
  quantity <- sapply(valid_grp, function(x){calc_func(wine_data[[col_quant]][wine_data[[col_grp]]==x], na.rm=TRUE)})
  res <- data.frame(group = valid_grp, quant = quantity, row.names = c(1:length(valid_grp)))
  
  return(res)
}


calc_avg_group <- function(wine_data, col_grp, col_quant, limit = 10, is_avg = TRUE){
  # get unique groups
  all_grp_raw <- unique(wine_data[[col_grp]])
  all_grp <- all_grp_raw[!is.na(all_grp_raw)]
  # count number of wines within each group
  count <- sapply(all_grp, function(x){sum(wine_data$Count[wine_data[[col_grp]]==x], na.rm=TRUE)})
  grp_count <- data.frame(group=all_grp, count=count, row.names = c(1:length(all_grp)))
  # only include group whose wine number exceeds limit
  valid_grp <- as.character(grp_count$group[which(grp_count$count > limit)])
  
  # compute the quantity group by col_grp
  calc_func <- if (is_avg) get("mean") else get("sum")
  quantity <- sapply(valid_grp, function(x){calc_func(wine_data[[col_quant]][wine_data[[col_grp]]==x], na.rm=TRUE)})
  res <- data.frame(group = valid_grp, quant = quantity, row.names = c(1:length(valid_grp)))
  
  return(res)
}




# averaged price group by vineyard
vineyard_avgpx <- calc_avg_group(wdata, "Vineyard.Name", "PriceRetail", 10, TRUE)

# number of wines group by vineyard
vineyard_numwines <- calc_avg_group(wdata, "Vineyard.Name", "Count", 10, FALSE)

# averaged price group by country
country_avgpx <- calc_avg_group(wdata, "Country", "PriceRetail", 30, TRUE)

# number of wines group by country
country_numwines <- calc_avg_group(wdata, "Country", "Count", 0, FALSE)



# compute averaged price group by winetype for each vineyard
vy_count <- aggregate(wdata$Count, by=list(wdata$Vineyard.Name), FUN=sum)
setorder(vy_count, -x, Group.1)
all_vy <- vy_count[1:9, 1]
avg_px_type_vy <- data.frame(vinyard=character(), winetype=character(), count=numeric(), avg_px=numeric())
for (v in all_vy){
  subdata <- wdata[wdata$Vineyard.Name == v, ]
  px_type <- aggregate(subdata$PriceRetail, by=list(subdata$WineType.Name), FUN=mean)
  count_type <- aggregate(subdata$Count, by=list(subdata$WineType.Name), FUN=sum)
  df_new <- cbind(rep(v, nrow(px_type)), count_type, px_type[,2])
  avg_px_type_vy <- rbind(avg_px_type_vy, df_new)
}
colnames(avg_px_type_vy) <- c("Vineyard", "WineType", "Count", "Price")

save(avg_px_type_vy, file="avg_px_type_vy.RData")




