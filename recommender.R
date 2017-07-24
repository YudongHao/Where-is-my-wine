# Content based filtering to make recommendation of wines
# Based on modified version of K means clustering for mixed types of data (categorical + logical + numeric)


setwd("~/Documents/Data Incubator/project")
library('cluster')

#------------------------------------------------------------------------------------
# Create Profile Matrix
#------------------------------------------------------------------------------------

# load data file
load("wine_data.RData")

# need to reduce data size to accommodate daisy function
# include wines with high score and ratings
wdata_red <- wdata[which(wdata$CommunityHighestScore > 9 & wdata$RatingHighestScore > 90), ]


# parse attribute
all_attr <- unlist(wdata_red$Attr)
unique_attr <- unique(all_attr)

# create a profile matrix with columns as features for all wines
prof_matrix <- data.frame(matrix(rep(0, length(unique_attr) * nrow(wdata_red)), nrow=nrow(wdata_red)))

# fill in attributes
for (i in 1:nrow(wdata_red)){
  row_indx <- unique_attr %in% unlist(wdata_red[i,]$Attr)
  prof_matrix[i, row_indx] = 1
}

# add other features
prof_matrix <- cbind(wdata_red$Id, 
                     wdata_red$Appellation.Id, 
                     wdata_red$Varietal.Id, 
                     wdata_red$WineType.Id,
                     wdata_red$Vineyard.Id,
                     wdata_red$Vintage,
                     wdata_red$CommunityHighestScore * 0.5 + wdata_red$RatingHighestScore/10 * 0.5, # create a new rating/score
                     wdata_red$PriceRetail,
                     prof_matrix)
colnames(prof_matrix) <- c("Id", "Region", "Varietal", "Type", "Vineyard", "Vintage", "Score", "Price", unique_attr)

#------------------------------------------------------------------------------------
# Content Based Recommendation
# 
# The recommender is based on all of the following variables
#   1) logical (Smooth&Supple, Earthy&Spicy, Light&Chrisp, etc.)
#   2) categorical (Vineyard, Type, Region, etc.) 
#   3) numeric (Scores, Prices, etc.)
# Gower distance is used to compute the distance
#
# To enhance performance, first cluster the wines.
#------------------------------------------------------------------------------------

####################################
# 1. A modified K means cluster with mixed data type
#         using Gower distance
###################################

# convert class of columns - numeric for numeric variables, factor for nominal variabls
# logical variables will be treated as asymmetric binary "asymm"
prof_matrix[["Region"]] <- factor(prof_matrix[["Region"]])
prof_matrix[["Varietal"]] <- factor(prof_matrix[["Varietal"]])
prof_matrix[["Type"]] <- factor(prof_matrix[["Type"]])
prof_matrix[["Vineyard"]] <- factor(prof_matrix[["Vineyard"]])
prof_matrix[["Vintage"]] <- factor(prof_matrix[["Vintage"]])

# remove wine id before clustering
prof_clean <- prof_matrix[, -1]

# define weight vector for variables
weight <- c(6,3,8,1,1,2,10,rep(1,40))

# compute the distance matrix
gower_dist <- daisy(prof_clean, metric="gower", weights=weight, type=list(asymm=c(8:ncol(prof_clean))))

# sanity check
summary(gower_dist)


####################################
# 2. Clustering
#         using partitioning around medoids (PAM) 
#         silhouette width as validation method
###################################
# Calculate silhouette width for many k using PAM
# optimize cluster numbers based on silhouette width
opt_cluster_number <- function(gower_dist, k_rng){
  sil_width <- rep(0, length(k_rng))
  for(i in 1:length(k_rng)){
    # PAM for clustering
    print(paste("Computing clustering with k =", k_rng[i]))
    pam_fit <- pam(gower_dist, diss = TRUE, k = k_rng[i])
    sil_width[i] <- pam_fit$silinfo$avg.width
  }
  plot(k_rng, sil_width)
}

opt_cluster_number(gower_dist, seq(60, 70, 1)) # tried from larger range to smaller range

# the optimized number of clusters
opt_cluster = 62

# assign cluster number to corresponding wines
pam_fit <- pam(gower_dist, diss = TRUE, k = opt_cluster)
wine_cluster <- data.frame(Id = prof_matrix$Id, Cluster=pam_fit$clustering)

# save to file
save(wine_cluster, gower_dist, pam_fit, opt_cluster, weight, prof_matrix, file="wine_cluster.RData")

####################################
# 3. Get and compute the user related information
###################################
#load("wine_cluster.RData"))

# a function to set the wines that user rated
# user_rate_size - the number of wine that user rated
set_user_profile <- function(user_rate_size, prof_matrix, wine_cluster){
  set.seed(1988) # for reproducibility
  # assume user rated (0-10) the following wines 
  rated_id <- sample(prof_matrix$Id, user_rate_size)
  set.seed(1234567)
  ratings <- runif(user_rate_size, min = 0, max = 10)
  user_prof <- data.frame(Id=rated_id, Rating=ratings, Price=rep(0, length(ratings)), Cluster=rep(0, length(ratings)))
  # assign cluster numbers to the wines that user had
  user_prof$Cluster <- wine_cluster$Cluster[match(user_prof$Id, wine_cluster$Id)]
  for (i in 1:length(rated_id)){
    user_prof$Price[i] = prof_matrix$Price[prof_matrix$Id == rated_id[i]]
  }
  return(user_prof)
}

# a function to compute the cluster that user likes most
# num_cluster - number of clusters to return
get_user_prefer_cluster <- function(user_prof, num_cluster = 2){
  # calculate for each user's cluster the average of user ratings
  avg_user_rating <- aggregate(user_prof$Rating, by=list(Cluster=user_prof$Cluster), mean)
  colnames(avg_user_rating) <- c("Cluster", "Rating")
  # select the top clusters with highest rating
  setorder(avg_user_rating, -Rating, Cluster)
  top_cluster <- avg_user_rating$Cluster[1:num_cluster]
  return(top_cluster)
}

####################################
# 4. Make recommendation
###################################
# Within the cluster that the user likes, recommend the wines that the user never had with top scores
# Params: user_rate_size - number of ratings that the user made
#         rec_size - number of recommendataions to make
#         prof_matrix - profile matrix
#         wine_cluster - pre-computed cluster information for all wines
#         wdata - all wine data
make_recommendation <- function(user_rate_size, rec_size, prof_matrix, wine_cluster, wdata){
  # get user rating
  user_prof <- set_user_profile(user_rate_size, prof_matrix, wine_cluster)
  
  # compute the cluster that user likes most
  top_cluster <- get_user_prefer_cluster(user_prof)
  # within the given cluster, select top scored wines
  sub_wine <- prof_matrix[(wine_cluster$Cluster %in% top_cluster) & !(wine_cluster$Id %in% user_prof$Id), ]
  # create a price weighted score based on user's preference
  # use original score tend to recommend wines with very high prices
  # because original score has very high correlation with price
  user_avg_price <- sum(user_prof$Rating * user_prof$Price) / sum(user_prof$Rating) # compute user rating weighted price as base price
  sub_wine_s <- cbind(sub_wine, sub_wine$Score - 0.0001 * (sub_wine$Price - user_avg_price)^2) # penalize large price difference
  colnames(sub_wine_s) <- c(colnames(sub_wine), "WScore")
  # order by weighted score
  setorder(sub_wine_s, -WScore, Score, Varietal, Type, Vineyard)
  rcmd_id <- sub_wine_s$Id[1:rec_size]
  
  # user's rating
  user_index <- which(wdata$Id %in% user_prof$Id)
  user_data <- cbind(wdata[user_index, c("Name", "Vineyard.Name", "Varietal.Name", "PriceRetail", "WineType.Name", "Appellation.Name")], round(user_prof$Rating, digits = 1))
  colnames(user_data) <- c("Name", "Vineyard", "Varietal", "Price", "WineType", "Appellation", "MyRating")
  rownames(user_data) <- NULL
  # recommendations
  rec_index <- which(wdata$Id %in% rcmd_id)
  rec_data <- wdata[rec_index, c("Name", "Vineyard.Name", "Varietal.Name", "CommunityHighestScore", "RatingHighestScore", "PriceRetail", "WineType.Name", "Appellation.Name", "Lable.Url")]
  colnames(rec_data) <- c("Name", "Vineyard", "Varietal", "CommunityScore", "RatingScore", "PriceRetail", "WineType", "Appellation", "Lable")
  rownames(rec_data) <- NULL
  
  save(user_data, rec_data, file="RecResult.RData")
}

# main function
make_recommendation(user_rate_size, rec_size, prof_matrix, wine_cluster, wdata)



