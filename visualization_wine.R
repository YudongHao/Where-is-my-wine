library(ggplot2)
library(ggmap)
library(data.table)

setwd("~/Desktop/Challenge _ The Data Incubator_files/project")

# load parsed data
load("wine_data.RData")

# -----------------------------------------------------------------------------
# Compute Quantity
# -----------------------------------------------------------------------------
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


# averaged price group by country
country_avgpx <- calc_avg_group(wdata, "Country", "PriceRetail", 30, TRUE)

# number of wines group by country
country_numwines <- calc_avg_group(wdata, "Country", "Count", 30, FALSE)

# averaged ratings group by country
country_avgrating <- calc_avg_group(wdata, "Country", "RatingHighestScore", 30, TRUE)

# binding wine numbers, average ratings and average price into one data frame with country
By_country <- data.frame(country = country_avgpx$group, avgpx = country_avgpx$quant,
                         numwines = country_numwines$quant, avgrating = country_avgrating$quant)


# Count the number of wine types in the top five countries with highest average wine price

# order the country based on their average prices in descending order
setorder(country_avgpx, -x, quant)

# find the top five countries with highest average prices
all_vy <- country_avgpx[1:5, 1]
avg_px_type_c <- data.frame(country=character(), winetype=character(), count=numeric(), 
                            avg_px=numeric())

# For each country, calculate the average price of each wine type
for (v in all_vy){
        subdata <- wdata[wdata$Country == v, ]
        px_type <- aggregate(subdata$PriceRetail, by=list(subdata$WineType.Name), FUN=mean)
        count_type <- aggregate(subdata$Count, by=list(subdata$WineType.Name), FUN=sum)
        df_new <- cbind(rep(v, nrow(px_type)), count_type, px_type[,2])
        avg_px_type_c <- rbind(avg_px_type_c, df_new)
}
colnames(avg_px_type_c) <- c("Country", "WineType", "Count", "Price")

# remove empty wine type and column Count
avg_px_type_c <- avg_px_type_c[avg_px_type_c$WineType != "", ]
avg_px_type_c$Count <- NULL



# -----------------------------------------------------------------------------
# Visualization
# -----------------------------------------------------------------------------

# plot certain characters(passed in as "fillname") vs country and vineyard distributions on world map
# 
world <- map_data("world")
world <- world[world$region != 'Antarctica',]
world$region[world$region == "USA"] = "United States"
world$region[world$region == "UK"] = "United Kingdom"
world$region[world$region == "Curaçao"] = "Curacao"

# save data for RShiny
save(world, By_country, file="wine_rshiny.RData")

# plot function, fillname can choose from (By_country$avgpx, By_country$numwines and By_country$avgrating)
plot_map <- function(fillname, By_country, world){
        
        m <- ggplot()
        m <- m + geom_map(data = world, map = world, aes(x = long, y = lat, map_id = region),
                          color="black", fill="#fffff4")
        
        m <- m + geom_point(data = wdata, na.rm = TRUE,
                            aes(x = wdata$Longitude, y = wdata$Latitude),
                            color = 'blue', alpha = 0.05, size = 0.4)
        
        m <- m + geom_map(data = By_country, map = world,
                          aes(fill = fillname, 
                              map_id = By_country$country), alpha = 0.5)
        if(fillname == By_country$avgpx){
                m <- m + scale_fill_gradient(name= "Average Price", low="#00ff87", high="#ff0c00")        
        }
        if(fillname == By_country$numwines){
                m <- m + scale_fill_gradient(name= "Wine Numbers", low="#00ff87", high="#ff0c00")        
        }
        if(fillname == By_country$avgrating){
                m <- m + scale_fill_gradient(name= "Average Rating", low="#00ff87", high="#ff0c00")        
        }
        m
}

plot_map(By_country$avgpx, By_country, world)


# Plot a bar chart showing the average price per wine type for the comparison of the top five countries with
# highest average wine price

bar_plot <- ggplot(data = avg_px_type_c, aes(x= Country, y=Price, fill=WineType)) + 
        geom_bar(stat="identity", position = 'dodge', width = 0.5) +
        scale_fill_manual(values = c("#F3B54A", "#348989","#983434", "#FFAAA6", "#6c846f"))

bar_plot


