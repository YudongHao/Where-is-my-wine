library(data.table)

setwd("~/Desktop/Challenge _ The Data Incubator_files/project")

# load parsed data
load("wine_data.RData")

## Compute Quantity

# a function to compute average/aggregated col_quant and group by col_grp
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