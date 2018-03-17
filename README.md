# Where-is-my-wine 
A wine recommendation system

http://where-is-my-wine.herokuapp.com/

## Overview

I would like to propose a wine recommendation system called “Where is my wine.” The reason why I’m interested in such a system is that as a wine lover, it is really difficult for me to choose a wine when I’m facing tons of various wines lying in the shelves of a liquor store. I would really appreciate it if there is a platform that can provide me some wine recommendations based on my personal preference.

## Visualization

With this purpose in mind, I first retrieved the complete product catalog from the API of Wine.com. This dataset contains professional wine information of prices, ratings, labels, appellation info, vineyard info and wine types. In order to better understand the dataset, I first investigated several interesting features such as average wine price per country, average wine rating per country, total wine numbers per country by plotting onto a world map.

From the average price per country world map, I can see France has the most expensive wines. As we know Champagne is very expensive and only produced in France. In order to see if Champagne is the main reason for the high average price in France, I also plotted a bar chart regarding the average price per wine type and compare France with other four countries with high average wine price. The result shows that France has the highest average price for every wine type we investigated.

## Recommend System

In this part, I’d like to build a content-based wine recommendation system based on modified K means clustering for mixed data types (including numeric, categorical and logical data).

After some research, I learned that there are two major types of recommendation algorithms: content based and collaborative based systems. Since it’s difficult to obtain other users’ reviews on wines, I chose the content-based method. This method will require the user to rate a few wines beforehand. 

My approach is the following.
1.	Cluster the wines. The wine data has many attributes that can be used for clustering, including categorical data (e.g. Vineyard, Type, Region), numeric data (price, score) and logical data (e.g. Smooth & Supple, Big & Bold, Earthy & Spicy). To use all of these features, the conventional K means clustering cannot be used because of the mixed data type. After some research, I learned that the Gower distance can be used to measure the dissimilarities for mixed data types. Once the dissimilarities of observations can be measured, a clustering algorithm, such as partitioning around medoids (PAM), can be used to cluster the data. I will also need to optimize the number of clusters by comparing the metrics such as silhouette width. 
2.	Determine the clusters that user likes the most. Assign clustering number to user rated wines, group user’s wines by cluster number, and compute the averaged rating for each cluster. Based on averaged rating, choose the top clusters.
3.	Recommend top wines within these clusters, ranked by a mixture of score, price and/or other features. 
Build a GUI for this wine recommendation system.

## Reference

1. https://muffynomster.wordpress.com/2015/06/07/building-a-movie-recommendation-engine-with-r/
2. https://rpubs.com/tim-dim/recommender
3. mixed clustering https://www.r-bloggers.com/clustering-mixed-data-types-in-r/
