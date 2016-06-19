#This code is used to find the geo location of the tweets addressed towards a particular person.
#The tweets are collected, gathered into a data frame and then the location of the users is plotted on the map using the library(ggmap) and library(ggplot2).

library(twitteR)
library(dismo)
library(maps)
library(ggplot2)
library(ggmap)

#Steps to be followed for twitter authentication
#the keys need to be changed as this data is confidential
consumer_key <- '*****************' 
consumer_secret <- '*******************'
access_token <- '*********************************'
access_secret <- '***********************************'
setup_twitter_oauth(consumer_key , consumer_secret, access_token, access_secret)


term <- ("donald + trump") #the term you need to find(it can be a name,#, @)
get_tweets <- searchTwitter(term, n = 1500)  # number of tweets needed to be collected
df_tw <- twListToDF(get_tweets)  # data frame generation

users <- lookupUsers(df_tw$screenName)  # get user's information
df_users <- twListToDF(users)  #data frame generation

users_locations <- !is.na(df_users$location)  # user's with clear location information
locations <- geocode(df_users$location[users_locations])
with(locations, plot(lon, lat)) #location estimation

worldMap <- map_data("world")  #generating world map
loc_pts <- ggplot(worldMap) 
loc_pts <- loc_pts + geom_path(aes(x = long, y = lat, group = group), 
                       colour = gray(2/3), lwd = 1/3)
loc_pts <- loc_pts + geom_point(data = locations,  # Points depicting users in red color
                        aes(x = lon, y = lat),
                        colour = "RED", alpha = 1/2, size = 1)
loc_pts <- loc_pts + coord_equal() 
loc_pts <- loc_pts + theme_minimal()
print(loc_pts)
