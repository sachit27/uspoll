#US Polls 2016 analysis with R

This is an effort to analyze US presidential election 2016.
There are 3 R scripts. sentiment.R is to do the sentiment analysis , predictive modelling and finding the accuracy . 
cloud.R is to generate the word cloud for the presidential candidates with sentiment percentage.
wmap.R is to do the geolocation analysis of tweets from users all over the world. It plots the locations of users on the world map.

For twitter authentication, you need to generate your own set of keys.
For datumbox api, you need to generate your own database access key.

The libraries have been specified in the code itself. And they can be easily installed from CRAN.
The sentiment analysis is done using lexicon based approach. There are text files which have negative and positive words to match the words and calculate the sentiments. They need to be downloaded and saved into the main directory before starting the analysis.


