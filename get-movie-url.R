##
# The script takes a list of movie names, does a google search and retrieves the URLs 
# which contain 'imdb' in it. The goal is to eventually get the imdb IDs of the movies
# Some of the code is taken from this stackoverflow answerhttps://stackoverflow.com/questions/32889136/how-to-get-google-search-results/40181564
##


# Code for getting the IMDb Url for the movies

library(urltools)
library(rvest)
library(curl)

#load data
df<-read.csv("nominated_movies.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
movie_list<-as.character(paste0(df$Film, "(", df$Year, ")"))

# Function for getting website.
getWebsite <- function(name){
  url = URLencode(paste0("https://www.google.com/search?q=",name))
  
  #page <- read_html(url)
  page<-read_html(curl(url, handle = curl::new_handle("useragent"="Chrome")))
  
  results <- page %>% 
    html_nodes("cite") %>% # Get all notes of type cite. You can change this to grab other node types.
    html_text()
  
  result<-results[grep("^....imdb.*", results)]
  
  # Return results if you want to see them all.
  return(result)
  
}

URL<-sapply(movie_list, getWebsite) # depending on the network you are on, you will most likely face a HTTP error 503

# add to dataframe
df$URL<-URL

# Write to CSV
df<-apply(df, 2, as.character)
write.csv(df, file = "MovieSite.csv", sep = ',', row.names = FALSE, append = T) # The resulting data frame will need further preprocessing