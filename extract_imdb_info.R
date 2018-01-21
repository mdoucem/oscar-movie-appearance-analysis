##
# The script takes in a list of movies with their imdb ids
# and retrieves movie information from IMDb.com
# The code is based on the one given in the blog: http://www.theswarmlab.com/blog/rvspython/r/2014/02/02/r-vs-python-round-2-2/
##


IMDb<-function(ID) {
  
  # Function takes as input the ID of the movie
  
  # Returns:
  # A data frame containing one line per movie 
  # and columns as following:
  # movie ID, film title, year of nomination, full cast, director(s), and producer(s)
  
  # Load required libraries
  require(XML)
  require(pbapply)
  require(httr)


  # Wrap core of the function in do.call and pblapply in order to
  # pseudo-vectorize it (pblapply) and return a data frame (do.call)
  
  info<-do.call(rbind, pblapply(ID, FUN = function(ID) {
    
    # Create  movie URL on IMDb.com
    URL<-paste0("http://www.imdb.com/title/tt", ID)
    
    # Download and parse HTML of IMDb page
    parsed.html<-htmlParse(URL)
    
    # Find genre
    Genre<-paste(xpathSApply(parsed.html, 
                             "//span[@class='itemprop' and @itemprop='genre']", xmlValue), collapse='|')
    
    # Find director
    Director<-paste(xpathSApply(parsed.html, 
                                "//div[@itemprop='director']/a", xmlValue), collapse='|')
    

    # Extract full cast from the full credits page
    parsed.html<-htmlParse(paste0(URL, "/fullcredits"))
    Full_Cast<-paste(xpathSApply(parsed.html, "//span[@itemprop='name']", xmlValue), collapse='|')
    
   
    data.frame(ID = ID, Genre = Genre, Full_Cast = Full_Cast, Director = Director)
  }
  
    ))
}

# Load the data containing the movie set
data<-read.csv("oscar-movies.csv", header = TRUE, sep=";", stringsAsFactors = FALSE)

# For each movie, extract IMDb info and append it to dataset
data<-within(data, {
  # Extract ID number
  IMDB_ID<-gsub("[^0-9]", "", URL)
  
  ## Now that this is done, we let the IMDb scraper collect the data 
  ## we append it to the data
  
  # Download IMDb info into a temporary variable
  IMDB_Info<-IMDb(IMDB_ID)

  # Save genre(s)
  Genre<-IMDB_Info$Genre

  # Save full cast
  Full_Cast <- IMDB_Info$Full_Cast #Could think of splitting this list into separate columns
  
  # Save director(s)
  Director <- IMDB_Info$Director
  
  # Delete IMDb info
  IMDB_Info <- NULL
  
  })

# Save to a dataframe if desired
saveRDS(data, file="myMovieData.rds")

# or save to csv
write.csv(data, file ="movies-full-set.csv")