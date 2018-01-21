##
# Script for finding out movie cast members who appear often together.
# Author: mdoucem
# Last update: 21.01.2018
##

library(dplyr)
library(tidyr)
library(stringr)
library(gridExtra)

# Load in the data 
movie_df<-read.csv("oscar-movies-full-set.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, na.strings = "")

# subset to the first 20 people in the cast list - if you have enough memory space, you can skil this step

m_df1<-subset(movie_df, select = c(1:21))

m_df2<-gather(m_df1, cast, names, -Film) # 1,648,332 by 3 dimension
m_df3<-subset(m_df2, select = c(Film, names))

# do the cross product  
pair_mdf3<-crossprod(table(m_df3)) 

## which actors appear most frequently
total_app<-diag(pair_mdf3)
max_total_app<-sort(total_app[total_app>10], decreasing = T)
temp<-as.data.frame(max_total_app)
colnames(temp)<-c("Freq. Appearance")
View(temp) 
# Save table into pdf
pdf("actorFreq.pdf", height = 20, width = 10)
grid.table(temp)
dev.off()

## which actors have the most appearances with others
# Set the diagnals first to NA
pair_mdf4<-pair_mdf3
diag(pair_mdf4)<-NA

get_sum<-apply(pair_mdf4, 2, sum, na.rm=T)
summary(get_sum) # median is 19, mean is 30, max is a whooping 437
total_sum<-sort(get_sum, decreasing = T)
temp2<-as.data.frame(total_sum)
colnames(temp2)<-c("Total co.appearances")
View(temp2) # Merly streep has worked with the most other actors and actresses, a total of 437 other stars.
# Save table into pdf
pdf("actorCoFreq.pdf", height = 20, width = 10)
grid.table(head(temp2, 10)) # you can change this to save more than the top 10 results
dev.off()


# get the maximum any actor has acted with anyone
get_max<-apply(pair_mdf4, 2, max, na.rm=T) # you might need to subset if an error of a too large vector being generated occurs
pair_max<-get_max[get_max>2] #with 2 appearances together, there is 263 people
temp3<-sort(pair_max, decreasing = T) # get a table with the
temp4<-as.data.frame(temp3)
colnames(temp4)<-c("Total Freq.")
# save 
pdf("sumAppearances.pdf", height= 50, width = 10)
grid.table(head(temp4, 100)) # change to get top 10, 20 or more
dev.off()




## who has woked the most with whom?
pair_mdf5<-as.data.frame(pair_mdf4)
colnames(pair_mdf5)<-colnames(pair_mdf5)

name<-"Bradley Cooper" # set name e.g., Bradley cooper or Merly Streep or Al Pacino etc.
temp5<-pair_mdf5 %>% select(name)

set_num_app = 2
co_app<-subset(temp5, temp4[1]>= set_num_app)
# colnames(co_app)<-c("Freq. of", colnames(co_app), "co-apperances")
temp5<-co_app[order(-co_app[1]), , drop=FALSE]
View(temp5)

# save to pdf
filename<-paste(name,"FreqList.pdf",sep = "")
if (!file.exists(filename)) {
  pdf(filename, height = 20, width = 10)
  grid.table(temp5)
  dev.off()
}
