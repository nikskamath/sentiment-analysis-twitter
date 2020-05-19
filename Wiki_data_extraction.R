library(htmltab)
library(stringr)
library(lettercase)
library(tm)
setwd()
url <- "https://en.wikipedia.org/wiki/List_of_video_game_publishers"
pub_loc <- htmltab(doc=url, which=2)
pub_loc
head(pub_loc)

pub_loc$Headquarters <- gsub(".*,","",pub_loc$Headquarters)
pub_loc$Publisher<- gsub(" ","_",trimws(pub_loc$Publisher,"b"))
pub_loc$Headquarters<- gsub(" ","_",trimws(pub_loc$Headquarters,"b"))

pub_loc1 <- pub_loc[,-c(3:5)]
write.csv(pub_loc1, "C:/Users/Nikhil Kamath/Documents/NCI/Semester 1/Data Warehousing and Business Intelligence/DWBI Project/pub_loc.csv",row.names = TRUE)
