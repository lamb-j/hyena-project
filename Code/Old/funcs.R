#----------------------------------
#Functions used to search for Hyenas in tblSessions.csv
source("wd.R")

setwd(Raw_Data)
tblS   <- read.csv("tblSessions.csv", stringsAsFactors = FALSE)
setwd(Formatted_Data)
tblRFA <- read.csv("tblRFA.csv", stringsAsFactors = FALSE)

ID_to_Vec <- function(i){
   x <- tblS[i,2]
   x <- unlist(strsplit(x, split=" "))
   return (x)
}

#Searches the observations for " ID "
print_obsv1 <- function(ID){
 
  for(i in 1:nrow(tblS)){
    obV <- ID_to_Vec(i)
    
    if(ID%in%obV) print(tblS[i,])
  }
}
    
#Searches the observations for "ID"
print_obsv2 <- function(ID){
  
  for(i in 1:nrow(tblS)){
    if(tblS[i,2] == "") next
    if(grepl(tblS[i,2], ID)) print(tblS[i,])
  }
}

seen <- function(ID, i){
   x <- ID_to_Vec(i)
   if(ID%in%x) return(1)
   return(0)
}
    


#Creates a vector never_seen of all the Hyenas that are in no observations
no_obsV <- function(){
  never_seen <- vector()

  for(i in 1:nrow(tblRFA)){
    print(i)
    flag = 0
    ID <- tblRFA[i,1]
    for(j in 1:nrow(tblS)){
       if(seen(ID, j)) {
         flag = 1
         break
       }
    }
    if(flag == 0){
       never_seen <- c(never_seen, ID)
       print(ID)
    }
  }
  return(never_seen)
}
    
#--------------------------------------
# Function for finding hyenas in tblRFA    

find_ID <- function(ID){
  for(i in 1:nrow(tblRFA)){
    if(tblRFA[i,1] == ID) print(i)
  }
}

setwd(Code) 
