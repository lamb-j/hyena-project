#------------------------------------------
#Code used to format Raw Data Files
#tblRFA.RData
#tblA.RData
source("wd.R")
setwd(Formatted_Data)

#Function for determining if a hyena is an adult
isAdult <- function(x, date){
  adult_date <- as.Date(tblRFA[x,8],"%Y-%m-%d")
  current_date <- as.Date(date,"%m/%d/%y")
  
  if(is.na(adult_date) || is.na(adult_date)) return(FALSE)
  
  # If the adult date is before the current date
  if(current_date >= adult_date) return(TRUE)
  
  return(FALSE)
}

setwd(Formatted_Data)
save(isAdult, file = "isAdult.RData")

#---------------------------------------
#Code used to format tblRFA from tblHyenas
setwd(Raw_Data)
tblRF  <- read.csv('tblHyenas.csv', stringsAsFactors = FALSE)
tblS   <- read.csv("tblSessions.csv", stringsAsFactors = FALSE)

#Fix some names
tblRF[1,1] <- "02"
tblRF[2,1] <- "03"
tblRF[42,13] <- "02"
tblRF[505,13] <- "02"
tblRF[547,13] <- "02"
tblRF[841,13] <- "02"
tblRF[1101,13] <- "02"
tblRF[2100,13] <- "02"

#Remove some old/unobserved Hyenas
tblRF <- tblRF[-1441,]  #ID:mc1
tblRF <- tblRF[-1427,]  #ID:mart
tblRF <- tblRF[-14,]    #ID:44

#Reduce to resident females
tblRF  <- subset(tblRF, (Sex == 'f'))
tblRF  <- subset(tblRF, ((Status == 'resident') & (Clan == 'talek')))

#Remove redundant/uneeded columns
tblRF$Sex      <- NULL
tblRF$AgeClass <- NULL
tblRF$Status   <- NULL
tblRF$Clan     <- NULL
tblRF$eartag   <- NULL
tblRF$park     <- NULL
tblRF$Fate     <- NULL
tblRF$Name     <- NULL
tblRF$sampleID <- NULL
tblRF$Litrank  <- NULL
tblRF$PrevID   <- NULL
tblRF$miscNotes<- NULL
tblRF$DenGrad  <- NULL
tblRF$Weaned   <- NULL
tblRF$LeaveDen <- NULL
tblRF$ArrivedDen        <- NULL
tblRF$NumberLittermates <- NULL
tblRF$MortalitySource   <- NULL
#Remaning columns: ID, lastUpdated FirstSeen, Disappeared, Mom, Birthdate, DeathDate 

#------------------------
#Removes any indivuduals in tblRF who were never observed in the target time period

#deletes obsvs before 1/1/1989 after 12/31/2009
tblS <- tblS[-c(1:819),]
tblS <- head(tblS, 80291)

ID_to_Vec <- function(i){
   x <- tblS[i,2]
   x <- unlist(strsplit(x, split=" "))
   return (x)
}

#creates a vector of all the observed hyenas
obsv <- vector()
for(i in 1:nrow(tblS)){
 print(i)
 obsv <- c(obsv, ID_to_Vec(i))
 #removes duplicates
 obsv <- unique(obsv)
}

hyenas <- vector()
#runs through the hyenas and sees if they are in the vector
for(i in 1:nrow(tblRF)){
  print(i)
  if(tblRF[i,1]%in%obsv) hyenas <- c(hyenas, tblRF[i,1])
}

tblRF <- subset(tblRF, ID%in%hyenas)


#------------------------------------
#Code used to get adult dates for all the hyenas

# read the formatted data into a table
rowsRF <- nrow(tblRF)
# get the number of rows (iterations)
tblRFA <- tblRF
# copy the data frame
tblRFA["Adult Date"] <- ""
# create an empty column for the dates at which the hyenas became adults

for (i in 1:rowsRF)
  # for all hyenas
{
  if (tblRFA[i,6] != "")
    # check if there is a given birthdate value
  {
    birthDate <- as.Date(tblRFA[i,6], "%m/%d/%y")
    # convert to a date object
    maxAdultDate <- seq.Date(birthDate, by = "36 months", length = 2)
    # create a vector object, with the date and the date plus 36 months
    tblRFA[i,8] <- format(maxAdultDate[2])
    # set the Adult Date to the date of the hyena's 3rd birthday as a maximum
  }
  else
  {
    if (tblRFA[i,3] != "")
      # check if there is a given date for their first sighting
    {
      firstSeenDate <- as.Date(tblRFA[i,3], "%m/%d/%y")
      # convert to a date object
      maxAdultDate <- seq.Date(firstSeenDate, by = "36 months", length = 2)
      # create a vector object, with the date and the date plus 36 months
      tblRFA[i,8] <- format(maxAdultDate[2])
      # set the Adult Date to 36 months after the first sighting as a maximum
    }
  }
  for (j in 1:rowsRF)
  {
    if (format(tblRFA[i,1]) == format(tblRFA[j,5]))
      # find all known pups of the hyena
    {
      if (tblRFA[j,6] != "")
        # check if their birthday is known
      {
        if (tblRFA[i,8] != "")
          # check if there is an existing adult date to compare with
        {
          if (as.Date(tblRFA[j,6], "%m/%d/%y") < as.Date(tblRFA[i,8]))
            # if so, check if a pup has a birthday earlier than the current adult date
          {
            tblRFA[i,8] <- format(as.Date(tblRFA[j,6], "%m/%d/%y"))
            # if it does, set it as the new one
          }
        }
        else
        {
          tblRFA[i,8] <- format(as.Date(tblRFA[j,6], "%m/%d/%y"))
          # if there is not, set the birthday of the pup as the new adult date
        }
      }
    }
  }
}

#Fix some mom's
tblRFA[62,5] <- "02"
tblRFA[184,5] <- "02"


#--------------------------
#Formatting for tblA

#Read in Data from Raw_Data directory
tblA1 <- read.csv("~tblAggression_preupdate2011.csv", stringsAsFactors = FALSE)
tblA2 <- read.csv("~tblAgression_new2011_Virginia.csv", stringsAsFactors = FALSE)
tblA3 <- read.csv("~tblAgression_new2011_Molly.csv", stringsAsFactors = FALSE)
tblA  <- rbind(tblA1, tblA2, tblA3)

#Remove unused rows, order the rows, remove observations before 1989
tblA <- tblA[,-c(1,3,4,7:17)]
tblA <- subset(tblA, DATE != "")
tblA <- tblA[order(as.Date(tblA$DATE, format="%m/%d/%y")),]
tblA <- tblA[-c(1:1420),]

setwd(Formatted_Data)
save(tblRFA, file = "tblRFA.RData")
save(tblA, file = "tblA.RData")
setwd(Code)
