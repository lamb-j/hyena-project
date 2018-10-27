#-------------------------
#This program calculates the external connectivity, from one matriline to another
#It compares matrilineis by pair
source("wd.R")
setwd(Formatted_Data)
load("tblRFA.RData")

rfVec  <- as.vector(tblRFA$ID)
WeekVec <- seq(as.Date('1994-12-31'),to=as.Date('1999-12-31'),by='1 year')
#----------------
#Establish Matrilines

A <- c("02","dps","gin", "tp")
B <- c("23s","27","bm","but","cap","hk","msha","raf","scby","sd","viv","zor")
C <- c("67","brad","iz","j","str")
D <- c("79","amar","ani","baez","bail","bass","dqi","dyl","gui","hex","hrp","kl","psy","rm","rum",
"tq","ua","vf","wk")
E <- c("aca","adon","alfe","ali","all","amaz","aqua","arse","art","atac","ath","bb","blue","bnd",
"boba","bsh","cblt","cen","chw","coo","csn","ctz","dcub","dfy","dion","dra","emer","fp","gel","gil",
"gren","guci","haw","hel","hem","jab","jb","jone","juno","kal","kb","ken","kier","king","kip","loki",
"lop","lucy","mali","mgta","mh","midd","mono","mos","mrph","pan","parc","polo","rpl","rump","sam",
"sbl","sein","sie","stb","tdee","tgw","thor","tia","tpe","vdg","vio","wat","who","wm","wr","wy",
"yoda","zc")

F <- c("arg","atrx","bab","bmr","bsp","cam","deb","dj","ffl","gum","hg","hon","jaw","jet","jj","maz",
"mp","msl","nola","oke","suk","tilt","xxx","yog","zing")
G <- c("bam","mar")
H <- c("bd")
I <- c("bern", "lg", "sqa")
J <- c("bf", "scy", "spo")
K <- c("bil","es","fd","gan","gol","hob","jade","lira","mite")
L <- c("bn","je","sr")
M <- c("bsl","cart","cnti","coch","dil","ger","hio","hml","hoo","jut","lbm","mac","mmn","nav","obam",
"pt","puc","pyg","reag","roos","sals","sx","tang","vns","whh","yosh")
N <- c("cla","xs")
O <- c("cr","fg","gho","ot","sud","zim")
P <- c("csl")
Q <- c("eng")
R <- c("gw","par","rv","sls","tinu")
S <- c("mc3")
T <- c("mw")


mlist <- list(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)
mvec  <- paste(LETTERS[1:20]) 
mSize_Matrix <- matrix(0, ncol = length(WeekVec), nrow = length(mvec))
rownames(mSize_Matrix) <- mvec
colnames(mSize_Matrix) <- format(WeekVec, "%Y")
  
ext_All <- matrix(0, nrow = length(mlist), ncol = length(mlist))
rownames(ext_All) <- mvec
colnames(ext_All) <- mvec

#------------------------------------------
extM <- function(twM, HVec, year){
  rownames(twM) <- rfVec
  colnames(twM) <- rfVec

#Remove cubs from HVec
#HVec - vector of observed hyenas for the year
#rfVec - vector of resident female adult hyenas observed over the 22 year study
 
  HVec <- Reduce(intersect, list(rfVec, HVec))

  cubVec <- vector()
  for(i in 1:length(HVec)){
     index <- match(HVec[i], rfVec)
     if(tblRFA[index,8] == "") next
    
     adult_date <- as.Date(tblRFA[index, 8], "%Y-%m-%d")
      
     if(adult_date > WeekVec[year - 1993]) cubVec <- c(cubVec, i)
  }
  HVec <- HVec[-cubVec]

#This function takes two Matrilines as arguments, and uses the twM to calculate the sum of the
#interactions of the indivduals between the matrilines
  mat_comp <- function(M1, M2, a){

    cMatrix <- matrix(nrow = length(M1), ncol = length(M2) )
    rownames(cMatrix) <- M1
    colnames(cMatrix) <- M2
    
    #creates a connectivity matrix for M1 and M2
    for(i in 1:length(M1)){
      for(j in 1:length(M2)){
        cMatrix[i,j] <- twM[ (M1[i]), (M2[j]) ]
      }
    }
    
    #calculates the sum of the twai matrix
    sum <- sum(cMatrix)
    if (sum == 0) return(0)

    #Now we need to figure out how many of the Hyenas were actually relevant, and
    #use that to deteremine the number of actual connections to divide by.
    #Figure out how to exclude cubs from HVec   
    
    #Removes indivuduals that were never observed and cubs
    HRow <- Reduce(intersect, list(M1, HVec))
    HCol <- Reduce(intersect, list(M2, HVec))
    
    mSize_Matrix[a, year-1993] <<- length(HRow)
 
    if( (length(HRow) * length(HCol)) == 0) return(0) 
    
    sum <- sum / (length(HRow)*length(HCol))
    return(sum)
  }

#For each mlist i
  #For each mlist j
     #if i == j
       #mmatrix [i, j] = 1
     #else make a matrix of valid individuals
     #fill matrix with twai values
     #sum values and divide by nrow*ncol
     #mmatrix [i, j] = value

  ext_Matrix <- matrix(0, nrow = length(mlist), ncol = length(mlist))
  rownames(ext_Matrix) <- mvec
  colnames(ext_Matrix) <- mvec

#cycles through all pairs of Matrilines  
  for(i in 1:length(mlist)){
    for(j in 1:length(mlist)){
      if(FALSE) ext_Matrix[i, j] <- 1
      else ext_Matrix[i, j] <- mat_comp(mlist[[i]], mlist[[j]], i)
    }
  }
  
  ext_All <<- ext_All + ext_Matrix
  print(ext_All)
}

setwd(Tables)
for(i in 1994:1999){
  print(i)
  load(sprintf("twM_%d.RData", i))
  load(sprintf("HVec_%d.RData", i))  
  extM(twM, HVec, i)
}

A <- ext_All 

setwd(Formatted_Data)
save(A, file = "A.RData")
setwd(Code) 
