#-------------------------
#This program calculates the external connectivity, from one matriline to another
#It compares matrilineis by pair
source("wd.R")
setwd(Formatted_Data)
load("tblRFA.RData")

rfVec  <- as.vector(tblRFA$ID)
WeekVec <- seq(as.Date('1989-12-31'),to=as.Date('2009-12-31'),by='1 year')
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
S <- c("mc3") #mystery cub
T <- c("mw")


mlist <- list(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)
mvec  <- paste(LETTERS[1:20]) 

#------------------------------------------
intV <- function(twM, HVec, year){
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
      
     if(adult_date > WeekVec[year - 1988]) cubVec <- c(cubVec, i)
  }
  HVec <- HVec[-cubVec]

#This function takes two Matrilines as arguments, and uses the twM to calculate the sum of the
#interactions of the indivduals between the matrilines
  mat_comp <- function(M){

    cMatrix <- matrix(0, nrow = length(M), ncol = length(M) )
    rownames(cMatrix) <- M
    colnames(cMatrix) <- M
    
    print(M)
    print(cMatrix)
    #creates a connectivity matrix for M
    for(i in 1:length(M)){
      for(j in 1:length(M)){
        if (i == j) next
        cMatrix[i,j] <- twM[ (M[i]), (M[j]) ]
      }
    }
    print(M)
    print(cMatrix)
    #calculates the sum of the twai matrix
    sum <- sum(cMatrix)
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

  int_vec <- numeric(length(mlist))

#cycles through all pairs of Matrilines
#int_vec[i] = sum of internal connections divided by number of possible connections
  for(i in 1:length(mlist)){
    int_vec[i] <- mat_comp(mlist[[i]])
    mat_size   <- Reduce(intersect, list(HVec, mlist[[i]]))
    mat_size   <- length(mat_size)
    if(mat_size == 1) int_vec[i] <- 0
    else int_vec[i] <- int_vec[i] / (mat_size * (mat_size - 1))
  }
  return(int_vec)
}


Y <- 1989:2009
int_Matrix <- matrix(0, nrow = length(Y), ncol = length(mlist))
rownames(int_Matrix) <- Y
colnames(int_Matrix) <- mvec

setwd(Tables)
for(i in 1989:2009){
  print(i)
  load(sprintf("twM_%d.RData", i))
  load(sprintf("HVec_%d.RData", i))  
  int_Matrix[i - 1988, ] <- intV(twM, HVec, i)
}

print(int_Matrix)
int_Matrix <- t(as.matrix(ext_Matrix))

i <- int_Matrix

setwd(Formatted_Data)
save(i, file = "i.RData")
setwd(Code) 
