#alpha_min_fast 
#
#read.policy is a function that reads a sarsop .policy file and formats it into an R dataframe
#' @param filename is a pointer to the file name where the .policy file is stored
#' @example
#' outfileName <- paste("./pomdp_solved/", "potoroo","_", paste(benefitRatio, collapse="_"),".policy", sep="")
#' policy <- read.policy(outfileName)
read.policy <- function(filename){
  alphavectors.dat <- read.table(filename, sep="\t", header=FALSE, stringsAsFactors = FALSE)
  alphavectors.dat <- alphavectors.dat$V1[-(1:3)] #drop the 3 preamble lines
  alphavectors.dat <- alphavectors.dat[-length(alphavectors.dat)] #drop the end line
  alphavectors.dat <- sapply(1:length(alphavectors.dat), function(i)  gsub(">", ">  ", alphavectors.dat[i], fixed=TRUE)) #adds a tab between the > and the first alpha vector value
  alphavectors.dat <- sapply(1:length(alphavectors.dat), function(i)   strsplit(alphavectors.dat[i], "\\s+"))
  alphavectors.dat <-data.frame(matrix(unlist(alphavectors.dat), nrow=length(alphavectors.dat), byrow=T), stringsAsFactors = FALSE)
  alphavectors.dat <- alphavectors.dat[,-c(1,ncol(alphavectors.dat))]
  alphavectors.dat[,1] <- gsub("action=", "", alphavectors.dat[,1])
  alphavectors.dat[,2] <- gsub("obsValue=", "",alphavectors.dat[,2]) 
  alphavectors.dat[,2] <- gsub(">", "",alphavectors.dat[,2])
  alphavectors.dat <- data.frame(sapply(alphavectors.dat, as.numeric))
  colnames(alphavectors.dat)[1:2] <- c("action", "obsValue")
  return(alphavectors.dat) 
}


precision <- 10  #user-defined
N <- 10 #user-define number of alphavectors

source('ILP_alphamin_fast.R')

#read in subsets of alpha vectors 
#filename <- "./pomdp_solved/ShinySolution_-20_0_0.policy"
filename <- "./pomdp_solved/ShinySolution_TOY.policy"
gamma_all <- read.policy(filename)
gamma_all <- cbind(1:nrow(gamma_all), gamma_all)
colnames(gamma_all)[1] <- "id"
#convert obsValue to factor for grouping
gamma_all$obsValue <- as.factor(gamma_all$obsValue)
n.obsVars <- length(levels(gamma_all$obsValue))
gamma_x <- split(gamma_all,gamma_all$obsValue)


#read in beliefs
filename2 <- "./beliefs.txt"
sample.beliefs <- read.table(filename2, header=FALSE, stringsAsFactors = FALSE)
sample.beliefs <- as.matrix(sample.beliefs)
#sample.beliefs <- matrix(unlist(sample.beliefs),dim(sample.beliefs)[1], dim(sample.beliefs)[2] ,byrow=F)  # convert to matrix so we can do dot products




#precompute s= (alpha*b- alpha_hat * b) in a stored matrix (see Ferrer-Mestres et al) and set the 
epsilon_lower <- 0
S <- matrix(0, nrow= nrow(gamma_all), ncol= nrow(gamma_all))  #preallocate C matrix
for (x in 1:n.obsVars){                 #for each observable variable
  n.alpha_x <- nrow(gamma_x[[x]])       #number of alpha vectors for observable variable x
  ids.x <- gamma_x[[x]]$id              #id values of the alpha vectors relevant to observable variable x
  gamma_x.matrix <- as.matrix(gamma_x[[x]][-c(1:3)]) #extract the alpha vectors
  s.arg <- sample.beliefs %*% t(gamma_x.matrix)   #compute alpha *b for all alpha vectors relevant to observable variable x
  for (i in 1:n.alpha_x){                         #for all of the alpha vectors relevant to observable variable x
     row_id <- ids.x[i]                           #get the row ID of the alpha vector we are testing
     s_candidates <- s.arg[,i]- s.arg             #compute s= (alpha*b- alpha_hat * b) for all alpha_hats (each alpha_hat is a column, rows are the sample beliefs)
     max.s_candidates <- apply(s_candidates, 2, function(x) max(x, na.rm = TRUE)) #get max values over beliefs by taking max of each column
     S[row_id, ids.x] <-  max.s_candidates        #store the maximum values over all beliefs in a block matrix
  }
  
  #also find extremes of value function for each (alpha*b) over all sampled beliefs and alpha vectors relevant to observed variable x
  #set initial lower bound (NOTE THIS NEEDS TO BE EDITED FOR A GENERAL SOLVER SINCE OUR REWARDS ARE ALL NEGATIVE)
  epsilon_lower <- min(apply(s.arg, 2, function(x) min(x, na.rm = TRUE)), epsilon_lower) #this is a minimisation since rewards are negative throughout the problem
}
#set initial upper bound
epsilon_upper <- 0


delta  <- epsilon_upper- epsilon_lower
# implement alpha-min-fast
while (delta < precision) {
  C <- matrix(0, nrow=nrow(S), ncol= ncol(S))
  epsilon <- (epsilon_upper+ epsilon_lower)/2
  #set C matrix
  for (i in 1:nrow(S)){
    for (j in 1:ncol(S)){
      if (S[i,j] <= epsilon) {
        C[i,j] <- 1
      } #end if
    } #end j
  } #end i
  
  return.lprec <- solve_ILP_alphamin.fast(N, gamma_all, C)  #first element is the status code (see ?solve.lpExtPtr), second element is the solution
  
  if (return.prec[[1]]==0){
    epsilon_upper <- epsilon
  }
  else {epsilon_lower <- epsilon}
  
  delta  <- epsilon_upper- epsilon_lower
} #end while

