## Algorithm 1

# setwd('/Users/jiangyiran/Desktop/Purdue/2020Spring/546/project/covid-19')
data <- read.csv('./data/cleaned_data.csv')
# adjmat <- read.csv('./data/Adjacency Matrix.csv')

n <- nrow(data)
m <- 15
# names <- as.character(adjmat$X)[-16]
# adjmat <- t(as.matrix(adjmat[1:15,2:16]))
# rownames(adjmat) <- names
# colnames(adjmat) <- names
# adjmat[6,3] <- 0
# adjmat[9,7] <- 0
# adjmat[6:14,15] <- 0
# save(adjmat,file = './data/adjmat.Rdata')

load('./data/adjmat.RData')

## Input Hyperparameter
p <- 0.7
my_order <- c(1,2,4,7,8,10,3,5,6,9,11,12,13,14,15)
data <- data[,my_order]
adjmat <- adjmat[my_order,my_order]

data_star <- data
for (i in 3:14){
  data_star[!is.na(data_star[,i]) &data_star[,i] == 0,i] <- NA
}

nroot <- 6
numvalues <- c(4,rep(2,14)) ## number of values that can be taken of each parameter


## Useful Function
mb <- function(this.sample, k){ ## Markov Blanket Probability
  p_vec <- rep(NA,numvalues[k])
  for (s in ((1:numvalues[k]))){
    this.sample[k] <- s-1
    if(k > nroot){ ## Not the root node
      p_0 <- theta_list[[k-nroot]][matrix(c(this.sample[which(adjmat[,k]==1)]+1),1)] ## P(X_miss=0|Pa(X_miss))
    }else{
      p_0 <- 1
    }
    for(i in which(adjmat[k,]==1)){ ## The children
      if (this.sample[i] == 0){ ## P(Ch(X_miss)_i = 0| Pa(Ch(X_miss)))
        p_0 <- p_0*(1-theta_list[[i-nroot]][matrix(c(this.sample[which(adjmat[,i]==1)]+1),1)])
      }
      if (this.sample[i] == 1){ ## P(Ch(X_miss)_i = 1| Pa(Ch(X_miss)))
        p_0 <- p_0*theta_list[[i-nroot]][matrix(c(this.sample[which(adjmat[,i]==1)]+1),1)]
      }
    }
    p_vec[s] <- p_0
  }
  
  update.value <- base::sample((seq(numvalues[k])-1),1, prob = p_vec/sum(p_vec))
  
  return(update.value)
}


# Test with a small subset
# savedata <-data
# saveadjmat <-adjmat
# data <- data[1:50,-15]
# adjmat <- adjmat[-15,-15]
# m <- 14

## Step back
# data <- savedata
# adjmat <- saveadjmat


numvalues <- c(4,rep(2,m-1))
theta_list <- vector("list", m-nroot)
N_list_star <- list(vector("list", m-nroot), vector("list", m-nroot))
N_list <- list(vector("list", m-nroot), vector("list", m-nroot))
p_list <- list(vector("list", m-nroot), vector("list", m-nroot))
q_list <- list(vector("list", m-nroot), vector("list", m-nroot))
for(i in 1:(m-nroot)){
  p_list[[1]][[i]] <- array(p,
                                 dim=numvalues[which(adjmat[,(i+nroot)]==1)])
  p_list[[2]][[i]] <- array(p,
                                 dim=numvalues[which(adjmat[,(i+nroot)]==1)])
  q_list[[1]][[i]] <- array(1-p,
                            dim=numvalues[which(adjmat[,(i+nroot)]==1)])
  q_list[[2]][[i]] <- array(1-p,
                            dim=numvalues[which(adjmat[,(i+nroot)]==1)])
}



## Initialize parameters
for(i in 1:(m-nroot)){
  print(which(adjmat[,(i+nroot)]==1))
  theta_list[[i]] <- array(0.5,
                           dim=numvalues[which(adjmat[,(i+nroot)]==1)])
}

length(unlist(theta_list)) ## number of parameters

Rt_hist <- c()
ll_hist <- c()
ll_1_hist <- c()
ll_2_hist <- c()
Rt <- Inf
u <- 0

# savedata <-data
# data <- data[1:30,]
# data_star <- data_star[1:30,]
while(Rt > 0.1){
# for (u in 0:2){
  u <- u + 1
  cat('============== Iteration ', u,' ==============\n')
  ## E-step
  ## Initialize countings
  for(i in 1:(m-nroot)){
    N_list_star[[1]][[i]] <- array(1e-4,
                              dim=numvalues[which(adjmat[,(i+nroot)]==1)])
    N_list_star[[2]][[i]] <- array(1e-4,
                              dim=numvalues[which(adjmat[,(i+nroot)]==1)])
    N_list[[1]][[i]] <- array(1e-4,
                              dim=numvalues[which(adjmat[,(i+nroot)]==1)])
    N_list[[2]][[i]] <- array(1e-4,
                              dim=numvalues[which(adjmat[,(i+nroot)]==1)])
  }
  
  for(i in 1:nrow(data)){
    if (i %% 100 == 0){
    cat('Imputing samples ', i,'\n')
    }
    if (sum(is.na(data_star)) == 0){
      for (j in 1:(m-nroot)){
        N_list_star[[data_star[i,(j+nroot)]+1]][[j]][matrix(c(data_star[i,which(adjmat[,(j+nroot)]==1)]+1),1)] = 
          N_list_star[[data_star[i,(j+nroot)]+1]][[j]][matrix(c(data_star[i,which(adjmat[,(j+nroot)]==1)]+1),1)]+1
      }
    }else{
      
      ## Gibbs sampler
      
      this.sample <- as.numeric(data_star[i,])
      this.sample[which(is.na(data_star[i,]))] <- base::sample(c(0,1),sum(is.na(data_star[i,])),replace=TRUE)
      
      for (t in 1:2000){
        for (k in which(is.na(data_star[i,]))){
          this.sample[k] <- mb(this.sample, k)
        }
        if(t > 1000){ ## After burn-in
          for (l in 1:(m-nroot)){
            N_list_star[[this.sample[(l+nroot)]+1]][[l]][matrix(c(this.sample[which(adjmat[,(l+nroot)]==1)]+1),1)] =
              N_list_star[[this.sample[(l+nroot)]+1]][[l]][matrix(c(this.sample[which(adjmat[,(l+nroot)]==1)]+1),1)]+(1/1000)
          }
        }
      }
    }
    
    
    if (sum(is.na(data)) == 0){
      for (j in 1:(m-nroot)){
        N_list[[data[i,(j+nroot)]+1]][[j]][matrix(c(data[i,which(adjmat[,(j+nroot)]==1)]+1),1)] = 
          N_list[[data[i,(j+nroot)]+1]][[j]][matrix(c(data[i,which(adjmat[,(j+nroot)]==1)]+1),1)]+1
      }
    }else{
      
      ## Gibbs sampler
      # cat('Imputing samples ', i,'\n')
      this.sample <- as.numeric(data[i,])
      this.sample[which(is.na(data[i,]))] <- base::sample(c(0,1),sum(is.na(data[i,])),replace=TRUE)
      
      for (t in 1:2000){
        for (k in which(is.na(data[i,]))){
          this.sample[k] <- mb(this.sample, k)
        }
        if(t > 1000){ ## After burn-in
          for (l in 1:(m-nroot)){
            N_list[[this.sample[(l+nroot)]+1]][[l]][matrix(c(this.sample[which(adjmat[,(l+nroot)]==1)]+1),1)] =
              N_list[[this.sample[(l+nroot)]+1]][[l]][matrix(c(this.sample[which(adjmat[,(l+nroot)]==1)]+1),1)]+(1/1000)
          }
        }
      }
    }
    
    
  }
  
  ## Calculate Estimated log-likelihood
  
  ll_1 <- sum(unlist(N_list_star[[1]])*log(1-unlist(theta_list))) + 
    sum(unlist(N_list_star[[2]])*log(unlist(theta_list)))
  ll_2 <- sum(unlist(N_list[[1]])*log(1-unlist(theta_list))) + 
    sum(unlist(N_list[[2]])*log(unlist(theta_list)))
  ll <- p*ll_1 + (1-p)*ll_2
  cat('ll = ',ll, '\n')
  ll_1_hist <- c(ll_1_hist,ll_1)
  ll_2_hist <- c(ll_2_hist,ll_2)
  ll_hist <- c(ll_hist,ll)
  
  
  ## M-step
  l1 <- mapply("*", N_list_star[[1]], p_list[[1]], SIMPLIFY = FALSE)
  l2 <- mapply("*", N_list[[1]], q_list[[1]], SIMPLIFY = FALSE)
  l3 <- mapply("+", l1, l2, SIMPLIFY = FALSE)
  
  l4 <- mapply("*", N_list_star[[2]], p_list[[2]], SIMPLIFY = FALSE)
  l5 <- mapply("*", N_list[[2]], q_list[[2]], SIMPLIFY = FALSE)
  l6 <- mapply("+", l4, l5, SIMPLIFY = FALSE)  
  
  s1 <- mapply("+", l3, l6, SIMPLIFY = FALSE)
  new_theta_list <- mapply("/",l6, s1)
  
  # ll <- sum(unlist(N_list[[1]])*log(1-unlist(new_theta_list))) + 
  #   sum(unlist(N_list[[2]])*log(unlist(new_theta_list)))
  Rt <- sum(abs(unlist(new_theta_list) - unlist(theta_list)))
  Rt_hist <- c(Rt_hist,Rt)
  # cat('ll = ',ll, '\n')
  cat('|theta-theta_0| = ',Rt, '\n')
  theta_list <- new_theta_list
  
}


my_result <- list(Rt_hist = Rt_hist, ll_hist = ll_hist, theta_list = theta_list)
save(my_result, file = "algo1_result.RData")

#plot(Rt_hist)


plot(seq(10),ll_hist[1:10],type = 'l',xlab = 'iteration')



