## Algorithm 1
data <- read.csv('./cleaned_data.csv')
n <- nrow(data)
m <- 15
adjmat <- matrix(1, nrow = m, ncol = m)
theta_list <- vector("list", m-2)
N_list <- list(vector("list", m-2), vector("list", m-2))

numvalues <- c(4,rep(2,14)) ## number of values that can be taken of each parameter


mb <- function(this.sample, k){ ## Markov Blanket Probability
  this.sample[k] <- 0
  if(k > 2){ ## Not the root node
    p_0 <- theta_list[[k]][c(sample[which(adjmat[,k]==1)]+1)] ## P(X_miss=0|Pa(X_miss))
  }else{
    p_0 <- 1
  }
  for(i in which(adjmat[k,]==1)){ ## The childrens
    if (this.sample[i] == 0){ ## P(Ch(X_miss)_i = 0| Pa(Ch(X_miss)))
      p_0 <- p_0*(1-theta_list[[i]][c(sample[which(adjmat[,i]==1)]+1)])
    }
    if (this.sample[i] == 1){ ## P(Ch(X_miss)_i = 1| Pa(Ch(X_miss)))
      p_0 <- p_0*theta_list[[i]][c(sample[which(adjmat[,i]==1)]+1)]
    }
  }
  
  this.sample[k] <- 1
  if(k > 2){ ## Not the root node
    p_1 <- theta_list[[k]][c(sample[which(adjmat[,k]==1)]+1)] ## P(X_miss=1|Pa(X_miss))
  }else{
    p_1 <- 1
  }
  for(i in which(adjmat[k,]==1)){ ## The childrens
    if (this.sample[i] == 0){ ## P(Ch(X_miss)_i = 0| Pa(Ch(X_miss)))
      p_1 <- p_1*(1-theta_list[[i]][c(sample[which(adjmat[,i]==1)]+1)])
    }
    if (this.sample[i] == 1){ ## P(Ch(X_miss)_i = 1| Pa(Ch(X_miss)))
      p_1 <- p_1*theta_list[[i]][c(sample[which(adjmat[,i]==1)]+1)]
    }
    
  }
  
  update.value <- base::sample(c(0,1),prob = c(p_0/(p_0+p_1), p_1/(p_0+p_1)))
  return(update.value)
}


for(i in 1:(m-2)){
  ## Initialize parameters
  theta_list[[i]] <- array(0.5,
                           dim=numvalues[which(adjmat[,(i+2)]==1)])
}


## E-step
for(i in 1:n){
  if (sum(is.na(data)) == 0){
    for (j in 1:(m-2)){
      N_list[[data[i,(j+2)]+1]][[j]][c(data[i,which(adjmat[,(j+2)]==1)]+1)] =+ 1
    }
  }else{
    ## Gibbs sampler
    
    this.sample <- as.numeric(data[i,])
    this.sample[which(is.na(data[i,]))] <- sample(c(0,1), replace=TRUE)
    
    for (t in 1:2000){
      for (k in which(is.na(data[i,]))){
        this.sample[k] <- mb(this.sample, k)
      }
      if(t > 1000){ ## After burn-in
        for (l in 1:(m-2)){
          N_list[[this.sample[(l+2)]+1]][[l]][c(this.sample[which(adjmat[,(l+2)]==1)]+1)] =+ 1/1000
        }
      }
    }
  }
}

## M-step

s1 <- mapply("+", N_list[[1]], N_list[[2]], SIMPLIFY = FALSE)
theta_list <- mapply("/",N_list[[2]], s1)




