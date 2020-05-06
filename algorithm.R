## Algorithm 1

setwd('/Users/jiangyiran/Desktop/Purdue/2020Spring/546/project/covid-19')
data <- read.csv('./cleaned_data.csv')
adjmat <- read.csv('./Adjacency Matrix.csv')

n <- nrow(data)
m <- 15
names <- as.character(adjmat$X)[-16]
adjmat <- t(as.matrix(adjmat[1:15,2:16]))
rownames(adjmat) <- names
colnames(adjmat) <- names
adjmat[6,3] <- 0
adjmat[9,7] <- 0

my_order <- c(1,2,4,7,8,10,3,5,6,9,11,12,13,14,15)
data <- data[,my_order]
adjmat <- adjmat[my_order,my_order]
nroot <- 6

# adjmat <- matrix(1, nrow = m, ncol = m)
theta_list <- vector("list", m-nroot)
N_list <- list(vector("list", m-nroot), vector("list", m-nroot))

numvalues <- c(4,rep(2,14)) ## number of values that can be taken of each parameter


# test with a small subset
savedata <-data
saveadjmat <-adjmat
data <- data[1:50,-15]
adjmat <- adjmat[-15,-15]
m <- 14
theta_list <- vector("list", m-nroot)
N_list <- list(vector("list", m-nroot), vector("list", m-nroot))
numvalues <- c(4,rep(2,m-1))


## Initialize parameters
for(i in 1:(m-nroot)){
  print(which(adjmat[,(i+nroot)]==1))
  theta_list[[i]] <- array(0.5,
                           dim=numvalues[which(adjmat[,(i+nroot)]==1)])
}

## Initialize countings
for(i in 1:(m-nroot)){
  N_list[[1]][[i]] <- array(1e-4,
                      dim=numvalues[which(adjmat[,(i+nroot)]==1)])
  N_list[[2]][[i]] <- array(1e-4,
                            dim=numvalues[which(adjmat[,(i+nroot)]==1)])
}

## Useful Function
mb <- function(this.sample, k){ ## Markov Blanket Probability
  this.sample[k] <- 0
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
  
  this.sample[k] <- 1
  if(k > nroot){ ## Not the root node
    p_1 <- theta_list[[k-nroot]][matrix(c(this.sample[which(adjmat[,k]==1)]+1),1)] ## P(X_miss=1|Pa(X_miss))
  }else{
    p_1 <- 1
  }
  for(i in which(adjmat[k,]==1)){ ## The childrens
    if (this.sample[i] == 0){ ## P(Ch(X_miss)_i = 0| Pa(Ch(X_miss)))
      p_1 <- p_1*(1-theta_list[[i-nroot]][matrix(c(this.sample[which(adjmat[,i]==1)]+1),1)])
    }
    if (this.sample[i] == 1){ ## P(Ch(X_miss)_i = 1| Pa(Ch(X_miss)))
      p_1 <- p_1*theta_list[[i-nroot]][matrix(c(this.sample[which(adjmat[,i]==1)]+1),1)]
    }
    
  }
  
  update.value <- base::sample(c(0,1),1, prob = c(p_0/(p_0+p_1), p_1/(p_0+p_1)))
  return(update.value)
}



Rt_hist <- c()
  
for(u in 1: 10){ ## Try 10 iterations
  
  ## E-step
  
  for(i in 1:nrow(data)){
    if (sum(is.na(data)) == 0){
      for (j in 1:(m-nroot)){
        N_list[[data[i,(j+nroot)]+1]][[j]][matrix(c(data[i,which(adjmat[,(j+nroot)]==1)]+1),1)] = 
          N_list[[data[i,(j+nroot)]+1]][[j]][matrix(c(data[i,which(adjmat[,(j+nroot)]==1)]+1),1)]+1
      }
    }else{
      
      ## Gibbs sampler
      cat('Imputing samples ', i,'\n')
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


  ## M-step
  
  s1 <- mapply("+", N_list[[1]], N_list[[2]], SIMPLIFY = FALSE)
  new_theta_list <- mapply("/",N_list[[2]], s1)
  Rt <- abs(sum(unlist(new_theta_list) - unlist(theta_list)))
  Rt_hist <- c(Rt_hist,Rt)
  cat('|theta-theta_0| = ',Rt, '\n')
  theta_list <- new_theta_list
  
}


#plot(Rt_hist)
