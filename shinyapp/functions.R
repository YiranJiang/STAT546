get_prob <- function(adjmat,theta_list,inputinfo){
  m <- 15
  nroot <- 6
  # names <- as.character(adjmat$X)[-16]
  # adjmat <- t(as.matrix(adjmat[1:15,2:16]))
  # rownames(adjmat) <- names
  # colnames(adjmat) <- names
  # adjmat[6,3] <- 0
  # adjmat[9,7] <- 0
  # adjmat[6:14,15] <- 0
  
  my_order <- c(1,2,4,7,8,10,3,5,6,9,11,12,13,14,15)
  adjmat <- adjmat[my_order,my_order]
  nroot <- 6
  numvalues <- c(4,rep(2,14)) ## number of values that can be taken of each parameter
  
  
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
  
  na.pos <- which(is.na(inputinfo))
  output_prob <- vector("list", m)
  for(i in na.pos){
    output_prob[[i]] <- rep(0,numvalues[i])
  }
  
  ## Gibbs Sampler
  inputinfo[na.pos] <- base::sample(c(0,1),sum(is.na(inputinfo)),replace=TRUE)
  for (t in 1:3000){
    for (k in na.pos){
      inputinfo[k] <- mb(inputinfo, k)
    }
    if(t > 1000){ ## After burn-in
      for (l in na.pos){
        output_prob[[l]][inputinfo[l]+1] = 
          output_prob[[l]][inputinfo[l]+1] + 1/2000
      }
    }
  }
  return(output_prob)

}

  
  