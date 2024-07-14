pacman::p_load(tidyverse)
# Function to perform LDA with two predictors
#
# INPUT
# Five vectors:
# x1 <- 1:9
# x2 <- c(3,8,9,2,4,7,1,5,6)
# y <- rep(LETTERS[1:3], each = 3)
# new1 <- c(2,8)
# new2 <- c(6,5)
#
# OUTPUT
# get_LDA2(x1 = x1, x2 = x2, y = y, new1 = new1, new2 = new2)

# function named get_LDA2
get_LDA2 <- function(x1, x2, y, new1, new2) {
  #Check type of the input
  stopifnot(is.numeric(x1))
  stopifnot(is.numeric(x2))
  stopifnot(is.numeric(new1))
  stopifnot(is.numeric(new2))
  
  #Check length of the input
  stopifnot(length(x1) == length(x2) & length(x1)== length(y))
  stopifnot(length(new1) == length(new2))
  
  #Get number of class
  K <- length(unique(y))
  
  #Check if there are at least 2 classes
  stopifnot(K>1)
  
  #Load input into a tibble for manipulation
  df <- tibble (x1,x2,y)
  df <- df %>% drop_na()
  
  #Get number of observations
  N <- length(df$x1)
  
  #Check if number of observations per level is at least 2
  obs_per_level <-df %>% count(y)
  stopifnot(obs_per_level$n >1)
  
  #Check if variance per level is not zero
  var_per_level<-df %>% 
    group_by(y) %>%
    summarise(var1 = var(x1),
              var2 = var(x2))%>%
    select(-y)
  
  stopifnot(var_per_level !=0)  
  
  # Estimate of mu and pi
  df <- df %>%
    group_by(y) %>%
    mutate (mu1 = mean(x1),
            mu2 = mean(x2),
            centered_x1 = x1 - mu1,
            centered_x2 = x2 - mu2,
            muK = map2(mu1,mu2,c),
            pi = n()/N)
  
  # Calculate Covariance matrix and its inverse
  centered_x1x2 <- matrix(c(df$centered_x1, df$centered_x2), ncol = 2)
  cov <- 1/(N-K) * t(centered_x1x2) %*% centered_x1x2
  cov_inv <- solve(cov)
  
  # Store mu, pi and class into vector form for calculation
  mu<-df %>%slice(1) %>% pull(muK)
  pi <-df %>%slice(1) %>% pull(pi)
  class <- df %>% slice(1) %>%pull(y)
  
  # Manipulate New data into matrix form for computation
  new_df <- tibble(new1,new2)
  new_df <- new_df %>% drop_na()
  M <- length(new_df$new1)
  new_data<-matrix(c(new_df$new1,new_df$new2), ncol =2)
  
  # For each new observations, calculate delta for each level and level with highest delta is then store in pred
  pred <- numeric(M)
  for (m in 1:M){
    delta<-numeric(K)
    for (i in 1:K){
      delta[i] <-t(new_data[m,]) %*% cov_inv %*% mu[[i]] - 0.5 %*%t(mu[[i]]) %*% cov_inv %*% mu[[i]] + log(pi[i])
    } 
    pred[m] <-class[which.max(delta)]
  }
  return (pred)
}

# x1 <- 1:9
# x2 <- c(3,8,9,2,4,7,1,5,6)
# y <- rep(LETTERS[1:3], each = 3)
# new1 <- c(2,8)
# new2 <- c(6,5)
# 
# get_LDA2(x1 = x1, x2 = x2, y = y, new1 = new1, new2 = new2)

