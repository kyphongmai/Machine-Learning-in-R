pacman::p_load(tidyverse)
optimal_kmeans <- function(x,K){
  
  ###Check inputs
  #check that x is numeric
  if(!is.numeric(x)){
    stop("x must be a numeric vector")
  }
  
  #k must be at least 1 and be an integer
  if(K != round(K) || K < 1){
    stop("K must be an integer >= 1")
  }
  
  #check that x has at least K unique observations
  if (length(unique(x)) < K){
    stop("x must have at least K unique observations")
  }
  
  x_sorted <- sort(x)
  N<- length(x)
  #Initialize D (minimal within ss) and b(store the smallest index of each cluster)
  df <- tibble()
  b<- tibble()
  
  #Initializing functions to help calculate sum of square distance
  ssd <- function (x){
    dframe <- tibble (x=x)
    mu <- mean(x)
    ssd <-dframe %>% mutate(dist = (x - mu)^2) %>% summarise(sum = sum(dist))
    return (ssd$sum)
  }
  
  #Iteration to get D matrix with D[i,m] storing the minimum within ss of clustering x1,...xi into m clusters 
  for (m in 1:K){
    for (i in m:N){
      if (m!= 1){
        value <- vector()
        for (j in m:i){
          value[j] <- df[[j-1,m-1]] + ssd(x_sorted[j:i])
        }
        df[i,m] <- min(value, na.rm = TRUE)
        b[i,m] <- which.min(value)
      }else{
        df[i,1] <- ssd(x_sorted[1:i])
        b[i,1] <- 1
      }
    }
  }
  
  #Iterative backtracking steps to get the lowest indices of all clusters
  m <-K
  value <- b[[nrow(b),ncol(b)]]
  start_index <- numeric(K)
  start_index[m] <- value
  
  while (m > 1){
    m<- m-1
    b <- b[1:value-1,1:m]
    value <- b[[nrow(b),ncol(b)]]
    start_index[m] <-value
  }
  
  #Clustering results for sorted data
  index <-rep(1:N)
  cluster<- numeric(N)
  for (i in 1:K){
    value <- start_index[i]
    for (j in 1:N){
      if (index[j] >= value){
        cluster[j] <- i
      }
    }
  }
  
  #Create a tibble
  sorted_tibble <- tibble (x = x_sorted,cluster = cluster)%>%
    distinct(x,cluster)
  
  clustered_data <- tibble (x=x) %>% left_join (sorted_tibble, by ="x")
  
  return (clustered_data$cluster)

}
