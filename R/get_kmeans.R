pacman::p_load(tidyverse)

get_kmeans <- function (x,K){
  
  ###Check inputs
  #check that x is numeric
  if(!is.numeric(x)){
    stop("x must be a numeric vector")
  }
  
  #k must be at least 1 and be an integer
  if(K != round(K) || K < 1){
    stop("K must be an integer >= 1")
  }
  
  #check that x has at least K observations
  if (length(x) < K){
    stop("x must have at least K observations")
  }
  
  #extra check to see if x has at least K unique observations.
  #if x has < K unique observations, we will end up getting less than K clusters
  #e.g if x = c(1,1,2) -> get_kmeans(x,3) will actually have 2 clusters in stead of 3.
  #to prevent this issue, we will require x has at least K unique observations
  if (length(unique(x)) <K){
    stop("x has less than K unique observations, unable to provide K clusters")
  }
  
  ###K-means algorithm
  N <- length(x)
  df<-tibble (x = x)
  
  #Get initial clusters
  unique_x <- sort(unique(df$x))
  M<- length(unique_x)
  
  initial_cluster <- tibble (x = unique_x,
                             cluster = rep(1:K, each = ceiling(M/K), length.out = M))
  
  #Merge clusters with original data
  df<- df %>% left_join(initial_cluster, by = 'x')
  
  #At this step, we have data assigned with initial clusters (clusters are numbered with order of ascending mean value)
  
  #Iterative steps to get new clusters, stop when new clusters and old clusters are identical
  old_clusters <- NULL
  while (!identical(df$cluster, old_clusters)){
    old_clusters <- df$cluster
    #get cluster_mean
    cluster_mean <- df %>% group_by(cluster) %>% summarise(mean = mean(x))
    
    #reassign clusters based on calculated mean
    new_clusters <- numeric(N)
    for (i in 1:N){
      new_clusters[i] <- (cluster_mean %>% mutate (dist = (mean - df$x[i])^2) %>% arrange(dist) %>% head(1))$cluster
    }
    df$cluster <- new_clusters
  }
  
  return (df$cluster)
}

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
  
  #Create a tibble and merge back to get final result
  sorted_tibble <- tibble (x = x_sorted,cluster = cluster)%>%
    distinct(x,cluster)
  
  clustered_data <- tibble (x=x) %>% left_join (sorted_tibble, by ="x")
  
  return (clustered_data$cluster)
  
}
