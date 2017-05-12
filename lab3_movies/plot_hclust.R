plot_hclust_data = function(data, 
                            group_column,
                            column_names="movie_title",
                            group_distance_method="euclidian", 
                            clust_distance_method="complete", 
                            ks=1:9){
  library(ggplot2)
  library(dplyr, warn.conflicts = F)
  
  group_hclust <- data %>% 
    column_to_rownames(column_names) %>%
    select_(group_column) %>%
    dist(method = group_distance_method) %>%
    hclust(method = clust_distance_method)
  
  group_h <- tibble(k = ks) %>% 
             group_by(k) %>%
             do(cbind(data, group = as.character(cutree(group_hclust, .$k))))
  
  group_h %>% 
    ggplot(aes_string(y = group_column, colour = "group")) + 
    geom_jitter(aes(x = " "), 
                width = .2, height = 0, size = 2, alpha = .6) + 
    facet_wrap(~ paste(k, " Grupo")) + 
    xlab("") %>% 
    return()
  
}

plot_hclusts_2d = function(data, 
                            group_columns,
                            column_name,
                            group_distance_method,
                            clust_distance_method,
                            ks=1:9){
  #' Retorna um ggplot das soluções de agrupamento de `dados_filme` 
  #' para as quantidades de grupos em `ks` usando `hclust`.
  library(ggplot2)
  library(dplyr, warn.conflicts = F)
  
  group_hclust <- data %>% 
    column_to_rownames(column_name) %>%
    select_(group_columns) %>%
    dist(method = group_distance_method) %>%
    hclust(method = clust_distance_method)
  
  group_h<- tibble(k = ks) %>% 
    group_by(k) %>% 
    do(cbind(data, 
             group = as.character(cutree(group_hclust, .$k)))) 
  
  group_h %>% 
    ggplot(aes_string(x = group_columns[1], y = group_columns[2], colour = "group")) + 
    geom_jitter(width = .2, height = 0, size = 2, alpha = .6) + 
    facet_wrap(~ paste(k, " Grupo")) + 
    xlab("") %>% 
    return()
}

plot_kmeans_data_2d = function(d, group_columns, ks){
  
  library(dplyr)
  library(ggplot2)
  colNames <- match(group_columns,names(d))
  km <- d %>% select(colNames)
  
  classifier <- tibble(k=1:ks) %>% 
                group_by(k) %>% do(kmeans(km, .$k) %>% augment(d))
  
  colNames <- match(group_columns,names(classifier))
  coordinates <- classifier %>% select(colNames)
  
  classifier %>% 
    ggplot(aes(x = coordinates[2], y = coordinates[3], label = movie_title, colour = .cluster)) + 
    geom_jitter(width = .2, height = 0, size = 1, alpha = .6) +
    facet_wrap(~k)
  
}


plot_kmeans_data_1d = function(d, group_columns, ks){
  
  library(dplyr)
  library(ggplot2)
  colNames <- match(group_columns,names(d))
  km <- d %>% select(colNames) 

  classifier <- tibble(k=1:ks) %>% 
    group_by(k) %>% do(kmeans(km, .$k) %>% augment(d))
  
  colNames <- match(group_columns,names(classifier))
  coordinates <- classifier %>% select(colNames)
  
  classifier %>% ggplot(aes(x = "", y = coordinates[2], label = movie_title, colour = .cluster)) + 
    geom_jitter(width = .2, height = 0, size = 1, alpha = .6)+
    facet_wrap(~k)
  
}