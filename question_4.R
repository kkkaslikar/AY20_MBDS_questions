library(tidyverse)

curDir <- getwd()

mat <- read_tsv(file.path(curDir, "./Question\ 4/input_question_4"), col_names = F)

mat <- as.matrix(mat)


y <- rbind(c(0, 0, 1, 1),
           c(1, 1, 0, 0),
           c(0, 0, 0, 0),
           c(0, 1, 1, 0),
           c(0, 1, 1, 1)
           )


# defining 4-connectivity -------------------------------------------------


find_4_n <- function(x, i, j){

  n_collection <- vector(mode = "character")
  n1_i <- i + 1
  n1_j <- j

  if(n1_i <= nrow(x)){
    
    if(x[n1_i, n1_j] == 1){
      n1_string <- paste(n1_i, n1_j, sep = ",")
      n_collection <- c(n_collection, n1_string)
    }
  }
    
    n2_i <- i -1
    n2_j <- j 
    
    if(n2_i  >= 1){
      
      if(x[n2_i, n2_j] == 1){
        n2_string <- paste(n2_i, n2_j, sep = ",")
        n_collection <- c(n_collection, n2_string)
      }
    }
    
    n3_i <- i
    n3_j <- j + 1 
    
    if(n3_j  <= ncol(x)){
      
      if(x[n3_i, n3_j] == 1){
        n3_string <- paste(n3_i, n3_j, sep = ",")
        n_collection <- c(n_collection, n3_string)
      }
    }
    
    n4_i <- i
    n4_j <- j - 1 
    
    if(n4_j  >= 1){
      
      if(x[n4_i, n4_j] == 1){
        n4_string <- paste(n4_i, n4_j, sep = ",")
        n_collection <- c(n_collection, n4_string)
      }
    }
    return(n_collection)
}

# defining 4-neighbour collections -------------------------------------------

get_4_n_collections <- function(x){
  
  n_list <- list("blank" = NULL)
  
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      
      if(x[i, j] > 0){
        n_string <- paste(i, j, sep = ",")
        n_collection <- find_4_n(x, i, j)    # find 4 neighbour groups
        integration_status <- 0
        
        for(name in names(n_list)){
          if(n_string %in%  n_list[[name]]){
            n_list[[name]] <- union(n_list[[name]], n_collection)
            integration_status <- 1
          } else if(length(intersect(n_collection, n_list[[name]])) > 0){
            n_list[[name]] <- union(n_list[[name]], n_collection)
            integration_status <- 1
          }
        }
        
        if(integration_status == 0){
          n_list[[n_string]] <- c(n_collection, n_string)
        }
      }
    }
  }
  
  ## cleanup
  
  n_list <- n_list[!names(n_list) == "blank"]   # removing placeholder
  
  n_list <- n_list[sapply(n_list, length) != 0] # removing list elements of length 0 (if any)
  
  to_remove <- vector(mode = "character")       # container for storing redundant elements
  
  for(name1 in names(n_list)){
    for(name2 in names(n_list)){
      if(name2 != name1 & name2 %in% n_list[[name1]] & !name1 %in% to_remove){
        n_list[[name1]] <- union(n_list[[name1]], n_list[[name2]])
        to_remove <- c(to_remove, name2)
      }else if(name2 != name1 & length(intersect(n_list[[name1]], n_list[[name2]])) > 0 & !name1 %in% to_remove){
        n_list[[name1]] <- union(n_list[[name1]], n_list[[name2]])
        to_remove <- c(to_remove, name2)
      }
    } 
  }
  
  n_list <- n_list[!names(n_list) %in% to_remove]
  
  return(n_list)
}

# getting 4-neighbour collections -------------------------------------------

n4_list <- get_4_n_collections(mat)

# defining labelling ---------------------------------------------------------------

label_clusters <- function(x, n_list){
  
  lab_mat <- x
  
  for(i in seq_along(n_list)){
    cluster_label <- i
    n_collection <- n_list[[i]]
    print(paste("cluster_label", cluster_label))
    print("n_collection\n")
    print(n_collection)
    
    n_collection_coord <- str_split(n_collection, pattern = ",")
    
    for(i in seq_along(n_collection_coord)){
      current_coords <- as.integer(n_collection_coord[[i]])
      lab_mat[current_coords[1], current_coords[2]] <- cluster_label
    }
  }
  return(lab_mat)
}


# 4-neighbour labelling ---------------------------------------------------------------

lab4_mat <- label_clusters(mat, n4_list)

lab4_mat

all(1:max(lab4_mat) %in% lab4_mat)

# defining 8-connectivity -------------------------------------------------

find_8_n <- function(x, i, j){
  
  n_collection <- vector(mode = "character")
  
  n1_i <- i + 1
  n1_j <- j
  
  if(n1_i <= nrow(x)){
    
    if(x[n1_i, n1_j] == 1){
      n1_string <- paste(n1_i, n1_j, sep = ",")
      n_collection <- c(n_collection, n1_string)
    }
  }
  
  n2_i <- i -1
  n2_j <- j 
  
  if(n2_i  >= 1){
    
    if(x[n2_i, n2_j] == 1){
      n2_string <- paste(n2_i, n2_j, sep = ",")
      n_collection <- c(n_collection, n2_string)
    }
  }
  
  n3_i <- i
  n3_j <- j + 1 
  
  if(n3_j  <= ncol(x)){
    
    if(x[n3_i, n3_j] == 1){
      n3_string <- paste(n3_i, n3_j, sep = ",")
      n_collection <- c(n_collection, n3_string)
    }
  }
  
  n4_i <- i
  n4_j <- j - 1 
  
  if(n4_j  >= 1){
    
    if(x[n4_i, n4_j] == 1){
      n4_string <- paste(n4_i, n4_j, sep = ",")
      n_collection <- c(n_collection, n4_string)
    }
  }
  
  n5_i <- i + 1
  n5_j <- j + 1 
  
  if(n5_i <= nrow(x) & n5_j <= ncol(x)){
    
    if(x[n5_i, n5_j] == 1){
      n5_string <- paste(n5_i, n5_j, sep = ",")
      n_collection <- c(n_collection, n5_string)
    }
  }
  
  n6_i <- i - 1
  n6_j <- j + 1 
  
  if(n6_i >= 1 & n6_j <= ncol(x)){
    
    if(x[n6_i, n6_j] == 1){
      n6_string <- paste(n6_i, n6_j, sep = ",")
      n_collection <- c(n_collection, n6_string)
    }
  }
  
  n7_i <- i + 1
  n7_j <- j - 1 
  
  if(n7_i <= nrow(x) & n7_j >= 1){
    
    if(x[n7_i, n7_j] == 1){
      n7_string <- paste(n7_i, n7_j, sep = ",")
      n_collection <- c(n_collection, n7_string)
    }
  }
  
  n8_i <- i - 1
  n8_j <- j - 1 
  
  if(n8_i >= 1 & n8_j >= 1){
    
    if(x[n8_i, n8_j] == 1){
      n8_string <- paste(n8_i, n8_j, sep = ",")
      n_collection <- c(n_collection, n8_string)
    }
  }
  
  return(n_collection)
}

# defining 8-neighbour collections -------------------------------------------

get_8_n_collections <- function(x){
  n_list <- list("blank" = NULL)
  
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      if(x[i, j] > 0){
        
        n_string <- paste(i, j, sep = ",")
        n_collection <- find_8_n(x, i, j)
        integration_status <- 0
        
        for(name in names(n_list)){
          if(n_string %in%  n_list[[name]]){
            integration_status <- 1
            n_list[[name]] <- union(n_list[[name]], n_collection)
          } else if(length(intersect(n_collection, n_list[[name]])) > 0){
            integration_status <- 1
            n_list[[name]] <- union(n_list[[name]], n_collection)
          }
        }
        
        if(integration_status == 0){
          n_list[[n_string]] <- c(n_collection, n_string)
        }
      }
    }
  }
  
  n_list <- n_list[!names(n_list) == "blank"]
  
  n_list <- n_list[sapply(n_list, length) != 0]
  
  to_remove <- vector(mode = "character")

  for(name1 in names(n_list)){
    for(name2 in names(n_list)){
      if(name2 != name1 & name2 %in% n_list[[name1]] & !name1 %in% to_remove){
        n_list[[name1]] <- union(n_list[[name1]], n_list[[name2]])
        to_remove <- c(to_remove, name2)
      }else if(name2 != name1 & length(intersect(n_list[[name1]], n_list[[name2]])) > 0 & !name1 %in% to_remove){
        n_list[[name1]] <- union(n_list[[name1]], n_list[[name2]])
        to_remove <- c(to_remove, name2)
      }
    } 
  }
  
  n_list <- n_list[!names(n_list) %in% to_remove]
  
  return(n_list)
}

# getting 8-neighbour collections -------------------------------------------

n8_list <- get_8_n_collections(mat)


# 8-neighbour labelling ---------------------------------------------------------------

lab8_mat <- label_clusters(mat, n8_list)

lab8_mat
