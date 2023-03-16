# GET STRENGTH AND DIRECTION OF THE LINKS

get_links <- function(df, obs, method, cl) {
  
  # initial paralell processes
  # cl <- makeCluster(cluster)
  
  # initialize
  link <- NA
  link_str <- NA
  link_dir <- NA
  
  # get strength through bn bootstrap
  for (i in obs) {
    str <- boot.strength(df[1:i, 2:ncol(df)], R = 200, 
                         algorithm = method, 
                         cluster = cl)#, 
    # algorithm.args = list(whitelist = wl))#, blacklist = bl))
    link <- str[ , 1:2]
    link_str <- cbind(link_str, str[ , 3]) 
    link_dir <- cbind(link_dir, str[ , 4])
  }
  
  # connect names for links
  for (i in 1:nrow(link)) {
    link[i, "Links"] <- paste0(link[i,1],"-",link[i,2])
  }

  # create df strength
  link_str <- link_str %>% data.frame(.) %>%   
    # remove column with NA
    select(where(~!all(is.na(.x)))) %>%
    # rename column head as chainage
    set_colnames(c(round(df$Chainage[obs], 0))) %>%
    # add names of links
    mutate("Links" = link$Links)
  
  # df direction
  link_dir <- link_dir %>% data.frame(.) %>% 
    # remove column with NA
    select(where(~!all(is.na(.x)))) %>%
    # rename column head as chainage
    set_colnames(c(round(df$Chainage[obs], 0))) %>%
    # add names of links
    mutate("Links" = link$Links) 

  # store dfs as result
  result <- list()
  result[[1]] <- link_str
  result[[2]] <- link_dir
  
  # stop parallel processes
  stopCluster(cl)
  
  return(result)
} 
