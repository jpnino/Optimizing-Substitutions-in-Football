# Create dataset filtered by match and insert into list

match_list <- list()

for(t in 1:40){
  
  match_list[[t]] <- assign(paste0("match_",t), match_all %>% filter(match_id == t) %>% arrange(timeSec))
  
}

#name datasets in the list

for(h in 1:40){
  
  names(match_list)[h] <- paste0("match_",h)
  
}

############# function to calculate score columns ##################################
score_fn <- function(x){
  
  #create column for row sequence
  x$seq <- 1:nrow(x)
  
  
  # calculate total goals
  for(i in 2:max(x$seq)){
    
    if(x$event_name[i] != "Goal"){
      
      x$total_goals[i] <- x$total_goals[i-1] 
      
    } else {
      
      x$total_goals[i] <- x$total_goals[i-1] + 1
      
    } 
  }
  
  # calculate score_h
  for(j in 2:nrow(x)){
    
    if(x$event_name[j] != "Goal"){ 
      
      x$score_h[j] <- x$score_h[j-1] 
      
    } else if(x$event_name[j] == "Goal" & x$Home_Away[j] == 1){
      
      x$score_h[j] <- x$score_h[j-1] + 1
      
    } else if(x$event_name[j] == "Goal" & x$Home_Away[j] == 0){
      
      x$score_h[j] <- x$score_h[j-1] - 1
      
    }
  }
  
  #calculate score_a
  for(k in 2:nrow(x)){
    
    if(x$event_name[k] != "Goal"){ 
      
      x$score_a[k] <- x$score_a[k-1] 
      
    } else if(x$event_name[k] == "Goal" & x$Home_Away[k] == 1){
      
      x$score_a[k] <- x$score_a[k-1] - 1
      
    } else if(x$event_name[k] == "Goal" & x$Home_Away[k] == 0){
      
      x$score_a[k] <- x$score_a[k-1] + 1
      
    }
  }
  
  #calculate score for each event
  for(l in 1:nrow(x)){
    
    if(x$Home_Away[l] == 1){
      x$score[l] <- x$score_h[l] 
    }else{
      x$score[l] <- x$score_a[l]
    }
    
  }
  
  # Calculate categorical variable for match score situation
  
  for(u in 1:nrow(x)){
    
    if(x$Home_Away[u] == 1){
      if(x$score_h[u] == 1){
        x$situation[u] <-  "Up_1"} else if(x$score_h[u] == 2){ 
          x$situation[u] <-  "Up_2"} else if(x$score_h[u] > 2){
            x$situation[u] <- "Up_>2"} else if(x$score_h[u] == -1){
              x$situation[u] <- "Down_1"} else if(x$score_h[u] == -2){
                x$situation[u] <-  "Down_2"} else if(x$score_h[u] < -2){
                  x$situation[u] <- "Down_>2"} else if(x$score_h[u] == 0){
                    x$situation[u] <- "Tie"}  
    }else if(x$Home_Away[u] == 0){
      if(x$score_a[u] == 1){
        x$situation[u] <-  "Up_1"} else if(x$score_a[u] == 2){ 
          x$situation[u] <-  "Up_2"} else if(x$score_a[u] > 2){
            x$situation[u] <- "Up_>2"} else if(x$score_a[u] == -1){
              x$situation[u] <- "Down_1"} else if(x$score_a[u] == -2){
                x$situation[u] <-  "Down_2"} else if(x$score_a[u] < -2){
                  x$situation[u] <- "Down_>2"} else if(x$score_h[u] == 0){
                    x$situation[u] <- "Tie"}  
    }
  }    
  
  # Convert to factor type
  
  x$situation <- factor(x$situation)
  
  
  # calculate xGh
  for(w in 2:nrow(x)){
    
    if(x$Home_Away[w] == 1){
      
      x$xGh[w] <- x$xGh[w-1] + x$xG[w]
      
    } else if(x$Home_Away[w] == 0){
      
      x$xGh[w] <- x$xGh[w-1]
      
    }
  }
  
  #calculate xGa
  for(p in 2:nrow(x)){
    
    if(x$Home_Away[p] == 0){
      
      x$xGa[p] <- x$xGa[p-1] + x$xG[p]
      
    } else if(x$Home_Away[p] == 1){
      
      x$xGa[p] <- x$xGa[p-1]
      
    }
  }
  
  #calculate xGt
  for(l in 1:nrow(x)){
    
    if(x$Home_Away[l] == 1){
      x$xGdiff[l] <- x$xGh[l] - x$xGa[l]
    }else if(x$Home_Away[l] == 0){
      x$xGdiff[l] <- x$xGa[l] - x$xGh[l]
    }
  }
  
  ## sub before after
  
  
  for(m in 2:nrow(x)){
    
    if(x$event_name[m] == 'Player on' & x$period_id[m] == 2 & x$Home_Away[m] == 1){
      
      x$sub_not_h[m] <- 1
      
    } else if(x$period_id[m] == 1){
      
      x$sub_not_h[m] <- 0
      
    } else if(x$sub_not_h[m] != 1) {
      
      x$sub_not_h[m] <- x$sub_not_h[m-1]
      
    }
  }
  
  for(mm in 2:nrow(x)){
    
    if(x$event_name[mm] == 'Player on' & x$period_id[mm] == 2 & x$Home_Away[mm] == 0){
      
      x$sub_not_a[mm] <- 1
      
    } else if(x$period_id[mm] == 1){
      
      x$sub_not_a[mm] <- 0
      
    } else if(x$sub_not_a[mm] != 1) {
      
      x$sub_not_a[mm] <- x$sub_not_a[mm-1]
      
    }
  }
  ############## sub effectiveness h#################PROBLEM
  #min side of the subtraction is classified as inf. Purpose is to get the value of xGdiff when the value of sub_not_h/a ==1/0 and the min timeSec given that parameter.
  
  for (zz in 1:nrow(x)){
    
    if(x$sub_not_h[zz] == 1  & x$Home_Away[zz] == 1){
      
      x$sub_eff_h[zz] <- (max(x[x$sub_not_h== 1 & x$timeSec == max(x$timeSec),]$xGdiff)-
                            min(x[x$sub_not_h == 1
                                  & x$timeSec == min(c(x$timeSec[x$sub_not_h == 1],0)) 
                                  & x$Home_Away == 1,]$xGdiff))
      
    } else if (x$sub_not_h[zz] == 0){
      x$sub_eff_h[zz] <- 0
    }
  }
  
  for (yy in 1:nrow(x)){
    if(x$sub_not_a[yy] == 1 & x$Home_Away[yy] == 0){
      x$sub_eff_a[yy] <- (max(x[x$sub_not_a== 1 & x$timeSec == max(x$timeSec),]$xGdiff)- 
                            min(x[x$sub_not_a == 1
                                  & x$timeSec == min(c(x$timeSec[x$sub_not_a == 1],0)) 
                                  & x$Home_Away == 0,]$xGdiff))
      
    } else if (x$sub_not_a[yy] == 0){
      x$sub_eff_a[yy] <- 0
    }
  }
  
  
  #############################
  
  #return list
  x
  
}
################### END OF FUNCTION ###########################################################


# Apply new function to every dataset in the list

match_list <- lapply(match_list, score_fn)

# Merge all 40 matches into one single dataframe

all_matches <- bind_rows(match_list)
