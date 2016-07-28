C_Index = function(df, obs_times = "obs_times"){
  df = df[order(df[obs_times]),]
  edges = 0
  
  concordant = 0
  
  dfe = df[df$d_i != 0,]
  
  for(i in 1:nrow(dfe)){
    
    t_i = dfe[i, obs_times]
    h_i = dfe[i, "h_t"]
    d_i = dfe[i, "d_i"]
    
    # message(paste("i ==", i, "  ", "d_i == ", d_i))
    
    for(j in 1:nrow(df)){
      if(i == j){ next }
      t_j = df[j, obs_times]
      h_j = df[j, "h_t"]
      d_j = df[j, "d_i"]
      
      if(t_j > t_i){
        edges = edges + 1
        
        if(h_i < h_j){
          concordant = concordant + 1
        }
        
      }
      
      
      
    }
    
  }
  message(paste("Concordant Pairs", concordant, 
                ", Total Edges", edges, 
                ", C-Index", (concordant / edges) ))
  return((concordant / edges))
}

