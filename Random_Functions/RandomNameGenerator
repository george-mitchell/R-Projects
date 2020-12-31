## Random Name Generator

random.name = function(names,n){
  
  l = length(names)
  prob1 = vector('numeric', l)
  prob1 = prob1 + (1/l)
  results = vector('character', n)
  names1 = names
  
  for(i in 1:n){
    SMP = sample(names,1, prob = prob1)
    
    ## Collect randomnly generated names in a vector. 
    
    results[i] = SMP
    t = which(names == SMP)
    
    ## Decrease the chances of selecting name consecutively.
    
    prob1[t] = prob1[t]/2
    f = (1 - prob1[t])/(l-1)
    for(j in 1:(l)){
      if(prob1[j] != prob1[t]){
        prob1[j] = f
      }
      
    }
    
  }
   return(results)
}
