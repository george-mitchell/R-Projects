# BINGO 

bingo = function(){
  
  results = vector('numeric', 90)

  r = 1:90

  for(i in 1:90){
    s = sample(r,1,replace = FALSE)
    s
    r <- r[-which(r == s)]
    results[i] = s
  }

  return(results)
}

b = bingo()

b[1]
