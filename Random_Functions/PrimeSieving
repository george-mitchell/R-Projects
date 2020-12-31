## Prime Sieving


primes = function(x) {
  
  list = 2:x
  for(i in 2:x){
    for(j in (2:x)){
      if(j %% i == 0 & j>i) {
        list[j] = 0
      }
    }
  }
    list2 = which(list != 0)
    return(list2[2:length(list2)])
}
