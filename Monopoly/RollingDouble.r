
######### Rolling Two Dice For Monopoly


RollTwoDice2 = function() {
  x.1 = 0
  x.2 = 0
  x.1 = sample(1:6,1)
  x.2 = sample(1:6,1)
  total.1 = x.1 + x.2
  if(x.1 != x.2) {
    total_score = total.1
  } 
    ### If player rolls double, then roll again.
    
    else if (x.1 == x.2) {
    x.3 = 0
    x.4 = 0
    x.3 = sample(1:6,1)
    x.4 = sample(1:6,1)
    total.2 = x.3 + x.4
    if(x.3 != x.4) {
      total_score = total.1 + total.2
    } 
    ## If player rolls second double, then roll again. 
    
      else if(x.3 == x.4) {
      x.5 = 0
      x.6 = 0
      x.5 = sample(1:6,1)
      x.6 = sample(1:6,1)
      total.3 = x.5 + x.6
      if(x.5 != x.6) {
        total_score = total.1 + total.2 + total.3
      } 
      ## If player rolls third double, then player goes to jail.
      
        else if(x.5 == x.6) {
        total_score = "Jail"
      }
    }
    }
  
  ## Return outcome from all possible rolls.
  
  return(total_score)
}


## First simulation of monopoly incorporating rolling two dice 
## function.

SimulateMonopoly1 = function(no_of_rolls) {
  landings = numeric(40)
  #Start at GO
  current = 1
  for(i in 1:no_of_rolls) {
    y = 0
    y = RollTwoDice2()
    if(y == "Jail") {
      current = 11
    } else if(y != "Jail") {
      current = current + y
    }
    if(current > 40) {
      current = current - 40
    } 
    landings[current] = landings[current] + 1 
    if(current==3 | current==18 | current==34) {
      cc_move = CommunityChest(current)
      if(cc_move != current) {
        current = cc_move
        landings[current] = landings[current] + 1
      }
    } else if(current==31) {
      current = 11  #Go to Jail
      landings[current] = landings[current] + 1
    } else if(current==8 | current==23 | current==37) {
      chance_move = Chance(current)
      if(chance_move != current) {
        current = chance_move
        landings[current] = landings[current] + 1
      }
    }
  }
  return(landings)
}
