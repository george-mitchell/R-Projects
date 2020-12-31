## Computational Probability with R Module


## Question 1
no_of_rolls = 100000
SimulateMonopoly1(no_of_rolls)

## Question 2
SimulateMonopoly1 = function(no_of_rolls) {
  landings = numeric(40)
  #Start at GO
  current = 1
  for(i in 1:no_of_rolls) {
    current = current + RollTwoDice()
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
    }
  }
  return(landings)
}

## Question 3
CommunityChest = function(current) {
  goto = current
  u = runif(1)
  if(u < 1/16) {
    goto = 1 #Move to go
  } else if(u < 2/16) {
    goto = 11 #Go to Jail
  } else if(u < 3/16) {
    goto = 2 #Go to Old Kent Road
  }
  return(goto)
}

## Question 4
SimulateMonopoly1 = function(no_of_rolls) {
  landings = numeric(40)
  #Start at GO
  current = 1
  for(i in 1:no_of_rolls) {
    current = current + RollTwoDice()
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
    }
  }
  return(landings)
}

## Question 5 
  # Create Chance Function
Chance = function(current) {
  advance = current
  v = runif(1)
  if(v < 1/16) {
    advance = 1 #Advance to Go
  } else if(v < 2/16) {
    advance = 25 #Advance to Trafalgar Square
  } else if(v < 3/16) {
    advance = 12 #Advance to Pall Mall
  } else if(v < 4/16) {
    advance = 11 #Go directly to Jail
  } else if(v < 5/16) {
    advance = 16 #Trip to Marylebone Station
  } else if (v < 6/16) {
    advance = 40 #Advance to Mayfair
  }
  return(advance)
}
  #Implement into SimulateMonopoly1
SimulateMonopoly1 = function(no_of_rolls) {
  landings = numeric(40)
  #Start at GO
  current = 1
  for(i in 1:no_of_rolls) {
    current = current + RollTwoDice()
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

##Question 6
  # Edit current cc Function
CommunityChest = function(current) {
  goto = current
  u = runif(1)
  if(u < 1/16) {
    goto = 1 #Move to go
  } else if(u < 2/16) {
    goto = 11 #Go to Jail
  } else if(u < 3/16) {
    goto = 2 #Go to Old Kent Road
  } else if(u < 4/16) {
    goto = Chance(goto) #Take a Chance card instead
  }
  return(goto)
}

## Question 7
  # Edit Chance Function
Chance = function(current) {
  advance = current
  v = runif(1)
  if(v < 1/16) {
    advance = 1 #Advance to Go
  } else if(v < 2/16) {
    advance = 25 #Advance to Trafalgar Square
  } else if(v < 3/16) {
    advance = 12 #Advance to Pall Mall
  } else if(v < 4/16) {
    advance = 11 #Go directly to Jail
  } else if(v < 5/16) {
    advance = 16 #Trip to Marylebone Station
  } else if (v < 6/16) {
    advance = 40 #Advance to Mayfair
  } else if (v < 7/16) {
    if(advance <= 13 | advance > 29) {
      advance = 13
    } else if(advance > 13 & advance <= 29 ) {
      advance = 29
    }
  }
  return(advance)
}

## Question 8
  # Edit Chance Function
Chance = function(current) {
  advance = current
  v = runif(1)
  if(v < 1/16) {
    advance = 1 #Advance to Go
  } else if(v < 2/16) {
    advance = 25 #Advance to Trafalgar Square
  } else if(v < 3/16) {
    advance = 12 #Advance to Pall Mall
  } else if(v < 4/16) {
    advance = 11 #Go directly to Jail
  } else if(v < 5/16) {
    advance = 16 #Trip to Marylebone Station
  } else if (v < 6/16) {
    advance = 40 #Advance to Mayfair
  } else if (v < 7/16) {
    if(advance <= 13 | advance > 29) {
      advance = 13
    } else if(advance > 13 & advance <= 29 ) {
      advance = 29
    }
  } else if (v < 8/16) {
    if(advance <= 3) {
      advance = advance + 37
    } else if(advance==34) {
      advance = 11 #Go to Jail
    } else if(advance > 3 & advance <= 40) {
      advance = advance - 3
    }
  }
  return(advance)
}

# Question 9
  #Edit RollTwoDice Function
RollTwoDice2 = function() {
  x.1 = 0
  x.2 = 0
  x.1 = sample(1:6,1)
  x.2 = sample(1:6,1)
  total.1 = x.1 + x.2
  if(x.1 != x.2) {
    total_score = total.1
  } else if (x.1 == x.2) {
    x.3 = 0
    x.4 = 0
    x.3 = sample(1:6,1)
    x.4 = sample(1:6,1)
    total.2 = x.3 + x.4
    if(x.3 != x.4) {
      total_score = total.1 + total.2
    } else if(x.3 == x.4) {
      x.5 = 0
      x.6 = 0
      x.5 = sample(1:6,1)
      x.6 = sample(1:6,1)
      total.3 = x.5 + x.6
      if(x.5 != x.6) {
        total_score = total.1 + total.2 + total.3
      } else if(x.5 == x.6) {
        total_score = "Jail"
      }
    }
  }
  return(total_score)
}
# Final SimulateMonopoly1
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

## Plotting Function
no_of_rolls = 1000000
sim1 = SimulateMonopoly1(no_of_rolls)
plot(sim1/sum(sim1), ylim = range(0.01,0.06), xlab = "Property", ylab = "Probability", main = "Probability of landing on a square  Student ID - b2004466")
abline(h=0.025,col=2,lty=2)
