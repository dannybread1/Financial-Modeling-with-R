# Daniel Carr
# 2/22/2025

# General use Functions

# rm(list = ls()) #Clears the environment.
# cat("\014") #Clears console

# Function to calculate 1st order sensitivity 
calcSensitivity1 <- function(args, val, fxn, inc = 0.001){
  args_up <- args
  args_up[val] <- args_up[val] + inc
  
  args_down <- args
  args_down[val] <- args_down[val] - inc
  
  up <- do.call(what = fxn, args = as.list(args_up))
  down <- do.call(what = fxn, args = as.list(args_down))
  
  return((up - down)/(2*inc))
}

# Function to calculate 2nd order sensitivity
calcSensitivity2 <- function(args, val, fxn, inc = 0.001){
  args_up <- args
  args_up[val] <- args_up[val] + inc
  
  args_down <- args
  args_down[val] <- args_down[val] - inc
  
  up <- do.call(what = calcSensitivity1, args = list(args_up, val, fxn, inc))
  down <- do.call(what = calcSensitivity1, args = list(args_down, val, fxn, inc))
  
  return((up - down)/(2*inc))
}

fxnGoalSeek <- function(args, val, fxn, goal, precision = 0.001){
  # Function to goal-seek a particular numerical value by changing 1 variable in a function.
  # I'm sure there's a much better way to do this, but I'll worry about that later.
  
  # This doesn't work for an initial input of 0
  if (args[val] == 0) {args[val] <- 1}
  
  y <- do.call(what = fxn, args = as.list(args))
  args_up <- args
  args_down <- args
  i <- 1
  mag <- round(log(args[val], base = 10)) #Magnitude of shift
  
  while ((abs(y - goal) > precision) & i < 10000) {
    
    # Calculating function output for upward and downward shift in values.
    args_up[val] <- args[val] + 10^mag
    args_down[val] <- args[val] - 10^mag
    y_up <- do.call(what = fxn, args = as.list(args_up))
    y_down <- do.call(what = fxn, args = as.list(args_down))
    if (abs(y - goal) > abs(y_up - goal)) {
      args[val] <- args_up[val]
      y <- y_up
    }else{
      if (abs(y - goal) > abs(y_down - goal)) {
        args[val] <- args_down[val]
        y <- y_down
      }else{
        mag <- mag - 1
      }
    }
    i <- i+1
  }
  
  return(args[val])
}
