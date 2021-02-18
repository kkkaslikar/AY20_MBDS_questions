
# imports -----------------------------------------------------------------
library(stringr)

library(purrr)



# setup -------------------------------------------------------------------

r <- 90000

c <- 100000

orig.distance <- 87127231192

n.d.step <- r - 1

orig.n.r.step <- c - 1

n.r.step <- orig.n.r.step

d.step.sum <- r*(r+1)/2

distance <- orig.distance - d.step.sum
 
if((distance %/% r) > n.r.step){
  print("Summed number cannot be reached within the available total number of operations.")
}

idx <- vector(mode = "numeric", length = r)

r.step.options <- 1:r

r.step.options <- sort(r.step.options, decreasing = TRUE)


# loop --------------------------------------------------------------------

for(k in seq_along(r.step.options)){
  
  r.step.distance <- r.step.options[k]
  
  steps.taken <- distance %/% r.step.distance
  
  distance.covered <- r.step.distance*steps.taken
  
  if(distance.covered == distance){
    print("distance covered = distance")
    print(paste("r.step.distance", r.step.distance))
    print(paste("distance", distance))
  }
  
  while(steps.taken > 0){
    
    distance.covered <- r.step.distance*steps.taken
    
    remaining.distance <- distance - distance.covered
    
    remaining.n.r.steps <-n.r.step - steps.taken
    
    # if(remaining.n.r.steps < 0)
    # {
    #   print(r.step.options[k])
    #   print(steps.taken)}
    
    if (remaining.distance >= remaining.n.r.steps){
      break
    } else {
      steps.taken <- steps.taken - 1
    }
  }
  
  idx[r.step.distance] <- steps.taken
  
  if(remaining.distance > remaining.n.r.steps){
  #   print(paste("r step distance", r.step.distance))
  #   print(paste("r step distance", remaining.distance))
  #   print(paste("remaning number of steps", remaining.n.r.steps))
    
    distance <- remaining.distance
    
    n.r.step <- remaining.n.r.steps
    
  }else if(remaining.distance == remaining.n.r.steps){
    
    print("remaining distance = remaining number of steps")
    print(remaining.distance)
    print(remaining.n.r.steps)
    
    distance <- remaining.distance
    
    n.r.step <- remaining.n.r.steps
    
    r.step.distance <- 1
    
    steps.taken <- distance %/% r.step.distance
    
    distance.covered <- r.step.distance*steps.taken
    
    remaining.distance <- distance - distance.covered
    
    remaining.n.r.steps <-n.r.step - steps.taken
    
    distance <- remaining.distance
    
    n.r.step <- remaining.n.r.steps
    
    idx[r.step.distance] <- steps.taken
  }
  if(n.r.step == 0 & distance == 0 & sum(idx) == (c-1)){
    print("Done")
    break
  }
}

which(idx !=0)


# step 1 ------------------------------------------------------------------

r.step.distance <- r.step.options[which(r.step.options == 90000)]

steps.taken <- distance %/% r.step.distance

distance.covered <- r.step.distance*steps.taken

if(distance.covered == distance){
  print("distance covered = distance")
  print(paste("r.step.distance", r.step.distance))
  print(paste("distance", distance))
}

while(steps.taken > 0){
  
  distance.covered <- r.step.distance*steps.taken
  
  remaining.distance <- distance - distance.covered
  
  remaining.n.r.steps <-n.r.step - steps.taken
  
  if(remaining.n.r.steps < 0)
  {
    print(r.step.options[k])
    print(steps.taken)}
  
  if (remaining.distance >= remaining.n.r.steps){
    break
  } else {
    steps.taken <- (distance %/% r.step.distance - 1)
  }
}

remaining.distance

remaining.n.r.steps

idx[r.step.distance] <- steps.taken

if(remaining.distance > remaining.n.r.steps){
  print(paste("r step distance", r.step.distance))
  print(paste("r step distance", remaining.distance))
  print(paste("remaning number of steps", remaining.n.r.steps))
  
  distance <- remaining.distance
  
  n.r.step <- remaining.n.r.steps
  
}else if(remaining.distance == remaining.n.r.steps){
  
  print("remaining distance = remaining number of steps")
  print(remaining.distance)
  print(remaining.n.r.steps)
  
  distance <- remaining.distance
  
  n.r.step <- remaining.n.r.steps
  
  r.step.distance <- 1
  
  steps.taken <- distance %/% r.step.distance
  
  distance.covered <- r.step.distance*steps.taken
  
  remaining.distance <- distance - distance.covered
  
  remaining.n.r.steps <-n.r.step - steps.taken
  
  distance <- remaining.distance
  
  n.r.step <- remaining.n.r.steps
  
  idx[r.step.distance] <- steps.taken
}

if(n.r.step == 0 & distance == 0 & sum(idx) == (c-1)){
  print("Done")
  break
}


# step 2 ------------------------------------------------------------------

r.step.distance <- r.step.options[which(r.step.options == 63341)]

steps.taken <- distance %/% r.step.distance

distance.covered <- r.step.distance*steps.taken

if(distance.covered == distance){
  print("distance covered = distance")
  print(paste("r.step.distance", r.step.distance))
  print(paste("distance", distance))
}

while(steps.taken > 0){
  
  distance.covered <- r.step.distance*steps.taken
  
  remaining.distance <- distance - distance.covered
  
  remaining.n.r.steps <-n.r.step - steps.taken
  
  if(remaining.n.r.steps < 0)
  {
    print(r.step.options[k])
    print(steps.taken)}
  
  if (remaining.distance >= remaining.n.r.steps){
    break
  } else {
    steps.taken <- steps.taken - 1
  }
}

remaining.distance

remaining.n.r.steps

idx[r.step.distance] <- steps.taken

if(remaining.distance > remaining.n.r.steps){
  print(paste("r step distance", r.step.distance))
  print(paste("r step distance", remaining.distance))
  print(paste("remaning number of steps", remaining.n.r.steps))
  
  distance <- remaining.distance
  
  n.r.step <- remaining.n.r.steps
  
}else if(remaining.distance == remaining.n.r.steps){
  
  print("remaining distance = remaining number of steps")
  print(remaining.distance)
  print(remaining.n.r.steps)
  
  distance <- remaining.distance
  
  n.r.step <- remaining.n.r.steps
  
  r.step.distance <- 1
  
  steps.taken <- distance %/% r.step.distance
  
  distance.covered <- r.step.distance*steps.taken
  
  remaining.distance <- distance - distance.covered
  
  remaining.n.r.steps <-n.r.step - steps.taken
  
  distance <- remaining.distance
  
  n.r.step <- remaining.n.r.steps
  
  idx[r.step.distance] <- steps.taken
}

if(n.r.step == 0 & distance == 0 & sum(idx) == (c-1)){
  print("Done")
  break
}




# spare code --------------------------------------------------------------




for(k in seq_along(r.step.options)){
  
  r.step.distance <- r.step.options[k]
  
  steps.taken <- distance %/% r.step.distance
  
  distance.covered <- r.step.distance*steps.taken
  
  while(steps.taken > 0){
    
    distance.covered <- r.step.distance*steps.taken
    
    remaining.distance <- distance - distance.covered
    
    remaining.n.r.steps <-n.r.step - steps.taken
    
    # if(remaining.n.r.steps <= 0)
    # {print(r.step.options[k])
    #   print(steps.taken)}
    
    if (remaining.distance >= remaining.n.r.steps){
      print(r.step.distance)
      print(remaining.distance)
      print(remaining.n.r.steps)
      break
    } else {
      steps.taken <- (distance %/% r.step.distance - 1)
    }
  }
  
  idx[r.step.distance] <- steps.taken
  
  if(remaining.distance > remaining.n.r.steps){
    
    distance <- remaining.distance
    
    n.r.step <- remaining.n.r.steps
  }
  if(n.r.step == 0 & distance == 0 & sum(idx) == (c-1)){
    print("Done")
    break
  }
}



# more spare code ---------------------------------------------------------

r.step.distance <- r.step.options[1]

steps.taken <- distance %/% r.step.distance

distance.covered <- r.step.distance*steps.taken

while(steps.taken > 0){
  
  distance.covered <- r.step.distance*steps.taken
  
  remaining.distance <- distance - distance.covered
  
  remaining.n.r.steps <-n.r.step - steps.taken
  
  if (remaining.distance >= remaining.n.r.steps){
    break
  } else {
    steps.taken <- (distance %/% r.step.distance - 1)
  }
}

idx[r.step.distance] <- steps.taken

if(remaining.distance > remaining.n.r.steps){
  distance <- remaining.distance
  
  n.r.step <- remaining.n.r.steps
}else if(remaining.distance == remaining.n.r.steps){
  
  distance <- remaining.distance
  
  n.r.step <- remaining.n.r.steps
  
  r.step.distance <- 1
  
  steps.taken <- distance %/% r.step.distance
  
  distance.covered <- r.step.distance*steps.taken
  
  remaining.distance <- distance - distance.covered
  
  remaining.n.r.steps <-n.r.step - steps.taken
  
  distance <- remaining.distance
  
  n.r.step <- remaining.n.r.steps
  
  idx[r.step.distance] <- steps.taken
}

distance

n.r.step


# string_building ---------------------------------------------------------

sum(idx) == (c-1)

orig.distance - sum((1:length(idx))*idx) == d.step.sum

# step.string <- vector(mode = "character", length = 0)

# for(i in seq_along(idx)){
#   if(i < length(idx)){
#     string.segment <- c(rep("R", idx[i]), "D")
#     step.string <- append(step.string, string.segment)
#   } else {
#     string.segment <- c(rep("R", idx[i]))
#     step.string <- append(step.string, string.segment)
#   }
# }

all.segments <- purrr::map(.x = seq_along(idx), .f= ~{
  if(.x < length(idx)){
    string.segment <- c(rep("R", idx[.x]), "D")
    return(string.segment)
  } else {
    string.segment <- c(rep("R", idx[.x]))
    return(string.segment)
  }
})

all.segments <- purrr::map(.x = all.segments, .f = ~paste0(.x, collapse = ""))

final_string <- do.call(what = str_c, args = all.segments)

print(final_string)
str_length(final_string)
