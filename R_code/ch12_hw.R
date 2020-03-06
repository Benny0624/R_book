
mymax <- function(x)
{
  if(is.numeric(x) == FALSE) print("This is not a vector") 
  else{
  max_x <- x[1]
  for(i in seq_along(x)){
     if(x[i] >= max_x){
      max_x <- x[i]
    }else max_x <- max_x
  }
  print(max_x)
  }
}

mymin <- function(x)
  {
  if(is.numeric(x) == FALSE)
  {
    print("This is not a vector")
  }else {
  min_x <- x[1]
  for(i in seq_along(x)){
    if(x[i] <= min_x){
      min_x <- x[i]
    }else min_x <- min_x
  }
  print(min_x)
  }
}

myave <- function(x)
  {
  if(is.numeric(x) == FALSE){
    print("This is not a vector")
  }else{
    sum_x <- 0
    for (i in seq_along(x)) {
      sum_x <- sum_x + x[i]
    }
    ave_x <- sum_x / length(x)
    print(ave_x)
  }
}


mysort <- function(x)
{
  if(is.numeric(x) == FALSE){
    print("This is not a vector")
  }else{
    sorter <- integer(length(x))
    counter <- length(x)
    repeat{
      max_x<-max(x)
      if (sum(x==max_x)>1){
        sorter[counter:(counter-sum(x==max_x)+1)] <- max_x
      }else{
        sorter[counter] <- max_x
      }
      counter <- counter - sum(x==max_x)
      if(counter <= 1) break
      x <- x[-grep(max(x),x)]
    }
    return(sorter)
  }
}

elec_fee <- function(deg, customer, unitPrice = 100)
{
  coefficent <- numeric(0)
  for(i in deg){
  if(i > 300) {index <- 1}
    else if(i > 100 & i <= 300){index <- 2}
    else{index <- 3}
  coefficent <- c(coefficent, switch(index,0.8,0.9,1))
  }
  net.price <- deg * unitPrice * coefficent
  adj <- numeric(0)
  for(j in customer){
    adj <- c(adj, switch(j, government = 0.7, poor = 0.5, 1))
  }
  finalPrice <- net.price * adj
  round(finalPrice)
}

region_num <- function(n)
{
  Northeast <- 0
  South <- 0
  North.Central <- 0
  West <- 0
  for (i in n) {
    if(i == "Northeast")  Northeast <-  Northeast + 1
    else if(i == "South")  South <- South + 1
    else if(i == "North Central")  North.Central <- North.Central + 1
    else West <- West + 1
  }
  region.count <- c(Northeast, South, North.Central, West)
  names(region.count) <- c("Northeast", "South", "North.Central", "West")
  print(region.count)
}

state.x78 <- cbind(state.x77,state.region)
state.pop <- state.x78[,c(1,9)]
head(state.pop)

region_pop <- c(sum(state.pop[state.pop[,2] == 1,"Population"]),
                sum(state.pop[state.pop[,2] == 2,"Population"]),
                sum(state.pop[state.pop[,2] == 3,"Population"]),
                sum(state.pop[state.pop[,2] == 4,"Population"]))
names(region_pop) <- c("Northeast", "South", "North.Central", "West")
region_pop

state.area <- state.x78[,c(8,9)]
head(state.area)

region_area <- c(sum(state.area[state.area[,2] == 1,"Area"]),
                 sum(state.area[state.area[,2] == 2,"Area"]),
                 sum(state.area[state.area[,2] == 3,"Area"]),
                 sum(state.area[state.area[,2] == 4,"Area"]))
names(region_area) <- c("Northeast", "South", "North.Central", "West")
region_area

state.income <- state.x78[,c(2,9)]
head(state.income)

region_aveincome <- c(mean(state.income[state.income[,2] == 1,"Income"]),
                      mean(state.income[state.income[,2] == 2,"Income"]),
                      mean(state.income[state.income[,2] == 3,"Income"]),
                      mean(state.income[state.income[,2] == 4,"Income"]))
names(region_aveincome) <- c("Northeast", "South", "North.Central", "West")
region_aveincome

