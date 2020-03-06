ch13_1 <- function()
{
  an_info <- matrix(round(rnorm(90,10,3)), ncol = 3)
  colnames(an_info) <- c("Tiger", "Lion", "Leopard")
  rownames(an_info) <- c(paste(rep("Day", times = 30), 1:30))
  print(an_info)
  apply(an_info, 2, function(x) c(max(x), min(x), round(mean(x))))
}
ch13_1()


ch13_2 <- function()
{
  an_info <- matrix(c(round(rnorm(87,10,3)),NA,NA,NA), ncol = 3, byrow = TRUE)
  colnames(an_info) <- c("Tiger", "Lion", "Leopard")
  rownames(an_info) <- c(paste(rep("Day", times = 30), 1:30))
  print(an_info)
  apply(an_info, 2, function(x) c(max(x,na.rm = TRUE), min(x,na.rm = TRUE), 
                                  round(mean(x,na.rm = TRUE))))
}

ch13_2()

ch13_3 <- function()
{
  sstr <- as.character(state.region)
  vec.pop <- state.x77[,1]
  vec.area <- state.x77[,8]
  vec.income <- state.x77[,2]
  x <- c(vec.area, vec.pop, vec.income)
  names(x) <- NULL
  Info <- matrix(c(vec.pop, vec.area, vec.income), ncol =  3)
  a.answer <- matrix(c(tapply(vec.pop, factor(sstr, levels = c("Northeast", "South", 
                                                   "North Central", "West")), sum),
                       tapply(vec.area, factor(sstr, levels = c("Northeast", "South", 
                                                    "North Central", "West")), sum),
                       tapply(vec.income, factor(sstr, levels = c("Northeast", "South", 
                                                    "North Central", "West")), mean)),
                     nrow = 3, byrow = TRUE)
  colnames(a.answer) <- c("Northeast", "South", "North Central", "West")
  rownames(a.answer) <- c("pop", "area", "meanIncome")
  print(a.answer)
}

ch13_3()







