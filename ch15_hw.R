ch15_1 <- function( )
{
    an_info <- matrix(sample(10:100, 90, replace = T), ncol = 3)
    colnames(an_info) <- c("Tiger", "Lion", "Leopard")
    rownames(an_info) <- c(paste(rep("Day", times = 30), 1:30))
    print(an_info)
    apply(an_info, 2, function(x) c(max(x), min(x), round(mean(x))))
}

ch15_1()

deck


ch15_2 <- function( )
{
    cat("請輸入1~10內之數值資料，若想結束，請按Enter")
    x1 <- scan()
    x2 <- sample(1:10, 1, replace = T,c(1 ,2 , 3, 4, 5, 6, 7, 8, 9, 10))
    x3 <- "abc"
    if(x1 > x2)
    {
        x3 <- "Win"
    }else if (x1 == x2)
    {
        x3 <- "Flat"
    }else {
            x3 <- "Lose"
        }
    cat(x3)
}

ch15_2()

ch15_3 <- function( )
{
    x2 <- y
    repeat{
        x1 <- sample(1:6, 3, replace = T)
        cat("Dice game :", x1, "\n")
        cat("Play again? y/n")
        x2 <- scan(what = character())
        if(x2 == "n") break
    }
    print("Game over")
}
ch15_3()

ld.r.petal <- mean(iris$Petal.Length / iris$Petal.Width)
ld.r.sepal <- mean(iris$Sepal.Length / iris$Petal.Width)

ch15_4 <- cbind(ld.r.petal, ld.r.sepal)



ch15_5 <- function( )
{
    islands_2 <- as.data.frame(islands)
    label.islands <- cut(islands, 10, 
                         labels = c("Low", "9th", "8th", "7th", 
                                    "6th", "5th", "4th", "3rd", "2nd", "High"))
    islands_new <- cbind(as.character(label.islands), islands_2)
    dimnames(islands_new)[[2]] <- c("grade", "area")
    x <- order(islands, decreasing = T)
    y <- islands_new[x, ]
    print(y)
}

ch15_5()

ch15_6 <- function( )
{
    mystate.x77 <- as.data.frame(state.x77)
    mystate.x77$Name <- rownames(state.x77)
    rownames(mystate.x77) <- NULL
    mypopu.states <- mystate.x77[mystate.x77$Population < 5000, c("Name", "Population")]
    myincome.states <- mystate.x77[mystate.x77$Income < 5000, c("Name", "Income")]
    x <- merge(mypopu.states, myincome.states)
    print(x)
}
ch15_6()


ch15_7 <- function( )
{
    mystate.x77 <- as.data.frame(state.x77)
    mystate.x77$Name <- rownames(state.x77)
    rownames(mystate.x77) <- NULL
    mypopu.states <- mystate.x77[mystate.x77$Population < 5000, c("Name", "Population")]
    myincome.states <- mystate.x77[mystate.x77$Income < 5000, c("Name", "Income")]
    x <- merge(mypopu.states, myincome.states, all = T)
    print(x)  
}
ch15_7()


ch15_8 <- function( )
{
    mycar <- within(mtcars, 
                    cyl <- factor(cyl, levels = c(4, 6, 8),
                                  labels = c("四汽缸", "六汽缸", "八汽缸")))
    x <- with(mycar, tapply(hp, cyl, mean))
    print(x)
}
ch15_8()

ch15_9 <- function( )
{
    game <- c(paste(rep("G",times = 10), 1:10, sep = ""))
    site <- c("LA", "SF", "SF", "NYC", "LA", "TX", "TX", "SF", "NYC", "LA")
    Lin <- c(sample(10:20, 10, replace = T, set.seed(1)))
    Wang <- c(sample(10:25, 10, replace = T, set.seed(2)))
    Lu <- c(sample(15:25, 10, replace = T, set.seed(3)))
    Pete <- c(sample(9:18, 10, replace = T, set.seed(4)))
    Jordan <- c(sample(16:28, 10, replace = T, set.seed(5)))
    balls <- data.frame(game, site, Lin, Wang, Lu, Pete, Jordan)
    lballs <- melt(balls, id.var = c("game", "site"), 
                   variable.name = "name", value.name = "points")
    site.ball <- dcast(lballs, site ~ name, sum)
    game.site.ball <- dcast(lballs, game + site ~ name, sum)
    cat("Enter a for short form; b for long form;
        c for site~name; d for game + site ~ name")
    x <- scan(what = character())
    if(x == "a") print(balls)
    else if(x == "b") print(lballs)
    else if(x == "c") print(site.ball)
    else if(x == "d") print(game.site.ball)
    else print("Error")
}
ch15_9()
















