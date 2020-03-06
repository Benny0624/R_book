ch14_1 <- function( )
{
    cat("Please enter your name, press /Enter/ to end")
    x1 <- scan(what = character())
    cat("Welcome~", x1)
}

ch14_1()


ch14_2 <- function( )
{
    cat("Please enter your name, press /Enter/ to end")
    x1 <- scan(what = character())
    cat("Welcome~", x1, file = "~/exer14_2.txt")
}

ch14_3 <- function()
{
    x1 <- scan("C:/Users/veryv/OneDrive/®à­±/Exercise.txt", sep = ",")
    cat("Sum=", sum(x1), "\n")
    cat("Mean=", mean(x1), "\n")
    cat("Max=", max(x1), "\n")
    cat("Min=", min(x1))
}
ch14_3()

ch14_4 <- function()
{
    x1 <- scan("C:/Users/veryv/OneDrive/®à­±/Exercise.txt", sep = ",")
    cat("Sum=", sum(x1),"\n","Mean=", mean(x1),"\n",
        "Max=", max(x1),"\n","Min=", min(x1),file = "~/exer14_3.txt")
}
ch14_4()


ch14_5 <- function( )
{
    excel <- file.path("C:/Users/veryv/OneDrive/®à­±/Test.csv")
    xCSV <- read.csv(excel, sep = ",")
    tea.info <- matrix(c(tapply(xCSV$Quantity, xCSV$Product, sum),
                         tapply(xCSV$Revenue, xCSV$Product, sum)),
                       nrow = 2, byrow = T)
    colnames(tea.info) <- c("BlackTea ", "Green Tea")
    rownames(tea.info) <- c("year_quant", "year_rev")
    Sales <- tapply(xCSV$Revenue, xCSV$Name, sum)
    answer <- list(TeaInfo = tea.info, RevBySales = Sales)
    print(answer)
}

ch14_5()













