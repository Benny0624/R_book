ch16_1 <- function(){
    x <- rnorm(100, 60, 12)
    x.tb  <- table(x)
    y <- summary(x)
    yx <- as.vector(y)
    if(identical(unique(x),x) == T) {
        mode.x <- NA} else {
        mode.x <- which.max(x.tb)}
    var.x <- var(x)
    sd.x <- sd(x)
    range.x <- max(x) - min(x)
    x.info <- as.vector(cbind(mode.x, var.x, sd.x, range.x))
    x_summary <- matrix(c(yx, x.info), nrow = 1)
    colnames(x_summary) <- c("Min." ,"1st Qu." ,"Median" ,"Mean" ,"3rd Qu." ,"Max." 
                          ,"Mode", "Var", "Sd", "Range")
    rownames(x_summary) <- "freq"
    print(x_summary)
}


ch16_2 <- function()
{
    x <- rnorm(100, 60, 12)
    x.hist <- hist(x, freq = F)
    lines(density(x))
    print(x.hist)
}


ch16_3 <- function()
{
    x <- rnorm(100, 60, 12)
    y <- summary(x)
    z <- boxplot(x, col = "Green")
    print(y)
}


ch16_4_1 <- function()
{
    x <- rchisq(100, df = 8)
    x.tb  <- table(x)
    y <- summary(x)
    yx <- as.vector(y)
    if(identical(unique(x),x) == T) {
        mode.x <- NA} else {
            mode.x <- which.max(x.tb)}
    var.x <- var(x)
    sd.x <- sd(x)
    range.x <- max(x) - min(x)
    x.info <- as.vector(cbind(mode.x, var.x, sd.x, range.x))
    x_summary <- matrix(c(yx, x.info), nrow = 1)
    colnames(x_summary) <- c("Min." ,"1st Qu." ,"Median" ,"Mean" ,"3rd Qu." ,"Max." 
                             ,"Mode", "Var", "Sd", "Range")
    rownames(x_summary) <- "freq"
    print(x_summary)
}

ch16_4_2 <- function()
{
    x <- rchisq(100, df = 8)
    x.hist <- hist(x, freq = F)
    lines(density(x))
    print(x.hist)
}

ch16_4_3 <- function()
{
    x <- rchisq(100, df = 8)
    y <- summary(x)
    z <- boxplot(x, col = "Green")
    print(y)
}

ch16_5 <- function()
{
    x <- sample(rnorm(100, 60, 12), 100)
    y <- rchisq(100, df = 8)
    z <- cor(x, y)
    print(z)
}

ch16_5()














