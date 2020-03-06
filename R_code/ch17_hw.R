head(crabs)
histogram(crabs$FL)
histogram(~ FL | sex, data = crabs)

qqnorm(crabs$FL)
qqline(crabs$FL)
FLtest <- shapiro.test(crabs$FL)
FLtest2 <- with(crabs, tapply(FL, sex, shapiro.test))
FLtest$p.value
FLtest2$F$p.value
FLtest2$M$p.value
