head(Cars93)
summary(Cars93)
str(Cars93)

Car.table <- table(Cars93$Type)

dev.new()
x.par <- par(mfcol = c(1,2))
barplot(sort(Car.table, decreasing = T), 
        xlab = "CarType", ylab = "Freq", col = "blue")
pie(sort(Car.table, decreasing = T),clockwise = T, main = 'CarType')
text(sort(Car.table, decreasing = T), 
     labels = paste(round(prop.table(Car.table) * 100, digits = 2), "%" ))
par(x.par)


dev.new()
with(Cars93, plot(MPG.city,MPG.highway, pch = 22, col = 'blue', bg = 'green'))
model.Cars <- lm(MPG.highway ~ MPG.city , data = Cars93)
model.values <- fitted(model.Cars)
lines(Cars93$MPG.city, model.values, col = 'red')











