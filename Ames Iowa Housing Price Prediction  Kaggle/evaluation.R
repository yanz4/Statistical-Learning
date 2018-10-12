pred <- read.csv("mysubmission3.txt")
test.y <- read.csv("price.txt")

dim(pred)
dim(test.y)
pred <- merge(pred, test.y, by="PID")
sqrt(mean((log(pred$Sale_Price) - log(pred$True_Sale_Price))^2))

