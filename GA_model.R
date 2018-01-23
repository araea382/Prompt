setwd("D:/lynn/Agency Tracking/GA")
library(nnet)
library(dplyr)

ga <- read.csv("GA_data.csv")
ga$Postal_Code <- as.character(ga$Postal_Code)

train_set <- dplyr::filter(ga, GA_AIA != '0')
test_set <- dplyr::filter(ga, GA_AIA == '0')

train_set2 <- train_set[,-1]

library(rpart)
rt <- rpart(GA_AIA~., data=train_set2)

plot(rt)
text(rt)
