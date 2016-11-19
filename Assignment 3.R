## Reema Dmello
## NUID: 001613373
##Assignment 3 
## Linear Regression
#
# install.packages("XLConnect")
library(XLConnect)
concrete <- readWorksheetFromFile("D:/NEU Course Materials/Big Data Analytics/Assignment 3/Concrete_Data.xls", sheet=1)
#70% of the sample size
sample_size <- floor(0.70 * nrow(concrete))

#Set the seed 
set.seed(123)
train_ind <- sample(seq_len(nrow(concrete)), size = sample_size)

#Split the data into training and testing
train <- concrete[train_ind, ]
test <- concrete[-train_ind, ]

#head(train)

#build the model
mod2 <- lm(Concrete.compressive.strength.MPa..megapascals.. ~
              Blast.Furnace.Slag..component.2..kg.in.a.m.3.mixture.
            + Cement..component.1..kg.in.a.m.3.mixture.
            + Fly.Ash..component.3..kg.in.a.m.3.mixture.
           + Water...component.4..kg.in.a.m.3.mixture.
           + Superplasticizer..component.5..kg.in.a.m.3.mixture.
          #+ Coarse.Aggregate...component.6..kg.in.a.m.3.mixture.
          # + Fine.Aggregate..component.7..kg.in.a.m.3.mixture.
           + Age..day. , data=train)
summary(mod2)
mod2$coefficients    #check the values of the coefficients

# plotting the model - residuals vs fitted values, standardized values etc
plot(mod2)

#install.packages("forecast")
library("forecast")

# prediction on the training data 
predtrain = predict(mod2,train)

#prediction on the test data 
predtest = predict(mod2, test)
# length(predtest)

#accuracy on train data
accuracy(predtrain, train$Concrete.compressive.strength)
#accuracy on test data 
accuracy(predtest, test$Concrete.compressive.strength)

#evaluating model performance using Correlation 
correlation <- cor.test(test$Concrete.compressive.strength.MPa..megapascals.., predtest)
correlation$estimate

# writing output of true values and predicted values to a csv file
write.csv(data.frame(predict(mod2, test)), "D:/NEU Course Materials/Big Data Analytics/Assignment 3/output.csv")
write.csv(data.frame(test), "D:/NEU Course Materials/Big Data Analytics/Assignment 3/test.csv")

# summary(predtest)

#plot the graph 
plot(test$Concrete.compressive.strength.MPa..megapascals.., predtest,
     main="Correlation")


# additionally calculating the models accuracy rate on test data 

actuals_preds <- data.frame(cbind(actuals=test$Concrete.compressive.strength.MPa..megapascals.., predicteds=predtest))  # make actuals_predicteds dataframe.

actuals_preds

correlation_accuracy <- cor(actuals_preds) 

min_max_accuracy <- mean (apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))

correlation_accuracy
min_max_accuracy


