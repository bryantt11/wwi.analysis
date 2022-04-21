#--------------WWI PROJECT------------------

# Required libraries
library(dplyr)
library(ggplot2)
library(dummies)
library(corrplot)
library(caret)
library(stats)
library(forecast)
library(gains)
library(cluster)
library(factoextra)
library(purrr)
library(nnet)
library(neuralnet)
library(NeuralNetTools)
library(OneR)
options(scipen=999)

setwd("~/Sales")

# Load dataset 
sales_file <- read.csv("data/sales_file.csv")
sales.df <- data.frame(sales_file)
head(sales.df)   

# Write file to RDS
saveRDS(sales.df, "sales_file.rds")

# Determine the Suppliers in the dataset
unique(sales.df$Supplier)

#-------------EXPLORATORY ANALYSIS-------------

# Review data strucure and summary statistics
str(sales.df)
summary(sales.df)


## Are sales increasing or decreasing by year?
sales.df %>%
  group_by(Year) %>%
  summarize(
    Total.Sales = sum(sort(Total.Excluding.Tax)))%>%
  arrange(desc(Year))

ggplot(data=sales.df, mapping=aes(x=Year))+
  geom_bar()+
  labs(
    x = "Year",
    y = "No. of Sales"
  )

## What are the top 5 sales territories?
sales.df %>%
  group_by(Sales.Territory) %>%
  summarize(
    Total.Sales = sum(sort(Total.Excluding.Tax)))%>%
  arrange(desc(Total.Sales))

ggplot(data=sales.df, mapping=aes(x=Sales.Territory, fill=Sales.Territory))+
  geom_bar()+
  labs(
    x = "Employee",
    y = "No. of Sales"
  ) +
  coord_flip()

## Who are the top 5 salespeople for WWI?
sales.df %>%
  group_by(Employee) %>%
  summarize(
    Total.SalesSP= sum(sort(Total.Excluding.Tax)))%>%
  arrange(desc(Total.SalesSP))

ggplot(data=sales.df, mapping=aes(x=Employee, fill=Employee))+
  geom_bar()+
  geom_label(stat="count", mapping=aes(label=..count.., fill=Employee))+
  labs(
    x = "Employee",
    y = "No. of Sales"
  ) +
  coord_flip()

## Which sales territories are the most profitable? 
sales.df %>%
  group_by(Sales.Territory) %>%
  summarize(
    Total.Profits = sum(sort(Profit)))%>%
  arrange(desc(Total.Profits))

histogram(sales.df$Profit)

## Who are the top suppliers?
sales.df %>%
  group_by(Supplier) %>%
  summarize(
    Total.Supplied = sum(sort(Total.Excluding.Tax)))%>%
  arrange(desc(Total.Supplied))

ggplot(data=sales.df, mapping=aes(x=Supplier, fill=Supplier))+
  geom_bar()+
  geom_label(stat="count", mapping=aes(label=..count.., fill=Supplier))+
  labs(
    x = "Supplier",
    y = "No. of Products Sold"
  ) +
  coord_flip()

## What are are the top products sold by unit price? 
sales.df %>%
  group_by(Stock.Item.Key) %>%
  summarize(
    Total.Unit.Price = sum(sort(Unit.Price)))%>%
  arrange(desc(Total.Unit.Price))

## What kind of products are commonly sold?
sales.df %>%
  group_by(Category) %>%
  summarize(
    Total.Unit.Price = sum(sort(Unit.Price)))%>%
  arrange(desc(Total.Unit.Price))

## What sales territories are the 3 least profitable, and what is the costs 
## associated with doing business in these territories?
sales.df %>%
  group_by(Sales.Territory) %>%
  summarize(
    Total.Stock.Profit = sum(sort(Profit)),
    Total.Cost = sum(Calc.Unit.Cost))%>%
  arrange(Total.Cost)

## Which of WWI’s suppliers are associated with the most profits? Why are the 
##  ones that do not have profitability performing poorly?
sales.df %>%
  group_by(Supplier) %>%
  summarize(
    Total.Stock.Profit = sum(sort(Profit)),
    Total.Cost = sum(Calc.Unit.Cost),
    Cost.Percent = (Total.Cost/Total.Stock.Profit)*100)%>%
  arrange(desc(Total.Stock.Profit))

# What is ROI on products purchased by WWI to sale?
Total.Profit <- sum(sales.df$Profit)
Total.Unit.Cost <- sum(sales.df$Calc.Unit.Cost)
Total.Tax <- sum(sales.df$Tax.Rate)
Total.Cost <- Total.Unit.Cost+Total.Tax
Total.ROI <- ((Total.Profit-Total.Cost)/Total.Cost)*100

print(paste("The ROI for WWI is: ", round(Total.ROI), "%"))


# Convert to numeric for correlation plot and confirm results
corr.sales <- select(sales.df, 1, 2, 5, 6, 7, 8, 9, 10, 11, 
                     12, 13, 14, 15, 16, 17, 18, 19)
str(corr.sales)

# Structure data
corr.sales$Is.Order.Finalized <- as.factor(corr.sales$Is.Order.Finalized)
corr.sales$Is.Order.Finalized <- as.numeric(corr.sales$Is.Order.Finalized)
corr.sales$Sales.Territory <- as.factor(corr.sales$Sales.Territory)
corr.sales$Sales.Territory <- as.numeric(corr.sales$Sales.Territory)
corr.sales$Employee <- as.factor(corr.sales$Employee)
corr.sales$Employee <- as.numeric(corr.sales$Employee)
corr.sales$Category <- as.factor(corr.sales$Category)
corr.sales$Category <- as.numeric(corr.sales$Category)
corr.sales$Supplier <- as.factor(corr.sales$Supplier)
corr.sales$Supplier <- as.numeric(corr.sales$Supplier)

str(corr.sales)

# Correlation plot
corrplot(cor(corr.sales))
cor(corr.sales)


#---------------------LINEAR REGRESSION---------------------
## Based on the historical data previously analyzed, what can be said about 
## future profitability? Costs?

# Duplicate dataset and review for any N/A's
model.sales <- na.omit(sales.df)


# Convert data types from numeric for analysis
model.sales$Sale.Key<- as.numeric(model.sales$Sale.Key)
model.sales$Month<- as.numeric(model.sales$Month)
model.sales$Day<- as.numeric(model.sales$Day)
model.sales$Year <- as.numeric(model.sales$Year)
model.sales$WWI.Customer.ID<- as.numeric(model.sales$WWI.Customer.ID)
model.sales$Stock.Item.Key<- as.numeric(model.sales$Stock.Item.Key)
model.sales$Quantity<- as.numeric(model.sales$Quantity)
model.sales$Tax.Rate<- as.numeric(model.sales$Tax.Rate)
model.sales$Is.Order.Finalized <- as.factor(model.sales$Is.Order.Finalized)
model.sales$Is.Order.Finalized <- as.numeric(model.sales$Is.Order.Finalized)
model.sales$Sales.Territory <- as.factor(model.sales$Sales.Territory)
model.sales$Sales.Territory <- as.numeric(model.sales$Sales.Territory)
model.sales$Employee <- as.factor(model.sales$Employee)
model.sales$Employee <- as.numeric(model.sales$Employee)
model.sales$Category <- as.factor(model.sales$Category)
model.sales$Category <- as.numeric(model.sales$Category)
model.sales$Supplier <- as.factor(model.sales$Supplier)
model.sales$Supplier <- as.numeric(model.sales$Supplier)
str(model.sales)

# Select Data to reduce multicollinearity
model.sales.df <- subset(model.sales, select=-c(3,4))
model.sales.df.norm <- data.frame(scale(model.sales.df))
                             
str(model.sales.df)


# Original linear model
fit.lm <- lm(Profit~., model.sales.df.norm)
summary(fit.lm)
print(fit.lm)

ggplot(model.sales.df.norm, aes(x = Profit, y = Calc.Unit.Cost)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

#---------------------STEPWISE REGRESSION---------------------
# partition data
set.seed (1) #set seed for reproducing the partition
train.index1 <- sample(1:nrow(model.sales.df.norm), 0.6 * nrow(model.sales.df.norm))
train.df <- model.sales.df.norm[train.index, ]
valid.df <- model.sales.df.norm[-train.index, ]

# Write new files for train/test sets - 
saveRDS(train.df, "stepwise_train.rds")
saveRDS(valid.df, "stepwise_valid.rds")

# use step() to run backward regression.
sales.lm.step.back <- step(fit.lm, direction = "backward")
summary(sales.lm.step.back) 
sales.lm.step.back.pred <- predict(sales.lm.step.back, valid.df)
accuracy(sales.lm.step.back.pred, valid.df$Profit)

# use step() to run forward regression.
sales.lm.null <- lm(Profit~1, data = train.df)
sales.lm.step.forward <- step(sales.lm.null, 
                               scope=list(lower=sales.lm.null, upper=fit.lm), 
                               direction = "forward")
summary(sales.lm.step.forward)
sales.lm.step.forward.pred <- predict(sales.lm.step.forward, valid.df)
accuracy(sales.lm.step.forward.pred, valid.df$Profit)

# use step() to run both
sales.lm.step <- step(fit.lm, direction = "both")
summary(sales.lm.step)
sales.lm.step.pred <- predict(sales.lm.step, valid.df)
accuracy(sales.lm.step.pred, valid.df$Profit)

# dataframe creation to compare results 
comparison <- data.frame(
  Backward=c(accuracy(sales.lm.step.back.pred, valid.df$Profit)),
  Forward= c(accuracy(sales.lm.step.forward.pred, valid.df$Profit)),
  Step=c(accuracy(sales.lm.step.pred, valid.df$Profit))
)
comparison

# Lift charts
actual <- valid.df$Profit
gainBack<- gains(actual, sales.lm.step.back.pred, groups=10)
gainFwd <- gains(actual, sales.lm.step.forward.pred, groups=10)
gainStep <- gains(actual, sales.lm.step.pred, groups=10)

# lift chart - backward
plot(c(0, gainBack$cume.pct.of.total*sum(actual)) ~ c(0, gainBack$cume.obs), 
     xlab = "# cases", ylab = "Cumulative Profit", type="l", 
     main="Lift Chart - Backward")
lines(c(0,sum(actual))~c(0,dim(valid.df)[1]), col="red", lty=2)

# lift chart - forward
plot(c(0, gainFwd$cume.pct.of.total*sum(actual)) ~ c(0, gainFwd$cume.obs), 
     xlab = "# cases", ylab = "Cumulative Profit", type="l", 
     main="Lift Chart - Forward")
lines(c(0,sum(actual))~c(0,dim(valid.df)[1]), col="red", lty=2)

# lift chart - step
plot(c(0, gainStep$cume.pct.of.total*sum(actual)) ~ c(0, gainStep$cume.obs), 
     xlab = "# cases", ylab = "Cumulative Profit", type="l", 
     main="Lift Chart - Step")
lines(c(0,sum(actual))~c(0,dim(valid.df)[1]), col="red", lty=2)

#---------------------K MEANS---------------------
# Partition the dataset into 60% training and 40% validation sets 
model.sales.df2 <- scale(model.sales.df)

# Determine best K using elbow method
set.seed(222)

# Function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(model.sales.df2[, -c(1,2,3,4,5,6,7,8,9,10,17)], 
         k, nstart = 10, algorithm="Lloyd", iter.max = 100000)$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

#  Extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# Compute K Means with a k=6
set.seed(123)
final <- kmeans(model.sales.df2[, -c(1,2,3,4,5,6,7,8,9,10,17)], 6, nstart = 25)
table(final$cluster)

# Cluster plot
fviz_cluster(final, data = model.sales.df2[, -c(1,2,3,4,5,6,7,8,9,10,17)])

#---------------------NEURAL NETWORK---------------------
# Fit the neural network model
model.sales.df3<- data.frame(model.sales)
str(model.sales.df3)

# Divide training (60%) and test set (40%)
set.seed(321) 
train.index <- sample(1:nrow(model.sales.df3), 0.6 * nrow(model.sales.df3))
train.nn <- model.sales.df3[train.index, ]
valid.nn <- model.sales.df3[-train.index, ]

# Write new files for train/test sets - 
saveRDS(train.nn, "nnet_train.rds")
saveRDS(valid.nn, "nnet_valid.rds")

# Train NN on training set and plot
nn.sales1 <- nnet(Profit~Sale.Key+
                        Month+
                        Stock.Item.Key+
                        Quantity+
                        Tax.Rate+
                        Calc.Unit.Cost, train.nn, size=15, linout=T)
summary(nn.sales1)
plotnet(nn.sales1, bias = FALSE)

# Train NN on test set and plot
nn.sales2 <- nnet(Profit~Sale.Key+
                   Month+
                   Stock.Item.Key+
                   Quantity+
                   Tax.Rate+
                   Calc.Unit.Cost, valid.nn, size=15, linout=T)
summary(nn.sales2)
plotnet(nn.sales2, bias = FALSE)

# RMSE - error on training set
sales.pred1 <- predict(nn.sales1, train.nn)
rmse1 <- sqrt(mean(sales.pred1^2))
print(rmse1)

# RMSE - error on training set
sales.pred2 <- predict(nn.sales2, valid.nn)
rmse2 <- sqrt(mean(sales.pred2^2))
print(rmse2)
