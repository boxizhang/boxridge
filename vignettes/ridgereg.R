## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(mlbench)
library(caret)
library(mlr)
data("BostonHousing")

## ------------------------------------------------------------------------
head(BostonHousing)

## ------------------------------------------------------------------------
set.seed(12345)
inTrain <- createDataPartition(BostonHousing$medv, p = .8, list = FALSE)
training <- BostonHousing[ inTrain,]
testing  <- BostonHousing[-inTrain,]

#For the 10-fold cross-validation
ctrl <- trainControl(method = "repeatedcv",
  number = 10, # k=10
  repeats = 10) # repeat 10 times

## ------------------------------------------------------------------------
set.seed(12345)
lm <- caret::train(medv ~ ., 
            data = training,
            method = "lm",
            trControl = ctrl,
            preProc = "scale")
lm

## ------------------------------------------------------------------------
set.seed(12345)
lflmGrid <- expand.grid(nvmax=1:(ncol(training)-1))
lflm <- caret::train(medv ~ ., 
              data = training,
              method = "leapForward",
              trControl = ctrl,
              tuneGrid = lflmGrid,
              preProc = "scale")

lflm

summary(lflm)

## ---- fig.height= 7, fig.width= 7----------------------------------------
p_data <- as.data.frame(cbind(predict(lflm), scale(resid(lflm))))

plot1 <- ggplot(p_data, aes(x = p_data[,1], y = p_data[,2])) + geom_point() +
          geom_smooth(method = "loess", color = "red") + xlab("Fitted") +
          ylab("Residuals") + ggtitle("Resid vs Fitted with 10 factors") +
          theme(plot.title = element_text(hjust = 0.5))

plot(plot1)

## ------------------------------------------------------------------------
#To use rigdereg() function
rglm <- list(type=c("Classification", "Regression"),
            library="mlr",
            loop=NULL,
            prob=NULL)

#Parameter setting
rglm$label<-"Ridge regression by mlr package"

rglm$parameters <- data.frame(parameter = "lambda",
                  class = "numeric",
                  label = "lambda")

rglm$fit <- function(x, y, wts, param, lev, last, classProbs, ...) { 
  
  rg_data <- as.data.frame(x)
  rg_data$.outcome <- y
  
  rgrf <- ridgereg$new(rg_data$.outcome ~ ., data = rg_data, lambda=param$lambda, ...)
  
  # rgrf <- ridgereg$new(formula = formula, data = rg_data, lambda = param$lambda)
  rgrf
 }
 
rglm$predict <- function(modelFit, newdata, preProc=NULL, submodels = NULL){
  predict(modelFit, newdata)
}

rglm$sort  <- function(x) x[order(x$lambda),]


rglm$grid <- function(x, y, len=NULL, search="grid"){
  data.frame(lambda=1)
}

