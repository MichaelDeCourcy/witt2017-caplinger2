# Install and import packages

install.packages("needs")
install_github('araastat/reprtree')

needs::needs(devtools, 
             randomForest, 
             plotrix, 
             tree,
             caret, 
             inTrees, 
             rprojroot)

root <- rprojroot::find_rstudio_root_file()

dats <- read.csv(file.path(root,'data','Region0.csv'))
head(dats, 100)

dats$OWNERSHP<- as.factor(dats$OWNERSHP)
dats$RENT<- as.factor(dats$RENT)
dats$FOODSTMP<-as.factor(dats$FOODSTMP)
dats$VEHICLES <- as.factor(dats$VEHICLES) 
dats$MARST <- as.factor(dats$MARST) 
dats$RACE <- as.factor(dats$RACE) 
dats$HISPAN <- as.factor(dats$HISPAN)
dats$EDUC <- as.factor(dats$EDUC)
dats$EMPSTAT <- as.factor(dats$EMPSTAT)
dats$GENERATION <- as.factor(dats$GENERATION)
dats$VETSTAT <- as.factor(dats$VETSTAT) 
dats$NCHILD <- as.factor(dats$NCHILD)
dats$SPEAKENG<-as.factor(dats$SPEAKENG)
dats$HCOVANY<-as.factor(dats$HCOVANY)
dats$CITIZEN<-as.factor(dats$CITIZEN)
dats$FAMSIZE<-as.factor(dats$FAMSIZE)
dats$MARRNO<-as.factor(dats$MARRNO)
dats$GRADEATT<-as.factor(dats$GRADEATT)
dats$UHRSWORK<-as.factor(dats$UHRSWORK)
dats$DIFFREM<-as.factor(dats$DIFFREM)
dats$DIFFMOB<-as.factor(dats$DIFFMOB)
dats$UTILITIES<-as.factor(dats$UTILITIES)

#formula <- factor(POVERTY) ~ REGION + OWNERSHP + FOODSTMP + SEX + MARST + RACE + HISPAN + EDUC + GENERATION + VETSTAT + NCHILD + POVERTY + AGE + INCTOT + FTOTINC + HHINCOME

formula <- POVERTY ~ OWNERSHP + RENT + FOODSTMP + VEHICLES + FAMSIZE + NCHILD + GENERATION + MARST + MARRNO + RACE + HISPAN + CITIZEN + SPEAKENG + HCOVANY + EDUC + GRADEATT + EMPSTAT + UHRSWORK + DIFFREM + DIFFMOB + VETSTAT + UTILITIES

#Set seed for data

data <- read.csv(file.path(root, 'data','Region0.csv'))

set.seed(415)

# Split data into sample and test

samp <- sample(nrow(data), size = 0.8 * nrow(data))

train <- data[samp, ]

test  <- data[-samp, ]

# Set HyperParameter

metric <- "Accuracy"

mtry <- sqrt(15)

tunegrid <- expand.grid(.mtry=mtry)

# Train multiple models and combine the models

model1 <- randomForest(formula , 
                       train, 
                       ntree = 50, 
                       norm.votes = FALSE, 
                       do.trace = T, 
                       keep.inbag = T)

model2 <- randomForest(formula, 
                       train, 
                       ntree = 50, 
                       norm.votes = FALSE, 
                       do.trace = T, 
                       keep.inbag = T)

model3 <- randomForest(formula, 
                       train, 
                       ntree = 50,
                       norm.votes = FALSE, 
                       do.trace = T, 
                       keep.inbag = T)

model4 <- randomForest(formula, 
                       train, 
                       ntree = 50, 
                       norm.votes = FALSE, 
                       do.trace = T, 
                       keep.inbag = T)

model.all <- combine(model1, model2, model3, model4)


# Cross Validation from test

# determine model accuracy

pred = predict(model.all, newdata = test)
truth = test$POVERTY

roc.curve = function(thresh, print = FALSE) {
  
 Ps = (pred > thresh) * 1
 FP = sum((Ps==1) * (truth==0)) / sum(truth==0)
 TP = sum((Ps==1) * (truth==1)) / sum(truth==1)
 
 if(print) print(table(Observed = truth, Predicted = Ps))
 
 vect = c(FP, TP)
 names(vect) = c("FPR","TPR")
 return(vect)
}

threshold <- 0.0002

roc.curve(thresh = threshold, print = TRUE)


I = (((pred > threshold) & (truth==0)) | ((pred <= threshold) & (truth==1)))
 plot(pred,
      truth,
      col = c("red","blue")[I+1],
      pch = 19,
      cex = .7,
      xlab = "",
      ylab = "")
 
 ROC.curve=Vectorize(roc.curve, vectorize.args = 'thresh')

 
M.ROC = ROC.curve(seq(0, 1, by = .01))
 plot(M.ROC[1,],
      M.ROC[2,],
      col = 2,
      lwd = 3,
      type = "l",
      las = 1)
# Describe model output and feature relevance

varImpPlot(model1)

importance(model1)

varImpPlot(model2)

importance(model2)

varImpPlot(model3)

importance(model3)

varImpPlot(model4)

importance(model4)

varImpPlot(model.all)

importance(model.all)

# Extract Tree

library(randomForest)
setwd("~/Desktop")
data <- read.csv('Region0.csv')
data('Region0.csv')

# Optimize model hyperparameters 
pred <- predict(model.all, newdata = test)
plot(pred)
model1
model2
model3
model4
