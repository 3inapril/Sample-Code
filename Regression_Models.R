library(plyr)
library(dplyr)
# TREE -------------------------------------------------------------------------

#' sub-divide, or partition, the space into smaller regions, where the interactions
#' are more manageable. We then partition the sub-divisions again — this is called 
#' recursive partitioning — until finally we get to chunks of the space which are
#'  so tame that we can fit simple models to them. The global model thus has two 
#'  parts: one is just the recursive partition, the other is a simple model for 
#'  each cell of the partition

#' Each of the nodes, or leaves, of the tree represents a cell of the partition, 
#' and has attached to it a simple model which applies in that cell only. For 
#' classic regression trees, the model in each cell is just a constant estimate of 
#' Y (mean value).

#' With regression trees, what we want to do is maximize I[C; Y ], where Y is now 
#' the dependent variable, and C are now is the variable saying which leaf of the
#' tree we end up at. Once again, we can’t do a direct maximization, so we again
#' do a greedy search. We start by finding the one binary question which maximizes
#' the information we get about Y; this gives us our root node and two daughter 
#' nodes. At each daughter node, we repeat our initial procedure, asking which 
#' question would give us the maximum information about Y , given where we already 
#' are in the tree. We repeat this recursively.

#' A typical stopping criterion is to stop growing the tree when the decrease in 
#' S(S=SUM over terminal node(sum of squared deviation in each terminal node)) 
#' becomes less than some δ, or when they would 
#' result in nodes containing less than q, say, five percent of the total data.

#' The basic regression-tree-growing algorithm then is as follows:
#' 1. Start with a single node containing all points. Calculate mc and S
#' 
#' 2. If all the points in the node have the same value for all the independent 
#'    variables, stop. Otherwise, search over all binary splits of all variables for
#'    the one which will reduce S as much as possible. If the largest decrease in
#'    S would be less than some threshold δ, or one of the resulting nodes would
#'    contain less than q points, stop. Otherwise, take that split, creating two
#'    new nodes.
#'    
#' 3. In each new node, go back to step 1.

#' CV to prune tree:
#' We then apply the basic tree-growing algorithm to the training data only, 
#' with q = 1 and δ = 0 — that is, we grow the largest tree we can. This is 
#' generally going to be too large and will over-fit the data. We then use 
#' cross-validation to prune the tree. At each pair of leaf nodes with a common 
#' parent, we evaluate the error on the testing data, and see whether the sum of
#' squares would be smaller by remove those two nodes and making their parent a
#' leaf. This is repeated until pruning no longer improves the error on the testing data.
#' 
#' There are lots of other cross-validation tricks for trees. One cute one is to
#' alternate growing and pruning. We divide the data into two parts, as before, and
#' first grow and then prune the tree. We then exchange the role of the training
#' and testing sets, and try to grow our pruned tree to fit the second half. We then
#' prune again, on the first half. We keep alternating in this manner until the size
#' of the tree doesn’t change.

# Regression Tree Example
library(rpart)

# grow tree 
reg_tree_fit <- rpart(Mileage ~ Price + Country + Reliability + Type, 
                      method="anova", data=cu.summary, 
                      control=rpart.control(xval = 20))

printcp(reg_tree_fit) # display the results 
plotcp(reg_tree_fit) # visualize cross-validation results 
#' for definition if rel error and x error, pls refer to Classsification_Models.R
summary(reg_tree_fit) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(reg_tree_fit) # visualize cross-validation results  	
par(mfrow=c(1,1))
# plot tree 
plot(reg_tree_fit, niform=TRUE,  margin=0.05,
     main="Regression Tree for Mileage ")
text(reg_tree_fit, use.n=TRUE, all=TRUE, cex=.8, pretty=2)
--------------
--------------
# prune the tree 
pfit <- prune(reg_tree_fit, 
              cp= reg_tree_fit$cptable[which.min(reg_tree_fit$cptable[,"xerror"]),"CP"])   
# plot the pruned tree 
plot(pfit, uniform=TRUE, margin=0.05,main="Pruned Regression Tree for Mileage")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
--------------
--------------
# for tree prunning, we can use CV to compare the performance of various tree sizes
library(tree)
reg_tree_fit2 <- tree(Mileage~Price + Country + Reliability + Type, 
                      data=cu.summary,
                      control = tree.control(nobs, mincut = 5, minsize = 10, mindev = 0.01))
cv_tree <- cv.tree(reg_tree_fit2, K=5)
names(cv_tree)
plot(cv_tree$size, cv_tree$dev, 
     type='b', xlab='tree size', ylab='mse', main='dev vs. size')
# based on the plot size 3 is a good tree size
cv_tree$size[which.min(cv_tree$dev)]

pruned_tree <- prune.tree(reg_tree_fit2, best = 5)
plot(pruned_tree, uniform=TRUE, margin=0.05,main="Pruned Regression Tree for Mileage")
text(pruned_tree, use.n=TRUE, all=TRUE, cex=.8)

summary(pruned_tree)

#' In a regression tree, the saturated model would be one that had as many terminal nodes (leaves) as observations so it would perfectly fit the response. The deviance of a simpler model can be computed as the node residual sums of squares, summed over all nodes. In other words, the sum of squared differences between predicted and observed values. This is the same sort of error (or deviance) used in least squares regression.

#' For a classification tree, residual sums of squares is not the most appropriate measure of lack of fit. Instead, there is an alternative measure of deviance, plus trees can be built minimising an entropy measure or the Gini index. The latter is the default in rpart. 

# Random Forest ----------------------------------------------------------------
#' The random forests algorithm (for both classification and regression) is as follows:
#' 1. Draw n=ntree bootstrap samples from the original data.
#' 2. For each of the bootstrap samples, grow an unpruned
#'    classification or regression tree, with the following modification: 
#'    at each node, rather than choosing the best split among all predictors,
#'    randomly sample mtry of the predictors and choose the best split from among 
#'    those variables. (Bagging can be thought of as the special case of random
#'    forests obtained when mtry = p, the number of predictors.)
#' 3. Predict new data by aggregating the predictions of the n=ntree trees 
#'    (i.e., majority votes for classification, average for regression).

#' error estimation:
#' 1. At each bootstrap iteration, predict the data not in the bootstrap sample
#'    (what Breiman calls “out-of-bag”, or OOB, data) using the tree 
#'    grown with the bootstrap sample.
#' 2. Aggregate the OOB predictions. (On the average, each data point would be out-of-bag
#'    around 36% of the times, so aggregate these predictions.) Calcuate the error
#'    rate, and call it the OOB estimate of error rate.
 
#' http://www.bios.unc.edu/~dzeng/BIOS740/randomforest.pdf

library(randomForest)
library(MASS)
data(Boston)
set.seed(1341)
BH.rf <- randomForest(medv ~ ., Boston)
#' The default mtry is p/3 for regression
#' For selecting mtry, Prof. Breiman suggests trying the default, half of the default, 
#' and twice the default, and pick the best
print(BH.rf)

# LASSO (L1) and Ridge Regression(L2) ------------------------------------------

#' LASSO has as great advantage that it can shrink some of the 
#' coefficients to exactly zero, performing thus a selection of attributes with 
#' the regularization. (better model explanation)

#' https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html#lin
#' glmnet cannot take factor directly, you need to transform factor variables to dummies.
#' glmnet doesn't handle missing values. Either you have to keep only those records that are complete. e.g. with complete.cases() or do some imputation on your missing values with packages like mice, mi, amelia, etc. 

require(glmnet)
library(MASS)
set.seed(19875)  # Set seed for reproducibility
n <- 1000  # Number of observations
p <- 5000  # Number of predictors included in model
real_p <- 15  # Number of true predictors
x <- matrix(rnorm(n*p), nrow=n, ncol=p)
y <- apply(x[,1:real_p], 1, sum) + rnorm(n)

# Fitting the model (Ridge: Alpha = 0)
set.seed(999)
#' the method already performs 10-fold cross validation to choose the best λ
cv.ridge <- cv.glmnet(x, y, 
                      family="gaussian", # if 'binomial' means logistic regression
                      alpha=0, # (0=Ridge regression, 1=LASSO, value of (0,1)=elastic-net mixing)
                      parallel=TRUE, nfolds=10,
                      standardize=TRUE, # internally standardizing
                      type.measure='mse')
#' default is type.measure="deviance", 
#'                    which uses squared-error for gaussian models (a.k.a type.measure="mse" there),
#'                    deviance for logistic and poisson regression, 
#'                    and partial-likelihood for the Cox model
#' 
#' type.measure="auc" is for two-class logistic regression only, 
#'                    and gives area under the ROC curve. 
#' type.measure="mse" or type.measure="mae" (mean absolute error) 
#'                    can be used by all models except the "cox"; they measure 
#'                    the deviation from the fitted mean to the response.

#' type.measure="class" applies to binomial and multinomial logistic regression only, 
#'                    and gives misclassification error. 

# Results
plot(cv.ridge)
cv.ridge$lambda.min # value of lambda that gives minimum cvm(mean cross-validated error)
cv.ridge$lambda.1se # largest value of lambda such that error is within 1 standard error of the minimum
# print regression coefficients
coef(cv.ridge, s=cv.ridge$lambda.min)

---------------------------
# compare various alpha
---------------------------
# try various alpha, use same number of folds for comparison different alpha
foldid=sample(1:10,size=length(y),replace=TRUE)
cv1=cv.glmnet(x,y,foldid=foldid,alpha=1)
cv.5=cv.glmnet(x,y,foldid=foldid,alpha=.5)
cv0=cv.glmnet(x,y,foldid=foldid,alpha=0)
# plot to compare various alpha:
par(mfrow=c(2,2))
plot(cv1);plot(cv.5);plot(cv0)
plot(log(cv1$lambda),cv1$cvm,pch=19,col="red",xlab="log(Lambda)",ylab=cv1$name)
points(log(cv.5$lambda),cv.5$cvm,pch=19,col="grey")
points(log(cv0$lambda),cv0$cvm,pch=19,col="blue")
legend("topleft",legend=c("alpha= 1","alpha= .5","alpha 0"),pch=19,col=c("red","grey","blue"))
par(mfrow=c(1,1))
# We see that lasso (alpha=1) does about the best here. We also see that the range of lambdas used differs with alpha.

# assume that we decide to go with alpha=0.2
fit <- glmnet(x, y, alpha = 0.2, nlambda = 20) # 20 candidate lambdas, default 100
print(fit)
#' Df (the number of nonzero coefficients), %dev (the percent deviance explained) and Lambda (the corresponding value of λ)
plot(fit, xvar = "lambda", label = TRUE)
plot(fit, xvar = "dev", label = TRUE)
#' Users can decide what is on the X-axis. xvar allows three measures: “norm” for the ℓ1ℓ1-norm of the coefficients (default), “lambda” for the log-lambda value and “dev” for %deviance explained.

#' when lambda = 0.5, fit the model
coef.exact = coef(fit, s = 0.5, exact = TRUE)
# prediction result
predict(fit, newx = x[1:5,], type = "response", s = 0.5)

# Logistic Regression ----------------------------------------------------------

library(glmnet)
#' Logistic regression is another widely-used model when the response is categorical.
#' If there are two possible outcomes, we use the binomial distribution, 
#' else we use the multinomial.

#' Logistic regression is often plagued with degeneracies when p>N and exhibits
#' wild behavior even when NN is close to pp; the elastic-net penalty alleviates 
#' these issues, and regularizes and selects variables as well.

#' glmnet cannot take factor directly, you need to transform factor variables to dummies. 
#' The dummy variables are standardized just like the the numerical variables by
#' glmnet because according to Tibshrani: 
#' "The lasso method requires initial standardization of the regressors, so that the penalization scheme is fair to all regressors. For categorical regressors, one codes the regressor with dummy variables and then standardizes the dummy variables"

#' to use glmnet logistic regression we need to dummify categorical vars first
str(cross.sell.train)
options(contrasts = c("contr.treatment", "contr.poly"))
library(caret)
Dummy <- caret::dummyVars("~.",data=termCrosssell[,1:16], fullRank=T)
dummy_coded_x <- as.data.frame(predict(Dummy,termCrosssell))
dummy_coded <- cbind(dummy_coded_x, termCrosssell %>% select(y))

#' split to train and test sets
set.seed(1234)
splitIndex <- createDataPartition(dummy_coded$y, p = .75, list = FALSE, times = 1)
trainDF <- dummy_coded[splitIndex,]
testDF  <- dummy_coded[-splitIndex,]

x <- as.matrix(trainDF[, 1:42])  
y <- as.matrix(trainDF[, 'y'])
glmnet_logistic <- glmnet(x, y, family = "binomial")

#' multi-class classification
model <- glmnet(mat, classes, family="multinomial", alpha=1)
pred <- predict(model, test, type = "class", s=)
#' s indicates which values of the regularization parameter for which you want predictions
table(pred, as.factor(11:15))

# Glmnet Modeling in caret Package ---------------------------------------------

library(caret)

getModelInfo()$glmnet$type # glmnet can be used on both regression and classification

data <- termCrosssell 
#' glmnet doesn't take NAs and doesn't iternally dummify categorical vars, so you 
#' need to code it yourself
Dummy <- caret::dummyVars("~.",data=data, fullRank=T)
dummy_coded <- as.data.frame(predict(Dummy,data))
#' split to train and test sets
set.seed(1234)
splitIndex <- createDataPartition(dummy_coded$balance, p = .75, list = FALSE, times = 1)
trainDF <- dummy_coded[splitIndex,]
testDF  <- dummy_coded[-splitIndex,]

# use 10-fold cv to find the best tunning parameters
objControl <- trainControl(method='cv', number=10, returnResamp='none')
# here the response var balance is a numeric var so this is regression instead of classification
objModel <- train(trainDF %>% select(-balance), trainDF[,'balance'],
                  method='glmnet', metric = "RMSE", trControl=objControl)
objModel$bestTune # gives the tunned best parameters
plot(varImp(objModel, scale=F)) # var imp plot
# to see the coefficient we have to specify the lambda used
coef(objModel$finalModel, objModel$bestTune$lambda) # gives the coefficients with the best tuned parameters

predictions <- predict(object=objModel, testDF %>% select(-balance))

# check the performance of model
postResample(pred=predictions, obs=testDF$balance)
RMSE(predictions, testDF$balance)

# GBM (Stochastic Gradient Boosting, AKA Gradient Boosted Machine) -------------

# check the parameters taht can be tunes for a model in caret
getModelInfo()$gbm$parameters

data <- termCrosssell 
#' gbm handle NAs and categorical vars
#' split to train and test sets
set.seed(1234)
splitIndex <- createDataPartition(data$balance, p = .75, list = FALSE, times = 1)
trainDF <- data[splitIndex,]
testDF  <- data[-splitIndex,]

# use 10-fold cv to find the best tunning parameters
objControl <- trainControl(method='cv', number=10, returnResamp='none')
# here the response var balance is a numeric var so this is regression instead of classification
gbmGrid <- expand.grid(interaction.depth =  c(1,2,3,4),
                       n.trees = (2:10)*50,
                       shrinkage = seq(0.01, .03,.005),
                       n.minobsinnode = 100)

objModel <- train(trainDF %>% select(-balance), trainDF[,'balance'],
                  method='gbm', metric = "RMSE", tuneGrid= gbmGrid ,
                  trControl=objControl, train.fraction = 0.5)
objModel$bestTune # gives the tunned best parameters
plot(varImp(objModel, scale=F)) # var imp plot
# to see the coefficient we have to specify the lambda used
coef(objModel$finalModel, objModel$bestTune$lambda) # gives the coefficients with the best tuned parameters

predictions <- predict(object=objModel, testDF %>% select(-balance))

# check the performance of model
postResample(pred=predictions, obs=testDF$balance)
RMSE(predictions, testDF$balance)