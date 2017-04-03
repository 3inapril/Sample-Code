data("mtcars")
library(ggplot2)
library(plyr)
library(dplyr)

options(stringsAsFactors = F)

# Bin Continuous Vars to Classes -----------------------------------------------

c1 <- cut(mtcars$disp, breaks = seq(70, 600, by = 80), include.lowest=TRUE)
c1
table(c1)

# bin using percentile
pctile <- quantile(mtcars$disp, probs = c(0, 0.1, 0.5, 0.8, 1))
c1 <- cut(mtcars$disp, breaks = pctile, include.lowest=TRUE)

# Dummfy categorical variable --------------------------------------------------

# option 1: create a model matrix
mdl_mtrx <- model.matrix(y ~ . -1, termCrosssell)
#' In this option n levels will be coded into n dummy variables. To avoid 
#' multicolinearity and singularity, we need to suppress the intercept(-1 in the formula)

# option 2: code it yourself
dummy_coded <- termCrosssell
for(level in unique(dummy_coded$job)) {
  dummy_coded[paste("job", level,sep="_")] <- ifelse(dummy_coded$job==level, 1, 0)
}

# option 3: use function in caret (n levels coded into n-1 dummy vars)
# Noted that the dummy vars become numeric instead of factor
# http://amunategui.github.io/dummyVar-Walkthrough/
options(contrasts = c("contr.treatment", "contr.poly"))
library(caret)
Dummy <- caret::dummyVars("~.",data=termCrosssell[,1:16], fullRank=T)
dummy_coded_x <- as.data.frame(predict(Dummy,termCrosssell))
dummy_coded <- cbind(dummy_coded_x, termCrosssell$y)

# Count NAs in each variable ---------------------------------------------------

mtcars2 <- mtcars
for (i in 1:20){
  mtcars2[sample(1:nrow(mtcars2), 1),
          sample(1:ncol(mtcars2), 1)] <- NA
}

apply(mtcars2, 2, function(x) sum(is.na(x)))
apply(mtcars2, 2, function(x) sum(is.na(x))/length(x))

# Imputation Missing Values with mean / median / mode --------------------------

# used when the variation is low or if the variable has low leverage over the response
mtcars2$qsec[is.na(mtcars2$qsec)] <- mean(mtcars2$qsec, na.rm = T)
mtcars2$qsec[is.na(mtcars2$qsec)] <- median(mtcars2$qsec, na.rm = T)

# a function to get mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mtcars2$qsec[is.na(mtcars2$qsec)] <- getmode(mtcars2$qsec)

# Imputation Missing Values with RF prediction ---------------------------------

library(mice)
# need to exclude response variable from the imputation RF
miceMod <- mice(mtcars2[, !names(mtcars2) %in% "carb"], method="rf")  # perform mice imputation, based on random forests.
miceOutput <- complete(miceMod)  # generate the completed data.
anyNA(miceOutput)

# Imputation Missing Values with KNN -------------------------------------------

# For every observation to be imputed, it identifies ‘k’ closest observations based 
# on the euclidean distance and computes the weighted average (weighted based on distance) of these ‘k’ obs
library(DMwR)
knnOutput <- knnImputation(mtcars2[, !names(mtcars2) %in% "carb"])  # perform knn imputation.
anyNA(knnOutput)

