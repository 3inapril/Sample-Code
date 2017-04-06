
# Test Normal Distribution -----------------------------------------------------

normaly_disb <- rnorm(100, mean=5, sd=1) # generate a normal distribution
# Null hypo is that the data is normaly distributed
shapiro.test(normaly_disb)


### T TEST
# To calculate the unlikeliness, we first calculate the test statistic [1]. The test statistic will be distributed according to a t-distribution if we assume the null hypothesis. We can then calculate how unlikely it is for our observed test statistic (or something more extreme) to occur if the null hypothesis was actually true, and then decide to reject or fail to reject the null hypothesis based on that probability. This probability is called the p-value.
# Test if the mean of a sample is a specific value one-sample t test -----------

# One Sample t-Test
# Testing if the mean of a sample could reasonably be a specific value (assume normally distributed)
x <- rnorm(50, mean = 10, sd = 0.5)
t.test(x, mu=10) # testing if mean of x could be

# Compare the mean of 2 samples (Two-sample T-test) ----------------------------

# Paired Test (compare pre and pro obs by obs)
t.test(data$pre, data$pro, paired = TRUE, alt = "greater")
# alternative hypo is that pre > pro, null is pre <= pro
# if p value < 0.05, reject null

# alt can be 'greater', 'two-sided' or 'less'

# paired t test can check if the increse exist pre vs. pro, while DID test can
# check if the increase pattern is different in user group adn control group (
# AKA, if treatment makes difference)

# Test if the mean of a sample is a specific value (no normal distribution) ----

# Wilcoxon Signed Rank Test
# One-sample test
# Testing the mean of a sample when normal distribution is not assumed
wilcox.test(input.vector, mu = m, conf.int = TRUE)

# Compare the means of two samples (no normal distribution) --------------------

# Two-sample test
# compare the mean of 2 samples when normal distribution is not assumed
wilcox.test(vector_1, vector_2, alternative = "greater")
# for paired test, set paired = TRUE

# Test if two samples have same variance ---------------------------------------

#  Fisher’s F-Test: 
var.test(x, y)

# Difference in Difference Test ------------------------------------------------

# paired t test can check if the increse exist pre vs. pro, while DID test can
# check if the increase pattern is different in user group adn control group (
# AKA, if treatment makes difference)
did_reg_unit <- lm(y ~ participant + pre_pro + participant * pre_pro, data = data)
summary(did_reg_unit)
# participant * pre_pro not significant
# the treatment makes no difference 

# Test the independence of two categorical variables in a contingency table ----

# Chi sq test
#' A chi-squared test is any statistical hypothesis test wherein the sampling distribution of the test statistic is a chi-squared distribution when the null hypothesis is true. can be used to attempt rejection of the null hypothesis that the data are independent

#'  independence of two cat var
#'  
#' given a table of two categorical vars with counts in each cell, chi-square test can be used to test 
#' if these two cat vars are independent. 
#' the expected frequency count for each cell of the table should be at least 5. If the expected number of observations in any category is too small, the chi-square test may give inaccurate results, and you should use an exact test instead

library(MASS)       # load the MASS package 
tbl = table(survey$Smoke, survey$Exer) 
tbl

chisq.test(tbl, simulate.p.value = TRUE) # or chisq.test(survey$Smoke, survey$Exer, simulate.p.value = TRUE)
#' Higher Chi2 means better correlation.
#' if you see Warning message: Chi-squared approximation may be incorrect. That is 
#' due to chi-square approximation to the distribution of the test statistic relies on the counts being roughly normally distributed.  If many of the expected counts are very small, the approximation may be poor. set the simulate.p.value argument to TRUE; then you aren't reliant on the chi-square approximation to the distribution of the test statistic.

#' Chi-Square Goodness of Fit Test
#' 
#' The test is applied when you have one categorical variable from a single population. It is used to determine whether sample data are consistent with a hypothesized distribution.
#' For example, suppose a company printed baseball cards. It claimed that 30% of its cards were rookies; 60%, veterans; and 10%, All-Stars. We could gather a random sample of baseball cards and use a chi-square goodness of fit test to see whether our sample distribution differed significantly from the distribution claimed by the company.
#' 
#' The variable under study must be categorical and the expected value of the number of sample observations in each level of the variable is at least 5.
#' H0: The data are consistent with a specified distribution. 
#' H1: The data are not consistent with a specified distribution.

#' eg. 
#' > eyes
#' Eye
#' Brown  Blue Hazel Green 
#' 220   215    93    64
#' Suppose a certain physiologist proposes that 50% of people should have brown eyes, 25% blue eyes, 15% hazel eyes, and 10% green eyes. 
#' chisq.test(eyes, p=c(.5, .25, .15, .1))
#' p-value = 6.462e-11  ; The physiologist's hypothesis is not supported. 

# Test Correlation -------------------------------------------------------------

cor.test(x, y) # where x and y are numeric vectors

# Kolmogorov And Smirnov Test --------------------------------------------------

# Test if two samples have the same distribution
x <- rnorm(50)
y <- runif(50)
ks.test(x, y)

# A/B Test ---------------------------------------------------------------------

#' The Null Hypothesis is the assumption that there is no relationship between the values. Usually, we set it to the exact opposite of what we would like to see. When expecting a higher conversion rate on the new landing page, use a Null Hypothesis that the mean conversion rates are equal.

#' Significance Level – the probability of detecting an effect that is not there. Stat-gurus call it the probability of a Type I Error or the probability of a False-Positive detection (read more on types of error here). 

#' Power of Test, which is the probability of finding an effect that exist. This is also called the probability of a True-Positive. Commonly chosen values are 80% or 90%.

#' Minimum Detectable Effect: the minimal difference in performance we wish to detect (if indeed one exists)

#' There are two basic Hypothesis Testing methodologies that come in handy when doing A/B testing.
#' http://www.marketingdistillery.com/2014/08/03/ab-tests-in-marketing-sample-size-and-significance-using-r/

#' A/B test assumptions:
#' The first assumption is usually pretty solid: we assume that the “samples” (namely the visitors we expose to the variations) are independent of each other, and their behavior is not inter-dependent. 
#' The second assumption is that the samples are identically distributed. Simply stated, this means that the probability of converting is the same for all visitors.
#' The last assumption is that the measures that we sample, e.g. the CTR or conversion rate, are normally distributed.

library(pwr)
#'
#' ### Test of proportion
#' 
#' We use a Test of Proportion when working with percentage metrics like Conversion Rate, Click-through Rate etc. Key assumption is the independence of groups, e.g. that customers in one group always see one version of the landing page. The Null Hypothesis here is that the percentage is equal in both groups.

#' power.prop.test() computes power of test or sample size.
power.prop.test(n = NULL, p1 = NULL, p2 = NULL, sig.level = 0.05,
                power = NULL,
                alternative = c("two.sided", "one.sided"),
                strict = FALSE)
#' We use this function by providing all but one parameter. R will calculate the left-out argument. Use p1 to give the known value (e.g. current Conversion Rate) and p2 to state the smallest effect you want to detect with the test.

#' pwr.2p2n.test() to compute sample size needed with two unequal groups.
pwr.2p2n.test(h = NULL, n1 = NULL, n2 = NULL, 
              sig.level = 0.05, power = NULL, 
              alternative = c("two.sided", "less","greater"))
# The recommended values for h(effect size) are: 0.2 for small effects, 0.5 for medium and 0.8 for big effects. 
# Use one of n1 or n2 to provide sample size in one group and omit the other to compute it. For example to calculate the size of the test group needed to detect a small effect with 1000 customers in the control group,  pwr.2p2n.test(h=0.2, n1=1000, sig.level=0.05, power=0.8)

# The actual test is performed using the prop.test {stats}
# to test two campaigns each with a 1000 displays, 32 and 54 conversions:
prop.test(c(32, 54), c(1000,1000))

#' 
#' ### Test of means
#' 
#' Comparing non-fractional values that follow a normal distribution (e.g. Average Order Value, Time Spent on Page etc.) is done with a Two-sample unpaired t-test. The recommended variant is the Welch t-test. It has quite flexible assumptions and can be used with unequal sample sizes and unequal variances in both groups. Note that if your data follows a log-normal distribution you may need to apply that log function.
# Sample size can be computed using pwr.t2n.test {pwr} function:
pwr.t2n.test(n1 = NULL, n2= NULL, d = NULL, 
             sig.level = 0.05, power = NULL, 
             alternative = c("two.sided", "less","greater"))
# the effect power is given as the d parameter. Use 0.2 for small, 0.5 for medium and 0.8 for large effects
# eg. for computing the test sample size for a campaign with an estimated medium effect and 10000 customers in the control group:
pwr.t2n.test(n1=10000, d=0.5, sig.level=0.05, power=0.90)
# rst: around 43 customers are needed in the test group.

# The test itself is done with a t.test {stats}:

t.test(x, y = NULL,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95, ...)

# eg. 
# x <- rnorm(1000, mean = 12)
# y <- rnorm(50, mean = 30)
# t.test(x, y,
#       alternative = c("two.sided"),
#       paired = FALSE,
#       conf.level = 0.95)
# reject null. different mean

# Between Subject ANOVA ------------------------------------------------------------------------

# It's easier to use the package car where you can specify the type of error

#' The influence of particular factors (including interactions) can be tested by examining the differences between models. The notation shows the incremental differences in sums of squares, for example SS(AB | A, B) represents “the sum of squares for interaction after the main effects”, and SS(A | B) is “the sum of squares for the A main effect after the B main effect and ignoring interactions.
#' SS(AB | A, B) = SS(A, B, AB) – SS(A, B)
#' SS(A | B) = SS(A, B) – SS(B)

#' The different types of sums of squares then arise depending on the stage of model reduction at which they are carried out. In particular:

#' Type I, also called “sequential” sum of squares:
#' SS(A) for factor A.
#' SS(B | A) for factor B.
#' SS(AB | B, A) for interaction AB.
#' Because of the sequential nature and the fact that the two main factors are tested in a particular order, this type of sums of squares will give different results for unbalanced data depending on which main effect is considered first.

#' Type II:
#' SS(A | B) for factor A.
#' SS(B | A) for factor B.
#' This type tests for each main effect after the other main effect.
#' Note that no significant interaction is assumed (in other words, you should test for interaction first (SS(AB | A, B)) and only if AB is not significant, continue with the analysis for main effects).

#' Type III:
#' SS(A | B, AB) for factor A.
#' SS(B | A, AB) for factor B.

#' Summary: Usually the hypothesis of interest is about the significance of one factor while controlling for the level of the other factors. This equates to using type II or III SS. In general, if there is no significant interaction effect, then type II is more powerful, and follows the principle of marginality. If interaction is present, then type II is inappropriate while type III can still be used, but results need to be interpreted with caution (in the presence of interactions, main effects are rarely interpretable).

#' Because the multi-way ANOVA model is over-parameterised, it is necessary to choose a contrasts setting that sums to zero, otherwise the ANOVA analysis will give incorrect results with respect to the expected hypothesis. (The default contrasts type does not satisfy this requirement.)
getOption("contrasts")
options(contrasts = c('contr.sum','contr.poly'))
# After finish, switch back 
options(contrasts = c('contr.treatment','contr.poly'))
#' R provides Type I sequential SS, not the default Type III marginal SS reported by SAS and SPSS. 
#' In a unbalanced data with more than one term on the right hand side of 
#' the equation order will matter (i.e., A+B and B+A will produce different results)! 
#' this rarely tests a hypothesis of interest, since essentially the effect of one factor is calculated based on the varying levels of the other factor.
#' We will need use the drop1( ) function to produce the familiar Type III results. It will compare each term with the full model. Alternatively, we can use anova(fit.model1, fit.model2) to compare nested models directly.'

summary(aov_fit2) # display Type I ANOVA table
drop1(aov_fit2, ~., test="F") # type III SS and p-values from an F-test.

#' One-way ANOVA
aov_fit1 <-  aov(balance ~ marital, data=cross.sell.train) # growth is numeric while sugar is categorical
summary(aov_fit1)
# p < 0.05, there is a significant effect of marital status upon balance

#' Post-hoc testing (Multiple Comparisons)
# there are multiple levels in marital. We would like to know which levels are significantly different from the controls and from other levels.
# remember that results are based on Type I SS
TukeyHSD(aov_fit1)

#' Two-way ANOVA
aov_fit2 <-  aov(balance ~ marital*education, data=cross.sell.train) # growth is numeric while sugar is categorical
summary(aov_fit2)

library(car)
Anova(lm(balance ~ marital*education, data=cross.sell.train, 
         contrasts=list(topic=contr.sum, sys=contr.sum)), 
      type=3)

# boxplot for each combination
boxplot(balance ~ marital*education, data=cross.sell.train)
print(model.tables(aov_fit2,"means"),digits=3) 

#' Visualize the data
# Two-way Interaction Plot 
library(datasets)
attach(mtcars)
gears <- factor(gear)
cyl <- factor(cyl)
interaction.plot(cyl, gear, mpg, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),	
                 xlab="Number of Cylinders", 
                 ylab="Mean Miles Per Gallon", 
                 main="Interaction Plot")

# Plot Means with Error Bars
plotmeans(mpg~cyl,xlab="Number of Cylinders",
          ylab="Miles Per Gallon", main="Mean Plot\nwith 95% CI")


# Within Subject Design ANOVA --------------------------------------------------

#' A within-subjects design is an experiment in which the same group of subjects serves in more than one treatment. 
#' As an example of a within-subjects design, let’s say that we are interested in the effect of different types of exercise on memory. We decide to use two treatments, aerobic exercise and anaerobic exercise. 
#' Since we are using a within-subjects design we have all participants begin by running in place and taking the test, after which we have the same group of people lift weights and then take the test. We compare the memory test scores in order to answer the question as to what type of exercise aids memory the most.

#' In a Within Subject(AKA, repeated-measures) design, each participant provides data at multiple time points. Due to this, the assumptions about model error are different for variances which are presented between subjects (SSB) than are variables presented within subjects (SSW). After the within-subject variability is partialled out, we model separately the effect of the experiment (i.e., SSE) and the error not account for by the experiment (i.e., SSR).

#' One-Way Within Subject ANOVA
datafilename="http://personality-project.org/r/datasets/R.appendix3.data"
data.ex3=read.table(datafilename,header=T)   #read the data into a table
data.ex3                                      #show the data
aov.ex3 = aov(Recall~Valence+Error(Subject/Valence),data.ex3)
summary(aov.ex3)
print(model.tables(aov.ex3,"means"),digits=3)       
#report the means and the number of subjects/cell
boxplot(Recall~Valence,data=data.ex3)          #graphical output



#' Two-Way Within Subject ANOVA
datafilename="http://personality-project.org/r/datasets/R.appendix4.data"
data.ex4=read.table(datafilename,header=T)   #read the data into a table
data.ex4                                      #show the data
aov.ex4=aov(Recall~(Task*Valence)+Error(Subject/(Task*Valence)),data.ex4 )

summary(aov.ex4)
print(model.tables(aov.ex4,"means"),digits=3)       
#report the means and the number of subjects/cell
boxplot(Recall~Task*Valence,data=data.ex4) #graphical summary of means of the 6 cells
attach(data.ex4)
interaction.plot(Valence,Task,Recall)    #another way to graph the interaction
detach(data.ex4)