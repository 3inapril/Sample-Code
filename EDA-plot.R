data("mtcars")

library(ggplot2)
library(plyr)
library(dplyr)
options(stringsAsFactors = F)

colors <-      c('#FA8072', '#4876FF',      '#00C5CD',     '#008B45',        '#B0171F',  
                 '#FF8C00',      '#98FB98',   '#D02090')

color_names <- c('salmon',  'royalblue 1' , 'turquoise 3', 'springgreen 3',  'indian red',  
                 'darkorange',   'palegreen', 'violetred')

date_dat <- data.frame(date=rep(seq.Date(as.Date('2015-01-01'), as.Date('2016-02-01'), 'month'), each=2),
                       value=runif(28, min=50, max=100),
                       category=rep(c('Group1','Group2'), 14))


# Boxplot ----------------------------------------------------------------------

# add x, y label and title
ggplot(mtcars, aes(factor(cyl), disp)) +
  geom_boxplot() +
  labs(title = "disp by cyl group boxplot",
       x = "cyl", 
       y = "disp")

# change legend
# Set the "anchoring point" of the legend (bottom-left is 0,0; top-right is 1,1)

ggplot(mtcars, aes(factor(cyl), disp)) +
  geom_boxplot(aes(fill = factor(gear))) +
  scale_fill_discrete(name="Gear",
                      breaks=c('5','4','3'),
                      labels=c("5 gear", "4 gear", "3 gear")) +
  theme(legend.justification=c(1,0), legend.position=c(1,0)) +
  labs(title = "TITLE HERE",
       x = "X VARIABLE", 
       y = "Y VARIABLE")
  # Position legend in graph, where x,y is 0,0 (bottom left) to 1,1 (top right)
  # + theme(legend.position=c(.5, .5))

# Scatter plot -----------------------------------------------------------------

rdm <- runif(nrow(mtcars), min=-50, max=80)
ggplot(mtcars, aes(x=disp, y=disp+rdm, color=factor(gear))) + 
  geom_point() +
  # scale_fill_discrete needs fill = ... to work. Or you can do it manually
  scale_colour_manual(name="Gear",
                      values = color_names[c(6,3,5)],
                      breaks=c('5','4','3'),
                      labels=c("5 gear", "4 gear", "3 gear")) +
  xlim(0, max(mtcars$disp)) +
  ylim(0, max(mtcars$disp+rdm)) +
  # reference lines
  geom_abline(slope = 1, intercept = 0) +
  geom_hline(aes(yintercept=median(mtcars$disp+rdm))) +
  labs(title = "Scatter plot in different group",
       x = "X VARIABLE", 
       y = "Y VARIABLE")


ggplot(mtcars, aes(x=disp, y=hp)) + 
  geom_point() +
  xlim(0, max(mtcars$disp)) +
  ylim(0, max(mtcars$disp+rdm)) +
  # reference lines
  geom_abline(slope = 1, intercept = 0) +
  geom_hline(aes(yintercept=median(mtcars$disp+rdm)))  +
  labs(title = "scatter plot var_1 vs. var_2",
       x = "X VARIABLE", 
       y = "Y VARIABLE")

# Histogram ---------------------------------------------------------------------

mtcars_summary <- mtcars %>% 
  group_by(cyl) %>% 
  summarise(mpg_mean = mean(mpg)) %>% 
  ungroup()

# with labels on top of the histgram bar to indicate values of this bar
ggplot(mtcars_summary, aes(x=as.factor(cyl),y=mpg_mean)) +
  geom_bar(stat="identity") +
  ggtitle("bar plot 1") +
  xlab("cyl") +
  ylab("mean mpg") +
  geom_text(aes(label = paste0(round(mpg_mean, 2)," mpg")), size = 5, hjust = 0.5, vjust = -0.5) + 
  theme(plot.title = element_text(size=22),
        axis.text=element_text(size=15),
        axis.title=element_text(size=16))

# to make bars of all groups of the same width
# label of count on top of the bars
ggplot(mtcars, aes(factor(cyl), fill = factor(vs))) +
  geom_bar(position = "dodge", width = 0.5) +
  geom_text(size = 4, stat='count', aes(label=..count..),  vjust=0.5, colour="black",
            position=position_dodge(.5))

# By default, geom_bar uses stat="bin". This makes the height of each bar equal to the number of cases in each group, and it is incompatible with mapping values to the y aesthetic. If you want the heights of the bars to represent values in the data, use stat="identity"

# stack bar plot
mtcars_summary2 <- mtcars %>% 
  group_by(cyl, gear) %>% 
  summarise(freq = n()) %>% 
  ungroup()
ggplot(mtcars_summary2, 
       aes(x = factor(cyl), y = freq, fill = factor(gear), label = freq)) +
  geom_bar(stat = "identity") +
  geom_text(size = 4, position = position_stack(vjust = 0.5))

# Time series Plot -------------------------------------------------------------

# dot with smooth line
ggplot(date_dat, 
       aes(x=date, y=value, color = category)) +
  geom_point() +
  geom_smooth(method=loess, se = F) +
  geom_vline(xintercept = as.numeric(as.Date('2015-10-01')),
             linetype = 4, 
             colour="black") +
  labs(title = "xxx",
       x = "Date",
       y = "Value") 

# lines 
ggplot(date_dat, 
       aes(x=date, y=value, color = category)) +
  geom_line(size=1) +
  geom_vline(xintercept = as.numeric(as.Date('2015-10-01')),
             linetype = 4, 
             colour="black") +
  labs(title = "xxx",
       x = "Date",
       y = "Value") 

# Density plot, QQ-plot ----------------------------------------------

# density curve only
ggplot(mtcars, aes(disp)) +
  geom_density(adjust= 1) # adjust can be 1/5, 1/2, 1, 2, 3.... etc.

# multiple density curves
ggplot(mtcars, aes(disp, colour = as.factor(cyl))) +
  geom_density() +
  scale_colour_manual(name="cyl",
                      breaks=c('4','6','8'),
                      values = color_names[c(1,4,6)], 
                      labels=c("4 cyl", "6 cyl", "8 cyl"))+
  xlim(min(mtcars$disp), max(mtcars$disp)) +
  labs(title = "multiple density curves",
       x = "X VARIABLE", 
       y = "Y VARIABLE")

# histograms of multiple groups
ggplot(mtcars, aes(x=disp, fill=as.factor(cyl))) +
  geom_histogram(bins = 15, alpha=.5, position="identity") +
  labs(title = "histograms of multiple groups",
       x = "X VARIABLE", 
       y = "Y VARIABLE")

# Density plots of multiple groups
ggplot(mtcars, aes(x=disp, fill=as.factor(cyl))) + 
  geom_density(alpha=.3) +
  labs(title = "Density plots of multiple groups",
       x = "X VARIABLE", 
       y = "Y VARIABLE")

# histogram + density
ggplot(mtcars, aes(disp)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 bins = 15,
                 colour="black", fill="white") +
  geom_density(alpha=0.5, fill="#FF6666") + # Overlay with transparent density plot
  labs(title = "histogram + density",
       x = "X VARIABLE", 
       y = "Y VARIABLE")

# QQ-plot
qqnorm(mtcars$disp)
qqline(mtcars$disp)

# Y variable against all X variables -------------------------------------------

mtcars_sub <- mtcars[c("mpg", "disp", "hp", "drat", "wt", "qsec")]
# scatter matrix
pairs(mtcars_sub)

# correlation matrix
corr_matrix <- cor(mtcars_sub)

check_corr <-data.frame(row=rownames(corr_matrix)[row(corr_matrix)], 
                        col=colnames(corr_matrix)[col(corr_matrix)], 
                        corr=c(corr_matrix))
check_corr <- check_corr %>% 
  filter(abs(corr) > 0.6,
         row > col) 
  

