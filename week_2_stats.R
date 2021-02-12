### null hypothesis: there is no observed difference between the heights of 
### seedlings produced from cross-fertilisation and from self-fertilisation.

library(tidyverse)
library(skimr)
library(rstatix)

darwin <- read_csv("Data/darwin.csv")
skim(darwin)

darwin <- darwin %>% 
  pivot_longer(cols=c("Self":"Cross"), 
               names_to="type", 
               values_to="height") %>% 
  mutate(pair=as.factor(pair), pot=as.factor(pot), type=as.factor(type))
### tidy data and change data types to appropriate ones

darwin %>% 
  ggplot(aes(x=height))+geom_histogram()
### plot histogram to check height distributions

darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()
### plots a dotplot of cross-fertilised maize heights vs self-fertilised.
### figure suggests mean height maybe greater for crossed plants than inbred plants

model1 <- lm(height~1, data=darwin)
model1
### estimates total mean of all plants. ~1 means there is no independent variable.

darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()+
  geom_abline(intercept=18.88, 
              slope=0, 
              linetype="dashed")
### plots dotplot with mean of all plants plotted on it as a dashed line

darwin %>% 
  summarise(mean=mean(height))
### check that the intercept has accurately estimated the mean

model2 <- lm(height~type, data=darwin)
model2
### a linear model that analyses a dependent variable (height) as a function of
### a categorical independent variable (type of pollination). 
### intercept now represents the mean height of crossed plants, second number 
### shows the difference in the mean height of the two groups (crossed and self)

darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()+
  geom_segment(aes(x=1, xend=2, y=20.192, yend=20.192-2.617), linetype="dashed")+
  stat_summary(fun.y=mean, geom="crossbar", width=0.2)
### relationship between groups is represented in our simple line equation by
### height=20.19−2.62(typeSelf). Plots the line equation is graph form.

darwin_relevel <- darwin %>% 
  mutate(type=factor(type, 
                     levels=c("Self",
                              "Cross")))
### sets the factor levels 

lm(height~type, data=darwin_relevel)
### states the new intercept and difference between the means

summary(model2)
### produces all test statistics

### intercept: mean value of one of the groups (whichever is 1st alphabetically)

### estimate: value of the intercept, and the difference between other 
### treatments and the intercept

### standard error: Standard error of the mean for the intercept and standard 
### error of the difference between the means

### degrees of freedom: number of values which are free to move while the mean
### stays the same. In essence it is n-the freedoms required for a particular
### test and is required to calculate probability from the test-statistic

### test statistics:F - the signal-to-noise ratio or Sum of squares/sum of 
### square errors. t-tests are carried out for each independent variable in the 
### ANOVA - here you can see that there is only one variable and the p-value 
### from the t-test and the F-test are the same. For simplicity F=t2.

### P-values: The probability of observing this signal-to-noise ratio at this 
### sample size if the null hypothesis were true

anova_test(height~type, data=darwin)
### performing a linear model when the independent variable is categorical 
### (a factor) is the same as running an ANOVA

pf(q=5.9395, df1=1, df2=28, lower.tail=FALSE)
### calculates probability of observing this signal-to-noise ratio with this 
### sample size

### write up: The cross-fertilised plants were taller on average than the 
### self-fertilised plants with a mean of 20.2 inches compared to 17.5 inches; 
### F1,28= 5.94, P = 0.02.