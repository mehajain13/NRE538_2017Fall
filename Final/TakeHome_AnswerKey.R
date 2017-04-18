library(dplyr)
library(fivethirtyeight)
library(car)

# load in datasets
flying = read.csv('/Users/mehajain/Desktop/flying.csv')
college = read.csv('/Users/mehajain/Desktop/college.csv')
happy = read.csv('/Users/mehajain/Desktop/happy.csv')
cancer = read.csv('/Users/mehajain/Desktop/cancer.csv')

# Q1 chi-square - flying
chisq.test(flying$gender,flying$unruly_child)

# Q2 ttest - college
qqPlot(college$sat)
var.test(college$sat~college$type)
t.test(sat~type,var.equal=FALSE,data=college)

# Q3 ANOVA - happiness
leveneTest(Hscore~Region,data=happy)
summary(aov(Hscore~Region,data=happy))
TukeyHSD(aov(Hscore~Region,data=happy))
## OR
summary(lm(Hscore~Region,data=happy))

# Q4 multiple linear regression - happiness
hist(happy$Corruption)
qqPlot(happy$Corruption)
hist(log(happy$Corruption))
qqPlot(log(happy$Corruption))

model=lm(log(Corruption)~GDP+Freedom+Generosity,data=happy)
vif(model)
summary(model)
plot(model)

# Q5 multiple linear regression w interaction term - happiness
model2=lm(log(Corruption)~Freedom*Region,data=happy)
summary(model2)
plot(model2)

# Q6 glm - breast cancer
model=glm(malignant~radius_mean+texture_mean+smoothness_mean,data=cancer,family='binomial')
summary(model)

# Q7 glm - breast cancer (extra credit)
# here it is key that they standardize their variables otherwise you cannot tell variable importance from the direct coefficients in Q6
