########## Question 2 ##########
library("foreign")
# Look at data
mydata <- read.dta("bm.dta")
summary(mydata)

# Compare the mean of each variable by black = 1 and black = 0 to see if
# there is any differences between the means
install.packages("sqldf")
library("sqldf")

black <- sqldf("select * from mydata where black = 1")
nonblack <- sqldf("select * from mydata where black = 0")

black1 <- mydata[mydata$black == 1, ]
nonblack1 <- mydata[mydata$black == 0, ]

t.test(black$yearsexp, nonblack$yearsexp, alternative = "two.sided", mu = 0) # insig
t.test(black$education, nonblack$education, alternative = "two.sided", mu = 0) #insig
t.test(black$ofjobs, nonblack$ofjobs, alternative = "two.sided", mu = 0) #insig
t.test(black$computerskills, nonblack$computerskills, alternative = "two.sided", mu = 0) #sig
t.test(black$female, nonblack$female, alternative = "two.sided", mu = 0) #insig

# Is there a significant difference between mean callback rates by race?
t.test(black$call, nonblack$call, alternative = "two.sided", mu = 0) #sig
t.test(black$call, nonblack$call, alternative = "less", mu = 0) #sig
t.test(black$call, nonblack$call, alternative = "greater", mu = 0) #sig

# Run regression on all variables (long regression)
mydata$education <- factor(mydata$education)
mydata$ofjobs <- factor(mydata$ofjobs)
mydata$yearsexp <- as.numeric(mydata$yearsexp)
mydata$computerskills <- factor(mydata$computerskills)
mydata$female <- factor(mydata$female)
mydata$black <- factor(mydata$black)

model_all <- lm(call ~ ., data = mydata)
summary(model_all)

# Run short regression
model_short <- lm(call ~ . - yearsexp, data = mydata)
summary(model_short)

# Run auxiliary regression
model_auxiliary <- lm(yearsexp ~ . - call, data = mydata)
summary(model_auxiliary)

# omitted variable bias
model_short$coefficients["computerskills1"] - model_all$coefficients["computerskills1"]
model_all$coefficients["yearsexp"] * model_auxiliary$coefficients["computerskills1"]

########## Question 3 ##########
install.packages("readstata13")
library(readstata13)
mydata2 <- read.dta13("cps.dta")
summary(mydata2)

# create a dummy variable on education
mydata2$educationclean <- ifelse(mydata2$education == "HSD" |
                                   mydata2$education == "HSG", 0, 1)
mean(mydata2$educationclean)

# compare mean education of black and mean education of white
black1 <- mydata2[mydata2$black == 1, ]
nonblack1 <- mydata2[mydata2$black == 0, ]

t.test(black1$educationclean, nonblack1$educationclean, alternative = "two.sided", mu = 0) #sig

# compare mean yearsexp of black and mean education of white
t.test(black1$yearsexp, nonblack1$yearsexp, alternative = "two.sided", mu = 0) #insig

# compare mean employed of black and mean employed of white. Drop NA's in employed
mydata4 <- read.dta13("cps.dta")
mydata4 <- mydata4[!is.na(mydata4$employed), ] #drop NA's in employed
summary(mydata4)
class(mydata4$employed)
mydata4$employedclean <- ifelse(mydata4$employed == "Yes", 1, 0)
summary(mydata4)
black2 <- mydata4[black == 1, ]
nonblack2 <- mydata4[black == 0, ]
t.test(black2$employedclean, nonblack2$employedclean, alternative = "two.sided", mu = 0) #insig

########## Question 8 ##########
mydata3 <- read.dta("CollegeDistance.dta")
summary(mydata3)
# short model
model3_short <- lm(ed ~ dist, data = mydata3)
summary(model3_short) #coeff estimate of dist = -0.07337
# long model w/ all variables
model3_long <- lm(ed ~ ., data = mydata3)
summary(model3_long) # coeff estimate of dist = -0.032586
# choose variables with moderate correlations with ed for testing model
cor(mydata3) #guessing sig variables: incomehi,momcoll,dadcoll,black,bytest, dist
model3_test <- lm(ed ~ incomehi + momcoll + dadcoll + black + bytest + dist, 
                  data = mydata3)
summary(model3_test) # coeff estimate of dist = -0.023367 --> don't use
# choose variables with significant correlations with ed for testing model 1
cor.test(mydata3$ed, mydata3$female) #not sig
cor.test(mydata3$ed, mydata3$black) #sig
cor.test(mydata3$ed, mydata3$hispanic) #not sig
cor.test(mydata3$ed, mydata3$bytest) #sig
cor.test(mydata3$ed, mydata3$dadcoll) #sig
cor.test(mydata3$ed, mydata3$momcoll) #sig
cor.test(mydata3$ed, mydata3$ownhome) #sig
cor.test(mydata3$ed, mydata3$urban) #not sig
cor.test(mydata3$ed, mydata3$cue80) #not sig
cor.test(mydata3$ed, mydata3$stwmfg80) #not sig
cor.test(mydata3$ed, mydata3$dist) #sig
cor.test(mydata3$ed, mydata3$tuition) #sig
cor.test(mydata3$ed, mydata3$incomehi) #sig
# use variables with significant correlations with ed for testing model
model3_test1 <- lm(ed ~ black + bytest + dadcoll + momcoll + ownhome + dist + tuition
                  + incomehi, data = mydata3)
summary(model3_test1) #coeff estimate of dist = -0.031016 
# omitted variable bias
-0.07337 - (-0.032586)
