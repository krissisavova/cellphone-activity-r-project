X71912_AnswersOfQuestions <- read.csv("71912-AnswersOfQuestions.csv")
colnames(X71912_AnswersOfQuestions) <- paste("Question", 1:ncol(X71912_AnswersOfQuestions), sep = "_")
str(X71912_AnswersOfQuestions)

#$ Question 1: chr [1:25] "Female" "Male" "Female" "Female" ...
#$ Question 2: chr [1:25] "18-24" "18-24" "24-30" "18-24" ...
#$ Question 3: num [1:25] 5 6 3 5 4 3 4 3 4 2 ...
#$ Question 4: num [1:25] 50 80 30 70 60 60 50 40 50 20 ...
#$ Question 5: chr [1:25] "None" "None" "Google Meet" "Slack, Teams" ...
#$ Question 6: chr [1:25] "Slack, Messenger" "Slack, Discord" "Google Chat" "Slack, Messenger" ...
#$ Question 7: num [1:25] 1 0 0 2 0 2 2 0 2 1 ...
#$ Question 8: chr [1:25] "Yes" "Yes" "Yes" "Yes" ...
#- attr(*, "spec")=
#        .. cols(
#                ..   gender = col_character(),
#                ..   ageRange = col_character(),
#                ..   hoursOfPhoneActivity = col_double(),
#                ..   PhoneDependencyInPerc = col_double(),
#                ..   mostUsedVideoCallApp = col_character(),
#                ..   appForMessagingColleagues = col_character(),
#                ..   phoneCallsForWorkDaily = col_double(),
#                ..   accessingAppsFromPhone = col_character()
#                .. )

View(X71912_AnswersOfQuestions) 

# Question 1: What is your gender?
table_q1 <- table(X71912_AnswersOfQuestions$Question_1)
table_q1 # Here we can see that 14 women and 11 men have given responses to the research questions

#Female   Male 
#14     11 

barplot(round(prop.table(table_q1)*100, 2), col = "red", main = "What is your gender?", xlab = "Types of Genders",
        ylab = "In percentage", ylim = c(0, 100))


# Question 2: How old are you (choose one of the possible ranges)?
table_q2 <- table(X71912_AnswersOfQuestions$Question_2)
table_q2 # Here we can see how many people have filled in the survey, based on their age ranges

#18-24 24-30 30-36 
#11     9     5 

barplot(round(prop.table(table_q2)*100, 2), col = "darkgreen", 
        main = "How old are you (choose one of the possible ranges)?", 
        xlab = "Age Ranges", ylab = "In percentage", ylim = c(0, 100))

# Question 3: How many hours per day (give an approximate number) do you use your cellphone?
hoursOfActivity_q3 <- c(5, 6, 3, 5, 4, 3, 4, 3, 4, 2, 5, 3, 4, 5, 4, 5, 4, 4, 4, 5, 3, 6, 4, 5, 3)
length(hoursOfActivity_q3) # The number of responses on this question is 25.
# [1] 25

median(hoursOfActivity_q3) # calculating the median
# [1] 4

mean(hoursOfActivity_q3) # calculating the average value
# [1] 4.12

table_q3 <- table(hoursOfActivity_q3) # calculating mode (or the most common value)
names(table_q3)[table_q3 == max(table_q3)]
# [1] "4"
table_q3
# hoursOfActivity_q3
# 2 3 4 5 6 
# 1 6 9 7 2

summary(hoursOfActivity_q3)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.00    3.00    4.00    4.12    5.00    6.00

quantile(hoursOfActivity_q3, prob = seq(0.1, 1.0, by = 0.1))
# 10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
#  3    3    4    4    4    4    5    5    5    6 

range(hoursOfActivity_q3)   # range: shows the smallest and the largest values
# [1] 2 6

var(hoursOfActivity_q3)
# [1] 1.026667

sd(hoursOfActivity_q3)      # standard distribution
# [1] 1.013246

fivenum(hoursOfActivity_q3)
# [1]  2  3  4  5  6

# Graphical representation
hist(table_q3, main = "How many hours per day (give an approximate number) do you use your cellphone?", 
     xlab = "normal distribution (hours)",
     ylab = "Amount of people", col = "blue")

d1 <- rnorm(n = 10^2, mean = mean(hoursOfActivity_q3), sd = sd(hoursOfActivity_q3))
qqplot(hoursOfActivity_q3, d1, main = "Checking Normal Distribution")
abline(a = 0, b = 1)

# Question 4: In percentage, how dependent on your cellphone do you feel?
dependencyPercentage_q4 <- c(50, 80, 30, 70, 60, 60, 50, 40, 50, 20, 50, 25, 40, 60, 50, 60, 40, 50, 50, 60, 30, 75, 50, 40, 30)
length(dependencyPercentage_q4) # The number of responses on this question is 25.
# [1] 25

median(dependencyPercentage_q4) # calculating the median
# [1] 50

mean(dependencyPercentage_q4) # calculating the average value
# [1] 48.8

table_q4 <- table(dependencyPercentage_q4) # calculating mode (or the most common value)
names(table_q4)[table_q4 == max(table_q4)]
# [1] "50"
table_q4
# dependencyPercentage_q4
# 20 25 30 40 50 60 70 75 80 
# 1  1  3  4  8  5  1  1  1

summary(dependencyPercentage_q4)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 20.0    40.0    50.0    48.8    60.0    80.0

quantile(dependencyPercentage_q4, prob = seq(0.1, 1.0, by = 0.1))
# 10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
#  30   38   40   50   50   50   58   60   66   80 

range(dependencyPercentage_q4)   # range: shows the smallest and the largest values
# [1] 20 80

var(dependencyPercentage_q4)
# [1] 229.75

sd(dependencyPercentage_q4)      # standard distribution
# [1] 15.15751

fivenum(dependencyPercentage_q4)
# [1] 20 40 50 60 80

# Graphical representation
hist(table_q4, main = "In percentage, how dependent on your cellphone do you feel?", xlab = "normal distribution",
     ylab = "Amount of people", col = "yellow")

d2 <- rnorm(n = 10^2, mean = mean(dependencyPercentage_q4), sd = sd(dependencyPercentage_q4))
qqplot(dependencyPercentage_q4, d2, main = "Checking Normal Distribution")
abline(a = 0, b = 1)

# Question 5: What app(s) do you use daily for your video calls at work?

q5_answer1 <- "Slack Video"
q5_answer2 <- "Zoom"
q5_answer3 <- "Teams"
q5_answer4 <- "Google Meet"
q5_answer5 <- "Skype"
q5_answer6 <- "None"
question_5 <- c(rep(q5_answer1, 11), rep(q5_answer2, 6), rep(q5_answer3, 7), rep(q5_answer4, 9),
                rep(q5_answer5, 1), rep(q5_answer6, 1))

table_q5 <- table(question_5) # creating a table with all answers
str(table_q5)

#'table' int [1:6(1d)] 9 1 1 11 7 6
#- attr(*, "dimnames")=List of 1
#..$ question_5: chr [1:6] "Google Meet" "None" "Skype" "Slack" ...

str(round(prop.table(table_q5)*100, 2))

row.names(table_q5) <- paste("Option", 1:nrow(table_q5), sep = " ")
barplot(round(prop.table(table_q5)*100, 2), col = "orange", 
        main = "What app(s) do you use daily for your video calls at work?",
        ylim = c(0, 100), ylab = "Percentage")

legend(x = "topright", legend = c("Option 1: Google Meet",
                                  "Option 2: None","Option 3: Skype", "Option 4: Slack Video",
                                  "Option 5: Teams", "Option 6: Zoom"), cex = 0.9, text.width = 5)

# Question 6: Which app(s) do you use daily to message your colleagues?

q6_answer1 <- "Slack Messaging"
q6_answer2 <- "Messenger Messaging"
q6_answer3 <- "Discord Messaging"
q6_answer4 <- "Google Chat Messaging"
q6_answer5 <- "Skype Messaging"
q6_answer6 <- "Other"
question_6 <- c(rep(q6_answer1, 16), rep(q6_answer2, 9), rep(q6_answer3, 4), rep(q6_answer4, 4),
                rep(q6_answer5, 1), rep(q6_answer6, 1))

table_q6 <- table(question_6) # creating a table with all answers
str(table_q6)

str(round(prop.table(table_q6)*100, 2))

row.names(table_q6) <- paste("Option ", 1:nrow(table_q6))
barplot(round(prop.table(table_q6)*100, 2), col = "purple", 
        main = "Which app(s) do you use daily to message your colleagues?",
        ylim = c(0, 100), ylab = "Percentage")

legend(x = "topright", 
       legend = c("Option 1: Discord Messaging",
                                  "Option 2: Google Chat Messaging","Option 3: Messenger Messaging", 
                  "Option 4: Other", "Option 5: Skype Messaging", "Option 6: Slack Messaging"), 
                  cex = 0.9, text.width = 5)

# Question 7: How many phone calls do you make a day (approximately)?
numOfPhoneCalls_q7 <- c(1, 0, 0, 2, 0, 2, 2, 0, 2, 1, 0, 5, 2, 1, 0, 0, 1, 3, 2, 1, 0, 0, 5, 0, 1)
length(numOfPhoneCalls_q7) # The number of responses on this question is 25.
# [1] 25

mean(numOfPhoneCalls_q7) # calculating the average value
# [1] 1.24

median(numOfPhoneCalls_q7) # calculating the median
# [1] 1

table_q7 <- table(numOfPhoneCalls_q7) # calculating mode (or the most common value)
names(table_q7)[table_q7 == max(table_q7)]
# [1] "0"
table_q7
# numOfPhoneCalls_q7
#  0  1  2  3  5 
# 10  6  6  1  2 

summary(numOfPhoneCalls_q7)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    0.00    1.00    1.24    2.00    5.00 

quantile(numOfPhoneCalls_q7, prob = seq(0.1, 1.0, by = 0.1))
# 10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
# 0.0  0.0  0.0  0.6  1.0  1.0  2.0  2.0  2.6  5.0 

range(numOfPhoneCalls_q7)   # range: shows the smallest and the largest values
# [1] 0 5

var(numOfPhoneCalls_q7)
# [1] 2.106667

sd(numOfPhoneCalls_q7)      # standard distribution
# [1] 1.451436

fivenum(numOfPhoneCalls_q7)
# [1] 0 0 1 2 5

hist(table_q7, main = " How many phone calls do you make a day (approximately)?", 
     xlab = "Amount of people",
     ylab = "normal distribution (number)", col = "pink")

d3 <- rnorm(n = 10^2, mean = mean(numOfPhoneCalls_q7), sd = sd(numOfPhoneCalls_q7))
qqplot(numOfPhoneCalls_q7, d3, main = "Checking Normal Distribution")
abline(a = 0, b = 1)

# Question 8: Do you use your phone to access the apps for messaging / having video calls for work?

table_q8 <- table(X71912_AnswersOfQuestions$Question_8)
table_q8

# No Yes 
# 9  16 

barplot(round(prop.table(table_q8)*100, 2), col = "red", main = "Do you use your phone to access the apps for messaging / having video calls for work?", xlab = "Answers",
        ylab = "Percentage", ylim = c(0, 100))

# Categorical - Categorical
# For this part of our research we can use the data from Question 5 and Question 6

question_2 <- c(rep("18-24", 11), rep("24-30", 9), rep("30-36", 5))
sampleForQ2 <- sample(x = question_2, size = 35, replace = TRUE)
table(sampleForQ2, question_6)

# question_6
# sampleForQ5   Discord Google Chat Messenger None Skype Slack
# Google Meet       0           2         3    0     0     2
# None              0           0         0    0     0     1
# Skype             0           0         0    0     0     1
# Slack             1           1         1    0     1     7
# Teams             1           0         3    1     0     2
# Zoom              2           1         2    0     0     3

prop.table(x = table(sampleForQ2, question_6), margin = 1)
# Graphical Represenation
barplot(prop.table(x = table(sampleForQ2, question_6), margin = 1), beside = TRUE)

legend(x = "topleft", legend = c("Black: 18-24",
                                  "Grey: 24-30", "White: 30-36"))

# Categorical - Numeric

sampleForQ4 <- sample(x = dependencyPercentage_q4, size = 25)
ttest <- boxplot(sampleForQ4~question_2) # t-test performance

# Numeric - Categorical

# H0: The phone activity depends on the sex of a person.
# H1: The phone activity has nothing to do with people's sexes.
wilcox.test(table_q3, table_q1)

wilcox.test(x = dependencyPercentage_q4, y = table_q1, mu = 0, alt = "greater", conf.int = TRUE)

# Numeric - Numeric
# correlation analysis:
RHO <- round(cor(hoursOfActivity_q3, numOfPhoneCalls_q7), 3)
par(mfrow = c(1, 1))
cor(hoursOfActivity_q3, numOfPhoneCalls_q7)
# [1] -0.2470544

cor.test(hoursOfActivity_q3, numOfPhoneCalls_q7, method = "spearman", exact = FALSE, alternative = "greater")

# Graphic Representation
plot(hoursOfActivity_q3, numOfPhoneCalls_q7, main = "Correlation ??nalysis")
abline(a = 0, b = 1, col = "red")

# Linear Regression
dataframe <- data.frame(hoursOfActivity_q3, dependencyPercentage_q4)
model <- lm(hoursOfActivity_q3~dependencyPercentage_q4)
model

#Call:
#        lm(formula = hoursOfActivity_q3 ~ dependencyPercentage_q4)

#Coefficients:
#        (Intercept)  dependencyPercentage_q4  
#1.47733                  0.05415  

summary(model)

#Call:
#        lm(formula = hoursOfActivity_q3 ~ dependencyPercentage_q4)

#Residuals:
#        Min      1Q  Median      3Q     Max 
#-1.7265 -0.1850 -0.1019  0.2735  1.3565 

#Coefficients:
#        Estimate Std. Error t value Pr(>|t|)    
#(Intercept)             1.477330   0.416865   3.544  0.00173 ** 
#        dependencyPercentage_q4 0.054153   0.008172   6.626 9.24e-07 ***
#        ---
#        Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.6068 on 23 degrees of freedom
#Multiple R-squared:  0.6563,	Adjusted R-squared:  0.6413 
#F-statistic: 43.91 on 1 and 23 DF,  p-value: 9.244e-07
