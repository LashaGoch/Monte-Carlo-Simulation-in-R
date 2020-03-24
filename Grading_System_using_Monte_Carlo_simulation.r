# TASK OVERVIEW:
# Create grading system based on Monte Carlo simulation to find the best set of weights for us. 
# Assume something about the distributions of your grades from exam, assignments and activity 
# and compare different sets of weights.   
# What will be for you the criterium of the goodness or the results? Maximizing of the expected rating or minimalization of failure? 

# SOLUTION:
# install package: triangle 
requiredPackages = c("triangle") # list of required packages
for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)} 
for(i in requiredPackages){library(i,character.only = TRUE) } 

# using triangular distribution for grades and weights avoids
# exam grade not to drop below 30. Same with activity & assignment grades

N <- 1000
# Expected grades
exam <- rtriangle(N, 30, 80, 70)
act <- rtriangle(N, 30, 90, 85)
assign <- rtriangle(N, 30, 95, 90)


#data.frame(exam, act, assign)
#View(data.frame(exam, act, assign))

# histograms for grades 2 by 2 graph
par(mfrow=c(2,2))
hist(exam)
hist(act)
hist(assign)

# sets of weights
w.exam <- rtriangle (N, 0.35, 0.6, 0.5)
w.act <- rtriangle (N, 0.15, 0.35, 0.25)
w.assign <- rtriangle (N, 0.15, 0.4, 0.3)

w.sum <- w.exam + w.act + w.assign

# run ifelse to adjust weights to set sum equal to 1
w.ex <- w.exam - (w.sum-1)/(w.sum/w.exam)
ifelse(w.sum <= 1, w.exam/w.sum, w.exam - (w.sum-1)/(w.sum/w.exam))

w.ac <- w.act - (w.sum-1)/(w.sum/w.act)
ifelse(w.sum <= 1, w.act/w.sum, w.act - (w.sum-1)/(w.sum/w.act))

w.as <- w.assign - (w.sum-1)/(w.sum/w.assign)
ifelse(w.sum <= 1, w.assign/w.sum, w.assign - (w.sum-1)/(w.sum/w.assign))


data.frame(exam, act, assign, w.exam, w.act, w.assign, w.exam+w.act+w.assign, w.ex, w.ac, w.as, w.ex+w.ac+w.as)
View(data.frame(exam, act, assign, w.exam, w.act, w.assign, w.exam+w.act+w.assign, w.ex, w.ac, w.as, w.ex+w.ac+w.as))

# histograms for weights
#hist(w.exam)
#hist(w.act)
#hist(w.assign)

# histograms for adjusted weights
hist(w.ex, main="Weight of exam")
hist(w.ac, main="Weight of activity")
hist(w.as, main="Weight of assignment")

mark = exam * w.ex + act * w.ac + assign * w.as


# summary of marks and adjusted weights
summary(mark)
summary(exam)
summary(act)
summary(assign)
summary(w.ex)
summary(w.ac)
summary(w.as)

# means for weights
mean(w.ex)
mean(w.ac)
mean(w.as)

# final weights for three sections:
# exam 0.4743569
# activity 0.2467799
# assignment 0.2788632
# focus is on maximizing the results.
