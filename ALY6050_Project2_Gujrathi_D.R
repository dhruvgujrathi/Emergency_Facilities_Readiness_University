#ALY6050 Week 2 Project_DHRUV VIJAY GUJRATHI
#Part 1
#Installing EnvStats Package
install.packages("EnvStats")
#Loading library EnvStats
library(EnvStats)
#Generating 5000 variables for analysis lying between the triangulation variables
x <- rtri(5000,20,300,80)
#Displaying the 5000 variables
x
#No. of victims expected at Beth Isarel hospital
bi <- x*0.3
bi
#No. of victims expected at Tufts Medical hospital
tm <- x*0.15
tm
#No. of victims expected at Massachussetts General hospital
mg <- x*0.20
mg
#No. of victims expected at Boston Medical hospital
bm <- x*0.25
bm
#No. of victims expected at Brigham and Women's hospital
bw <- x*0.10
bw
#Calculating the mean of patients at the Beth Isarel Hospital
mean_bi = mean(bi)
mean_bi
#Calculating the mean of patients at the Tufts Medical Hospital
mean_tm = mean(tm)
mean_tm
#Calculating the mean of patients at the Mass General Hospital
mean_mg = mean(mg)
mean_mg
#Calculating the mean of patients at the Boston Medical Hospital
mean_bm = mean(bm)
mean_bm
#Calculating the mean of patients at the Brigham and Women's Hospital
mean_bw = mean(bw)
mean_bw
#The average total time needed to transport all victims
total_time_bi =sum(rexp(mean_bi[1], 1/7))
total_time_bi <- total_time_bi/60
total_time_bi
#The average total time needed to transport all victims
total_time_tm =sum(rexp( tm[1], 1/10))
total_time_tm <- total_time_tm/60
total_time_tm
#The average total time needed to transport all victims
total_time_mg =sum(rexp( mg[1], 1/15))
total_time_mg <- total_time_mg/60
total_time_mg
#The average total time needed to transport all victims
total_time_bm =sum(rexp( bm[1], 1/15))
total_time_bm <- total_time_bm/60
total_time_bm
#The average total time needed to transport all victims
total_time_bw =sum(rexp( bw[1], 1/20))
total_time_bw <- total_time_bw/60
total_time_bw
#Displaying the law of large nos. chart for Beth Isarel Hospital
bi_ln <- bi
b_avg <- function(n) {
  mean(sample(bi_ln, size = n, replace = TRUE))
}
b_avg
#Plotting for displaying the law of large nos. chart for Beth Isarel Hospital
plot(sapply(1:5000, b_avg), type = "l", xlab = "No. of victims", ylab = "average")
abline(h = 0.2)
#Calculating the +/- 95% confidence interval  for time taken by a patient for reaching the Beth Isarel Hospital
EV1 = (mean(bi)+1.96*sd(bi))/sqrt(5000)
EV1
EV2 = (mean(bi)-1.96*sd(bi))/sqrt(5000)
EV2
#Probability distribution that best fits the total transport time
bi_probdis <- rnorm(5000, mean = mean(bi), sd = sd(bi))
round(bi_probdis)
hist(bi_probdis, prob = TRUE)
#Frequency distribution of the total travel time to and from Beth Isarel Hospital
bi_freqdist <- as.data.frame(table(round(bi_probdis)))
bi_freqdist
plot(bi_freqdist)
#Performing a Chi-squared Goodness of fit test for the total travel time to and from Beth Isarel Hospital
summary(bi_freqdist)
breaks <- c(-25, -15, -5, 5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)
tags <- c("[-25--15)","[-15--5)", "[-5-5)", "[5-15)", "[15-25)", "[25-35)","[35-45)", "[45-55)","[55-65)", "[65-75)", "[75-85)", "[85-95)", "[95-105)", "[105-115)")
group_tags <- cut(bi_freqdist$Freq, breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags )
summary(group_tags)
#chisq.test(table(group_tags), p = c(0,24,19,11,8,7,5,6,4,6,3,11,4)/108)
#Assigning the avg total travel time of victims to 't'
t <- total_time_bi
t
#Performing the exploratory data analysis of 't'
#Installing the following Packages
install.packages("tidyverse")
install.packages("funModeling")
install.packages("Hmisc")
#Loading the needed libraries
library(funModeling) 
library(tidyverse) 
library(Hmisc)
#Printing the status of 't'
print(status(t))
#Calculating the frequency of 't'
freq(t) 
#Printing the profiling number of 't'
print(profiling_num(t))
#Describing the variable 't'
describe(t)

#Part 2
#Generating 5000 variables for analysis with mean = 150 and SD=50
m <- rnorm(5000, mean=150, sd=50) 
#Displaying the 5000 variables
m
#No. of victims expected at Beth Isarel hospital
bi_m <- m*0.3
bi
#No. of victims expected at Tufts Medical hospital
tm_m <- m*0.15
tm
#No. of victims expected at Massachussetts General hospital
mg_m <- m*0.20
mg
#No. of victims expected at Boston Medical hospital
bm_m <- m*0.25
bm
#No. of victims expected at Brigham and Women's hospital
bw_m <- m*0.10
bw
#Calculating the mean of patients at the Beth Isarel Hospital
mean_bi_m = mean(bi_m)
mean_bi_m
#Calculating the mean of patients at the Tufts Medical Hospital
mean_tm_m = mean(tm_m)
mean_tm_m
#Calculating the mean of patients at the Mass General Hospital
mean_mg_m = mean(mg_m)
mean_mg_m
#Calculating the mean of patients at the Boston Medical Hospital
mean_bm_m = mean(bm_m)
mean_bm_m
#Calculating the mean of patients at the Brigham and Women's Hospital
mean_bw_m = mean(bw_m)
mean_bw_m
#The average total time needed to transport all victims
total_time_bi_m =sum(rexp( bi_m[1], 1/7))
total_time_bi_m <- total_time_bi_m/60
total_time_bi_m
#The average total time needed to transport all victims
total_time_tm_m =sum(rexp( tm_m[1], 1/10))
total_time_tm_m <- total_time_tm_m/60
total_time_tm_m
#The average total time needed to transport all victims
total_time_mg_m =sum(rexp( mg_m[1], 1/15))
total_time_mg_m <- total_time_mg_m/60
total_time_mg_m
#The average total time needed to transport all victims
total_time_bm_m =sum(rexp( bm_m[1], 1/15))
total_time_bm_m <- total_time_bm_m/60
total_time_bm_m
#The average total time needed to transport all victims
total_time_bw_m =sum(rexp( bw_m[1], 1/20))
total_time_bw_m <- total_time_bw_m/60
total_time_bw_m
#Displaying the law of large nos. chart for Beth Isarel Hospital
bi_ln_m <- bi_m
bi_avg_m <- function(n) {
  mean(sample(bi_ln_m, size = n, replace = TRUE))
}
#Plotting for displaying the law of large nos. chart for Beth Isarel Hospital
plot(sapply(1:5000, bi_avg_m), type = "l", xlab = "No. of victims", ylab = "average")
abline(h = 0.5, col = "red")

#Calculating the +/- 95% confidence interval  for time taken by a patient for reaching the Beth Isarel Hospital
EV1 = (mean(bi_m)+1.96*sd(bi))/sqrt(5000)
EV1
EV2 = (mean(bi_m)-1.96*sd(bi))/sqrt(5000)
EV2
#Probability distribution that best fits the total transport time
bi_probdis_m <- rnorm(5000, mean = mean(bi_m), sd = sd(bi_m))
round(bi_probdis_m)
hist(bi_probdis_m, prob = TRUE)
#Frequency distribution of the total travel time to and from Beth Isarel Hospital
bi_freqdist_m <- as.data.frame(table(round(bi_probdis_m)))
bi_freqdist_m
plot(bi_freqdist_m)
#Performing a Chi-squared Goodness of fit test for the total travel time to and from Beth Isarel Hospital
summary(bi_freqdist_m)
breaks <- c(-25, -15, -5, 5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)
tags <- c("[-25--15)","[-15--5)", "[-5-5)", "[5-15)", "[15-25)", "[25-35)","[35-45)", "[45-55)","[55-65)", "[65-75)", "[75-85)", "[85-95)", "[95-105)", "[105-115)")
group_tags_m <- cut(bi_freqdist_m$Freq, breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags )
summary(group_tags_m)

#chisq.test(table(group_tags_m), p = c(19,18,6,6,15,4,4,8,4,3,3,5,4)/99)

#Assigning the avg total travel time of victims to 't_m'
t_m <- total_time_bi_m
t_m
#Printing the status of 't_m'
print(status(t_m))
#Calculating the frequency of 't_m'
freq(t_m) 
#Printing the profiling number of 't_m'
print(profiling_num(t_m))
#Describing the variable 't_m'
describe(t_m)


