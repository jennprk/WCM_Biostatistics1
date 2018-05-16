# Biostatistics HW2
# Ji-Eun Park
# 10/04/2017

################
## Question 1 ##
################ 
# Part A
# pmf for dice A
x <- c(1:6)

pmf_a <- function(x){
  dunif(x,min=0,max=6,log=FALSE)
}

# pmf for dice B
pmf_b <- function(x) {
  ifelse(x==2|x==4|x==6,2/9,1/9)
}

# Part B
# cdf for die A
cdf_a <- function(x) {
  punif(x,min=0,max=6,log=FALSE)
}

# cdf for die B
cdf_b <- function(x) {
  for(n in x){
      print(sum(pmf_b(1:n)))}
}

################
## Question 2 ##
################
# Part A
twodiceb <- 2*(pmf_b(1)*pmf_b(6)+pmf_b(2)*pmf_b(5)+pmf_b(3)*pmf_b(4))
twodiceb
# The probability of throwing a seven when throwing two dice B is about 0.148.

# Part B
set.seed(1)
num10 <- rbinom(1,10,twodiceb)
num10
prob10 <- num10/10
prob10

num100 <- rbinom(1,100,twodiceb)
num100
prob100 <- num100/100
prob100

num1000 <- rbinom(1,1000,twodiceb)
num1000
prob1000 <- num1000/1000
prob1000

# The probability of 7s we get if we throw two dice Bs gets closer to the probability we got in part A as the number of times get bigger.

# Part C
library(ggplot2)
set.seed(2)
rbinom(1,10,twodiceb)

A <- rbinom(500,10,twodiceb)
dataframe_a <- as.data.frame(A)
colnames(dataframe_a) <- "times"
table(A)

ggplot(dataframe_a,aes(x=times)) +
  geom_histogram(stat="count",alpha=0.8, fill=c("lightblue1","lightblue2","lightblue3","lightblue4","lightblue3","lightblue4"))+
  labs(title="500 observations of unfair dice rolls for 10 times") +
  scale_x_continuous(breaks=0:5)
# The histogram shows a right-skewed distribution and getting 1 time out of 10 times is most observed.

# Part D
set.seed(3)
B <- rbinom(500,100,twodiceb)
dataframe_b <- as.data.frame(B)
colnames(dataframe_b) <- "times"
table(B)
ggplot(dataframe_b,aes(x=times)) +
  geom_histogram(stat="count",alpha=0.8)+
  labs(title="500 observations of unfair dice rolls for 100 times")

C <- rbinom(500,1000,twodiceb)
dataframe_c <- as.data.frame(C)
colnames(dataframe_c) <- "times"
table(C)
ggplot(dataframe_c,aes(x=times)) +
  geom_histogram(stat="count",alpha=0.8) +
  labs(title="500 observations of unfair dice rolls for 1000 times")
# Plots for 100 and 1000 times are also right skewed. It might seem that it is more symmetric as the graph does not the right tail but they are both highly right-skewed. The first plot shows that it is most likely to have 13 to 16 times out of 100times and the second plot shows that it is most likely to have 130 to 160 times out of 1000 times.

# Part E
# I would consider getting 76 times out of 100 times quite strange as our probability of getting a sum of 7 was only 0.148 and as we experimented above, my distribution was around 15 times out of 100 times. 
table(B)

# None of my simulations had 76 or more 7s.

prob <- 0
for(i in 76:100){
  prob <- prob  + dbinom(i,100,twodiceb)
}
prob

# The probability is basically zero.

# Therefore, I assume that you may have used a diﬀerent die than the one I have.



################
## Question 3 ##
################
getwd()
library(tidyr)
library(dplyr)
library(plyr)
setwd('~/Desktop')
getwd()
dat <- read.csv('plco_dat.csv')
dat

# Split up demographics 
dat_demographics <- dat[,1:5]
dat_demographics <- rename(dat_demographics, c("plco_id" = "ID", "age"="Age", "race7" = "Race", "state"= "State", "educat" = "Education"))

dat_demographics

# Split up PSA
dat_PSA <- dat[,c(1,6:11)]

dat_PSA <- rename(dat_PSA, c("plco_id" = "ID","psa_level0"="0", "psa_level1"="1", "psa_level2"="2", "psa_level3"="3", "psa_level4"="4", "psa_level5"="5"))

dat_PSA

# Split up DRE
dat_DRE <- dat[,c(1,12:15)]

dat_DRE <- rename(dat_DRE, c("plco_id" = "ID","dre_result0"="0", "dre_result1"="1", "dre_result2"="2", "dre_result3"="3"))

dat_DRE


# Long plco_dat_PSA
dat_PSA <- gather(dat_PSA, key = "Year", value = "PSA Result (ng/mL)", indexes = 2:7)
dat_PSA

# Long plco_dat_DRE
dat_DRE <- gather(dat_DRE, key = "Year", value = "DRE Result (ng/mL)", indexes = 2:5)
dat_DRE

# Join long plco_dat_DRE & long plco_dat_PSA together
joined_PSA_DRE <- full_join(dat_PSA, dat_DRE, by = c("ID", "Year"))

# Make a tidy data by joining the demographics and joined PSA & DRE by ID
dat_tidy <- full_join(dat_demographics, joined_PSA_DRE, by = "ID")

# Arrange it by year and id
dat_tidy <- dat_tidy %>%
  group_by(Year)%>%
  arrange(ID)

dat_tidy

################
## Question 4 ##
################

#MLE is a method of estimating the parameters of a statistical model given observations, by finding the parameter values that maximize the likelihood of making the observations given the parameters.

#The optimal MLE should have small variance and small bias, which could be calculatd with MSE which contains the variance and bias of the estimated parameter(in this formula, theta hat).

################
## Question 5 ##
################
# Part A
dat2 <- c(2.5303718, 1.7298308, 3.9871646, 0.0947321, 0.1686329 ,0.9495036, 0.9426819, 0.4358004, 8.1787094, 0.0874603, 3.0144902, 1.4406442, 0.8430409, 1.1313535, 0.5648521, 2.5493584, 4.6896106, 1.4362812, 1.7728045, 12.1230351)
theta <- seq(0.01,5,by=0.01)
n <- length(dat2)

likelihood <- function(theta, dat2) {
  theta^n*exp(-theta*sum(dat2))
}

nloglik <- function(theta,x) -sum(log(dexp(x= dat2,rate=theta)))
# The first equation f(x; is the pdf of exponential distribution.
# The second equation L( is the Likelihood function.
                       

# Part B
plot(theta, lapply(theta, likelihood, dat2), xlab = "Theta", ylab = "Likelihood", type = "l", main ="The likelihood function of exponential distribution")

plot(theta,lapply(theta,nloglik,dat2),type="l", xlab = "Theta", ylab = "Log-likelihood",main = "The log-likelihood function of exponential distribution")
# The likelihood plot can be plotted with using the likelihood function and the log-likelihood function.

# Part C

# From Part A’s likelihood function, we can derive the MLE of theta as shown below.

optimize(f=nloglik,x= dat2,interval = c(0,5))$minimum

theta_hat <- 1/mean(dat2)
theta_hat
