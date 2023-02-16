###statistical rethinking course 2023###
###WEEK 02####

##########################
####Homework Questions####
##########################

packages <- c('rstan','rethinking','ggplot2','ggdag', 'data.table', 'tidyr' )
lapply(packages, require, character.only=TRUE)


####Question 1#####

##Howell Data 

data(Howell1)

##younger than 13
d<-Howell1[Howell1$age < 13,]

##Estimate causal associations between height and weight
##first age influences height, height influences weight
##A->H, H->W
##second age influences weight through age related changes
## A -> W

##draw a DAG

coords <- data.frame(
  name = c('A', 'H', 'W'),
  x =    c(1,    1,   2),
  y =    c(1,    2,   2)
)

dag <- dagify(
  H ~ A,
  W ~ H + A,
  coords = coords
)

ggdag(dag) +
  theme_dag()

##write a generative simulation

sim_wheight<-function(H,A,b_ah,b_hw,b_aw) {
  N <- length(A)
  H <- rnorm(N, b_ah*A,3)
  W<-rnorm(N,b_hw*H + b_aw*A,2)
  return(data.table(age = A, height = H, weight = W))
}


A <- runif(1e3, 0, 12)


H<- 100 + rnorm(n,0,5)
summary(H)

dat <-sim_wheight(H,A,b_ah = 10, b_hw = 1, b_aw = 5)


ggplot(dat) + geom_point(aes(age, weight))
ggplot(dat) + geom_point(aes(age, height))
ggplot(dat) + geom_point(aes(height, weight))

####Question 2#####

##linear regression : total causal effect of year year of growth on weight


n <- 100
ggplot() + 
  geom_abline(aes(intercept = rnorm(n, 5, 1), 
                  slope = rnorm(n, 3, 1)),
                 # slope = runif(n, 0, 10)), ##why uniform versus normal
              alpha = 0.1, size = 2) + 
  labs(x = 'age', y = 'weight (kg)') + 
  xlim(0, 12) + 
  ylim(0, 50)


m2 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- alpha + b_aw * A,
    alpha ~ dnorm(5, 1),
    b_aw ~ dnorm(3,1),
    #b_aw ~ dunif(0,10),
    sigma ~ dexp(1)
  ),
  data = list(W=d$weight,A=d$age)
)

precis(m2)



####Question 3####

coords <- data.frame(
  name = c('A', 'H', 'W', 'S'),
  x =    c(1,    1,   2,   2),
  y =    c(1,    2,   1,   2)
)

dag <- dagify(
  H ~ A + S,
  W ~ H + A + S,
  coords = coords
)

ggdag(dag) +
  theme_dag()

d3 <-data.table(W=d$weight, A = d$age, S=d$male+1)

m3 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- alpha[S] + b_aw[S]* A,
    alpha[S] ~ dnorm(5, 1),
    b_aw[S] ~ dnorm(3,1),
    sigma ~ dexp(1)
  ),
  data = d3
)

precis(m3, depth=2)
precis(m3)

####contrast
###taken right from the solutions but not working
Aseq <- 0:12
mu1 <- sim(m3, data = list(A = Aseq, S = rep(1,13)))
mu2 <- sim(m3, data = list(A = Aseq, S = rep(2,13)))



mu_contrast <- mu1
for ( i in 1:13) mu_contrast[,1] <- mu2[,i] - mu1[,i]

plot(NULL, xlim=c(0,13), ylim = c(-15,15), xlab = "age", ylab = "weighted difference (boys-girls)")
for (p in c (0.5, 0.67, 0.89, 0.99))
shade(apply (mu_contrast,2,PI, prob = p), Aseq)
abline(h=0,lty = 2, lwd = 2)

