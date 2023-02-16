###statistical rethinking course 2023###
###WEEK 01####

require(ggplot2)

#############################
##### Code from lectures#####
#############################
##lecture 01##
##############

#function to toss a globe covered by water N times
sim_globe <-function (p=0.7, N = 9){
  sample(c("W","L"), size = N, prob = c(p, 1-p), replace = TRUE)
}

#function to compute posterior distribution
compute_posterior <- function(the_sample, poss = c(0,0.25, 0.5, 0.75,1)) {
  W <- sum(the_sample=="W") #number of W observed
  L <- sum(the_sample =="L") #number of L observed
  ways <- sapply (poss, function(q) (q*4)^W * ((1-q)*4)^L)
  post <- ways/sum(ways)
 # bars <- sapply(post, function(q) make_bar(q))
  data.frame (poss, ways, post = round (post, 3), bars)
}


compute_posterior(sim_globe())

##############
##lecture 02##
##############

sample <- c("W","L","W", "W","W","L","W","L","W")
W <- sum(sample=="W") #number of W observed
L <- sum(sample=="L") #number of L observed
p <-  c(0,0.25,0.5,0.75,1)  #proportions of W
ways <- sapply (p, function(q) (q*4)^W * ((1-q)*4)^L)
prob <- ways/sum(ways)
cbind(p,ways,prob)

#function to toss a globe covered by water N times
sim_globe <-function (p=0.7, N = 9){
  sample(c("W","L"), size = N, prob = c(p, 1-p), replace = TRUE)
}

sim_globe()
sim_globe(p=1, N=11)

sum(sim_globe(p=0.5,N=1e4)=="W")/1e4

compute_posterior <- function(the_sample, poss = c(0,0.25, 0.5, 0.75,1)) {
  W <- sum(the_sample=="W") #number of W observed
  L <- sum(the_sample =="L") #number of L observed
  ways <- sapply (poss, function(q) (q*4)^W * ((1-q)*4)^L)
  post <- ways/sum(ways)
  # bars <- sapply(post, function(q) make_bar(q))
  data.frame (poss, ways, post = round (post, 3), bars)
}

post_samples <- rbeta (1e3,6+1, 3+1)
density(post_samples, lwd = 4, col = 2, xlab="propn water", adj = 0.1)
curve(dbeta(x,6+1, 3+1), add = TRUE, lty = 2, lwd = 3)

#now simulate posterior predictive
post_samples <-rbeta (1e4,6+1, 3+1)
pred_post <-sapply (post_samples, function(p) sum(sim_globe(0,10)=="W"))
tab_post <- table(pred_post)
for (i in 0:10) lines(c(i,i), c(0,tab_post[i+1]),led=4,col=4)

##########################
####Homework Questions####
##########################

##Q1 - globe tossing turned out to be 4 water and 11 land

###samples of water and land for 15 tosses
W <- 4
L <- 11

compute_posterior <- function(W,L, poss) {
  ways <- sapply (poss, function(q) (q*4)^W * ((1-q)*4)^L)
  post <- ways/sum(ways)
  # bars <- sapply(post, function(q) make_bar(q))
  data.frame (poss, ways, post = round (post, 3))
}


posterior <- compute_posterior(W = 4, L = 11, poss = c(0,0.25, 0.5, 0.75,1)) ##probability of 0.84 at .25

posterior2 <- compute_posterior(W = 4, L = 11, poss = seq(0,1, by = 0.01)) ##curve looks cuter


##plot
ggplot(data = posterior2) + 
  geom_line(aes(poss, post)) + 
  theme_classic() + 
  xlab("proportion water") + 
  ylab("probability")


##Q2 - compute the posterior predictive distribution for the next 5 tosses of the same globe

sim_globe <-function (p, N){
  sample(c("W","L"), size = N, prob = c(p, 1-p), replace = TRUE)
}


post_samples <-rbeta (1e4,4+1, 11+1) ##main issue was this, needed help to know to do the 4+1 and 11+1

#####
###write code that samples from posterior distribution##
####

pred_post <-sapply (post_samples, function(p) sum(sim_globe(p, 5)=="W"))

tab_post <- table(pred_post)

tab_post_df <- as.data.frame(tab_post)


ggplot(data = tab_post_df) + 
  geom_col(aes(x=pred_post, y=Freq)) + 
  theme_classic() + xlab('Water Samples') + ylab('Count')


##Q3 - calculate the the probability of of 3 or more waters samples in the next 5 tosses

tab_post_df$prob = tab_post_df$Freq/1e4

###water tosses 'pred_post' is factor, as.numeric doesn't work?
with(tab_post_df, sum(prob[pred_post ==c("3","4", "5")])) ### 0.1797


## Q4 OPTIONAL


