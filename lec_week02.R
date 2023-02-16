###statistical rethinking course 2023###
###WEEK 02####

#############################
##### Code from lectures#####
#############################
##lecture 03##
##############

library(rethinking)
data(Howell1)

d<- Howell1[Howell1$age>=18,]
d2 <- Howell1

#function to simulate weights of individuals from height
sim_weight <-function(H,b,sd) {
  U <- rnorm (length(H), 0, sd)
  W <- b*H + U
  return(W)
}

H<-runif(200,min=130,max=170)
W<-sim_weight(H,b=0.5, sd =5)
plot(W~H,col=2,lwd=3)

###quadratic approximation
m3.1 <- quap(
  alist(
    W ~ dnorm(mu,sigma),
    mu <- a + b*H,
    a ~ dnorm(0,10), ##when there is no observations what do we believe  ##normal
    b ~ dunif(0,1), ##priors ##uniform
    sigma ~ dunif(0,10)  
  ), data =list(W=W,H=H)
)

n<-1000
a <- rnorm(n,0,10)
b <- runif(n,0,1)
plot(NULL, xlim=c(130,170), ylim = c(50,90) , xlab = "height", ylab = "weight")
for (j in 1:50) abline (a = a[j], b = b[j], lwd = 2, col = 2)

###simulate sample fo 10 people

set.seed(93)
H<-runif(10,130,170)
W<-sim_weight(H,b=0.5,sd=5)

##run the model

m3.1 <- quap(
  alist(
    W ~ dnorm(mu,sigma),
    mu <- a + b*H,
    a ~ dnorm(0,10), ##when there is no observations what do we believe  ##normal
    b ~ dunif(0,1), ##priors ##uniform
    sigma ~ dunif(0,10)  
  ), data =list(W=W,H=H)
)

##summary
precis(m3.1)

dat<-list(W=d2$weight, H=d2$height)


############Not working below### probably some typos

m3.2 <- quap(
  alist(
    W ~ dnorm(mu,sigma),
    mu <- a + b*H,
    a ~ dnorm(0,10), 
    b ~ dunif(0,1), 
    sigma ~ dunif(0,10)  
  ), data =dat
)

precis(m3.2)

post <- extract.samples (m3.2)
plot(d2$height,d2$weight,col=2,lwd=3)
for (j in 1:20) abline (a = a[j], b = b[j], lwd = 2)

height_seq <- seq(130,190,lens =20)
W_postpred<- sim(3.2, data = list(Height=height_seq))
W_PI <- apply (W_postpred,2,PI)
lines(height_seq,W_PI[1,])
lines(height_seq,W_PI[2,])



######lecture 2
#S = 1 F; S=2 M ##why not 0 and 1 for sex
sim_HW <- function(S,b,a){
  N <- length(S)
  H <- ifelse(S==1,150,160) +rnorm(N,0,5)
  W <- a[S] +b[S]*H + rnorm(N,0,5)
  data.frame(S,H,W)
}

S <- rbern(100) +1
dat<-sim_HW(S,b=c(0.5,0.6),a=c(0,0))
head(dat)

#female sample

S <- rep(1,100)
simF <- sim_HW(S,b=c(0.5,0.6),a=c(0,0))

#male sample
S <- rep(2,100)
simM <- sim_HW(S,b=c(0.5,0.6),a=c(0,0))

##effect of m-f
mean(simM$W-simF$W)

##observe sample

S<- rbern(100)+1
dat <- sim_HW(S,b=c(0.5,0.6),a=c(0,0))

##estimate posterior

m_SW <-quap(
  alist(
    W ~ dnorm(mu,sigma),
    mu <- a[S],
    a[S] ~dnorm(60,10),
    sigma ~ dunif(0,10)
  ), data = dat )
precis(m_SW,depth=2)


