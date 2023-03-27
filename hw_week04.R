###statistical rethinking course 2023###

##########################
########## Week 03 #######
### Homework Questions ###
##########################

packages <- c('rstan','rethinking','ggplot2','ggdag', 'dagitty', 'data.table', 'tidyr' )
lapply(packages, require, character.only=TRUE)

###QUESTION 1
###

###from textbook
d <- sim_happiness( seed=1977 , N_years=1000 ) 
precis(d)

d2 <- d[ d$age>17 , ] # only adults 
d2$A <- ( d2$age - 18 ) / ( 65 - 18 )

d2$mid <- d2$married + 1
m6.9 <- quap( alist( happiness ~ dnorm( mu , sigma ),
                                              mu <- a[mid] + bA*A, 
                                              a[mid] ~ dnorm( 0 , 1 ), 
                                              bA ~ dnorm( 0 , 2 ), 
                                              sigma ~ dexp(1) ) , data=d2 ) 
precis(m6.9,depth=2)

m6.10 <- quap( alist( happiness ~ dnorm( mu , sigma ), 
                      mu <- a + bA*A, a ~ dnorm( 0 , 1 ), 
                      bA ~ dnorm( 0 , 2 ), 
                      sigma ~ dexp(1) ) , 
               data=d2 ) 
precis(m6.10)

###compare the PSIS and WAIC

compare( m6.9, m6.10, func = PSIS)
compare( m6.9, m6.10, func = WAIC)

####model 9 performs best but not causal

precis(m6.9, depth = 2)



###QUESTION 2

data(foxes)
d <- foxes
d$W <- standardize(d$weight)
d$A <- standardize(d$area)
d$F <- standardize(d$avgfood)
d$G <- standardize(d$groupsize)


m1 <- quap(
  alist(
    W ~ dnorm( mu , sigma ),
    mu <- a + bF*F + bG*G + bA*A,
    a ~ dnorm(0,0.2),
    c(bF,bG,bA) ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ), data=d )


m2 <- quap(
  alist(
    W ~ dnorm( mu , sigma ),
    mu <- a + bF*F + bG*G,
    a ~ dnorm(0,0.2),
    c(bF,bG) ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ), data=d )

m3 <- quap(
  alist(
    W ~ dnorm( mu , sigma ),
    mu <- a + bG*G + bA*A,
    a ~ dnorm(0,0.2),
    c(bG,bA) ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ), data=d )


m4 <- quap(
  alist(
    W ~ dnorm( mu , sigma ),
    mu <- a + bF*F,
    a ~ dnorm(0,0.2),
    bF ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ), data=d )


m5 <- quap(
  alist(
    W ~ dnorm( mu , sigma ),
    mu <- a + bA*A,
    a ~ dnorm(0,0.2),
    bA ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ), data=d )



compare( m1 , m2 , m3 , m4 , m5 , func=PSIS )


precis(m1)


###QUESTION 3

data(cherry_blossoms)

d <- cherry_blossoms
d$D <- standardize(d$doy)
d$T <- standardize(d$temp)
dd <- d[ complete.cases(d$D,d$T) , ]


m3a <- quap(
  alist(
    D ~ dnorm(mu,sigma),
    mu <- a,
    a ~ dnorm(0,10),
    sigma ~ dexp(1)
  ) , data=list(D=dd$D,T=dd$T) )


m3b <- quap(
  alist(
    D ~ dnorm(mu,sigma),
    mu <- a + b*T,
    a ~ dnorm(0,10),
    b ~ dnorm(0,10),
    sigma ~ dexp(1)
  ) , data=list(D=dd$D,T=dd$T) )


m3c <- quap(
  alist(
    D ~ dnorm(mu,sigma),
    mu <- a + b1*T + b2*T^2,
    a ~ dnorm(0,10),
    c(b1,b2) ~ dnorm(0,10),
    sigma ~ dexp(1)
  ) , data=list(D=dd$D,T=dd$T) )


compare( m3a , m3b , m3c , func=PSIS )


Tval <- (9 - mean(d$temp,na.rm=TRUE))/sd(d$temp,na.rm=TRUE)
D_sim <- sim( m3b , data=list(T=Tval) )
# put back on natural scale
doy_sim <- D_sim*sd(d$doy,na.rm=TRUE) + mean(d$doy,na.rm=TRUE)
dens( doy_sim , lwd=4 , col=2 , xlab="day in year 1st bloom")

abline(v=89,lty=1)
dens( d$doy , add=TRUE , lwd=3 )




