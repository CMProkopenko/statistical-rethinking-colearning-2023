###statistical rethinking course 2023###

##########################
########## Week 03 #######
### Homework Questions ###
##########################

packages <- c('rstan','rethinking','ggplot2','ggdag', 'dagitty', 'data.table', 'tidyr' )
lapply(packages, require, character.only=TRUE)


data(foxes)
setDT(foxes)

d$W <- standardize(d$weight)
d$A <- standardize(d$area)
d$F <- standardize(d$avgfood)
d$G <- standardize(d$groupsize)


foxes[,  `:=`(W = standardize(weight),
              A = standardize(area),
              F = standardize(avgfood),
              G = standardize(groupsize))
          ]

### Question 1

m1 <- quap(
  alist(
    F ~ dnorm( mu , sigma ),
    mu <- a + bA*A,
    a ~ dnorm(0,0.2),
    bA ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ), data = foxes )

precis(m1)

#### Question 2

m2 <- quap(
  alist(
    W ~ dnorm( mu , sigma ),
    mu <- a + bF*F,
    a ~ dnorm(0,0.2),
    bF ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ), data = foxes )

precis(m2)


###Question 3

m3 <- quap(
  alist(
    W ~ dnorm( mu , sigma ),
    mu <- a + bF*F + bG*G,
    a ~ dnorm(0,0.2),
    bF ~ dnorm(0,0.5),
    c(bF,bG) ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ), data = foxes )

precis(m3)


m3b <- quap(
  alist(
    G ~ dnorm( mu , sigma ),
    mu <- a + bF*F,
    a ~ dnorm(0,0.2),
    bF ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ), data = foxes )
precis(m3b)



###Question 4

coords <- data.frame(
  name = c('A', 'F', 'G', 'W', 'U'),
  x =    c(1,    1,   2,    1.5,   2),
  y =    c(3,    2,   2,    1,   3)
)


dag <- dagify(
  W ~ F + G,
  F ~ A + U,
  G ~ F + U,
  coords = coords,
  latent = "U"
)

dag |> ggdag(seed = 2) + theme_dag()

adjustmentSets(dag, exposure = 'F', outcome = 'W', effect = 'total')

adjustmentSets(dag, exposure = 'F', outcome = 'W', effect = 'direct')


