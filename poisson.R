library(R2jags)
#3
data <- data.frame("y" = c(69  ,  28  , 139  , 132  , 194 ,  44  , 118,  68 , 219  ,  58 , 185 , 103 )
                  ,"x0" =c( 1 ,1 , 1  ,1  , 0, 0, 0, 0,0 , 0 , 0 , 0)
                , "x1" =c( 0  ,  0  ,  0  ,  0  , 1, 1, 1, 1,0 , 0 , 0 , 0),
                "x2" =c( 0  ,  0  ,  0  ,  0  , 0, 0, 0, 0,1 , 1 , 1 , 1))
model <- glm(y ~ x0 +x1+x2, data, family = poisson(link = "log"))
# beta2 NA dal momento che e una combinazione lineare tra beta0 e beta1 comne mostra il grafico del punto 2
summary(model)


cat("model{
    for ( i in 1:12) {
    
    y[i] ~ dpois(mu[i])
    log(mu[i]) <- intercept + beta0*x0[i] + beta1*x1[i] + beta2*x2[i]

    }

    intercept ~ dnorm(4,0.01)
    beta0 ~  dnorm(0,1)
    beta1 ~ dnorm(0,1)
    beta2 ~  dnorm(0,1)

    }", file="non-bayesian-poisson.txt")

inits1 <- list( intercept =4 , beta0 = 0, beta1 =0 , beta2=0)
jags.m1 <- jags.model( file = "non-bayesian-poisson.txt", data=data, inits=inits1, n.chains=1, n.adapt=500 )
params1 <- c("intercept","beta0", "beta1","beta2")
samps1 <- coda.samples( jags.m1, params1, n.iter=100000)
summary(samps1)
plot(samps1)
#l'intercept is very similar but also the slopse, that are close to 0
# choose normal distribution are computed by approximation by computer
# and by approximated solution of the optimization problem
#4
cat("model{
    for ( i in 1:12) {
    
    y[i] ~ dpois(mu[i])
   log(mu[i]) <- intercept + beta0*x0[i] + beta1*x1[i] + beta2*x2[i] + e

    }
    #priors
   
    intercept ~ dnorm(4,0.01)
    beta0 ~  dnorm(0,1)
    beta1 ~ dnorm(0,1)
    beta2 ~  dnorm(0,1)
    e ~ dnorm(0,1/lambda)
    #hyperpriors
    lambda ~ dgamma(3,2)  

    }", file="bayesian-poisson.txt")

inits2 <- list( intercept = 5, beta0 = 0, beta1 =1,beta2=0 ,lambda=1 )
jags.m2 <- jags.model( file = "bayesian-poisson.txt", data=data, inits=inits2, n.chains=1, n.adapt=500 )
params2 <- c("intercept", "beta0", "beta1", "beta2")
samps2 <- coda.samples( jags.m2, params2, n.iter=100000, thin=500 )
summary(samps2)
plot(samps2)
#l'intercept is very similar but also the slopse, that are close to 0
# choose normal distribution are computed by approximation by computer
# and by approximated solution of the optimization problem
# use gamma distribution to model the variance
# The variance i choose should be small since data are few and
# a too big variance can be too vague

plot(density(rgamma(100000, shape = 4, rate =2)))


