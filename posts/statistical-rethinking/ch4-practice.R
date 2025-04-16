library(data.table)
library(rethinking)
library(splines)
set.seed(1818)
# 4M1. Prior predictive check
sigma <- rexp(10000, rate=1)
mu <- rnorm(10000, mean=0, sd=10)
y.sim <- rnorm(10000, mean=mu, sd=sigma)
dens(y.sim)
# 4M2. Translate to quadratic approximation
quap(
  alist(
    y ~ dnorm(mu, sigma),
    mu ~ dnorm(0, 10),
    sigma ~ dexp(1)),
  data=list(y=rnorm(99)))
# 4H1. Model-based predictions of height based on weight
data("Howell1"); setDT(Howell1)
adults <- Howell1[age>=18, ]
adult.mean.weight <- mean(adults$weight)
fit.heights <- quap(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- alpha + beta * (weight - adult.mean.weight),
    alpha ~ dnorm(178, 20),
    beta ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ),
  data=list(weight=adults$weight, y=adults$height))
post <- extract.samples(fit.heights); precis(post)
pred <- link(fit.heights, data=data.frame(weight=c(46.95, 43.72, 64.78, 32.59, 54.63)))
apply(pred, 2, mean)
apply(pred, 2, PI, prob=0.89)
# 4H2. Model of height/weight with adolescents
kids <- Howell1[age < 18,]
kid.mean.weight <- mean(kids$weight)
fit.heights <- quap(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- alpha + beta * (weight - kid.mean.weight),
    alpha ~ dnorm(178, 20),
    beta ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ),
  data=list(weight=kids$weight, y=kids$height),
  start=list(alpha=mean(kids$height), beta=1, sigma=sd(kids$height)))
post <- extract.samples(fit.heights); precis(post)
plot(height ~ weight, data=kids, col=rangi2)
mu <- link(fit.heights, data=data.frame(weight=5:45))
mu.int89 <- apply(mu, 2, PI, prob=0.89)
lines(5:45, apply(mu, 2, mean))
shade(mu.int89, 5:45)
# 4H3. Model of height/weight with all data
mean.weight <- mean(Howell1$weight)
mean.height <- mean(Howell1$height)
fit.heights <- quap(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- alpha + beta * log(weight),
    alpha ~ dnorm(178, 20),
    beta ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ),
  data=list(weight=Howell1$weight, y=Howell1$height),
  start=list(alpha=mean.height, beta=1, sigma=sd(Howell1$height)))
post <- extract.samples(fit.heights); precis(post)
plot(height ~ weight, data=Howell1,col=col.alpha(rangi2, 0.4))
plot(height ~ weight, data=kids, col=rangi2)
mu <- link(fit.heights, data=data.frame(weight=0:65))
mu.int97 <- apply(mu, 2, PI, prob=0.97)
lines(0:65, apply(mu, 2, mean))
shade(mu.int97, 0:65)
sim.heights <- sim(fit.heights, data=list(weight=0:65))
heights.int97 <- apply(sim.heights, 2, PI, prob=0.97)
shade(heights.int97, 0:65)
# 4H4. Prior predictive check on splines
sample.beta1 <- rlnorm(10000, 0, 1)
sample.beta2 <- rnorm(10000, 0, 1)
sample.alpha <- rnorm(10000, 178, 20)
sample.mu <- c(
  sample.alpha + sample.beta1 * (-1) + sample.beta2 * (-1)^2,
  sample.alpha + sample.beta1 * 0 + sample.beta2 * 0,
  sample.alpha + sample.beta1 * 1 + sample.beta2 * 1,
  sample.alpha + sample.beta1 * 1.5 + sample.beta2 * (1.5)^2)
sample.sigma <- runif(10000, 0, 50)
prior.h <- rnorm(10000, sample.mu, sample.sigma)
dens(prior.h)
