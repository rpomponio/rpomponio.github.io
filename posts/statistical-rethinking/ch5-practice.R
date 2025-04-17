library(data.table)
library(rethinking)
data(foxes)
set.seed(1818)
# 5H1. Fit two bivariate Gaussian regressions, using quap...
fit.weight.by.territory <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- beta_a * area,
    beta_a ~ dnorm(0, 2),
    sigma ~ dexp(1)),
  data=list(
    weight=scale(foxes$weight),
    area=scale(foxes$area)))
post <- extract.samples(fit.weight.by.territory)
x.seq <- seq(-2, 2, length.out=10)
pred <- link(fit.weight.by.territory, data=(list(area=x.seq)), post=post)
plot(x.seq, colMeans(pred), ylim=c(-2, 2), type="l", xlab="Area", ylab="Weight")
shade(apply(pred, 2, PI, prob=0.95), x.seq)
fit.weight.by.groupsize <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- beta_s * size,
    beta_s ~ dnorm(0, 2),
    sigma ~ dexp(1)),
  data=list(
    weight=scale(foxes$weight),
    size=scale(foxes$groupsize)))
post <- extract.samples(fit.weight.by.groupsize)
pred <- post$beta_s %*% t(x.seq)
plot(x.seq, colMeans(pred), ylim=c(-2, 2), type="l", xlab="Group Size", ylab="Weight")
shade(apply(pred, 2, PI, prob=0.95), x.seq)
# 5H2. Now fit a multiple linear regression...
fit.weights <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- beta_a * area + beta_s * size,
    beta_a ~ dnorm(0, 2),
    beta_s ~ dnorm(0, 2),
    sigma ~ dexp(1)),
  data=list(
    weight=scale(foxes$weight),
    area=scale(foxes$area),
    size=scale(foxes$groupsize)))
plot(coeftab(fit.weight.by.territory, fit.weight.by.groupsize, fit.weights))
pred <- link(fit.weights, data=(list(area=x.seq, size=0)))
plot(x.seq, colMeans(pred), ylim=c(-2, 2), type="l", xlab="Area", ylab="Weight")
shade(apply(pred, 2, PI, prob=0.95), x.seq)
mtext("Holding Group Size CONSTANT")
pred <- link(fit.weights, data=(list(size=x.seq, area=0)))
plot(x.seq, colMeans(pred), ylim=c(-2, 2), type="l", xlab="Group Size", ylab="Weight")
shade(apply(pred, 2, PI, prob=0.95), x.seq)
mtext("Holding Area CONSTANT")
# 5H3. Finally, consider the avgfood variable...
fit.weights2 <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- beta_f * food + beta_s * size,
    beta_f ~ dnorm(0, 2),
    beta_s ~ dnorm(0, 2),
    sigma ~ dexp(1)),
  data=list(
    weight=scale(foxes$weight),
    food=scale(foxes$avgfood),
    size=scale(foxes$groupsize)))
fit.weights3 <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- beta_a * area + beta_f * food + beta_s * size,
    beta_a ~ dnorm(0, 2),
    beta_f ~ dnorm(0, 2),
    beta_s ~ dnorm(0, 2),
    sigma ~ dexp(1)),
  data=list(
    weight=scale(foxes$weight),
    area=scale(foxes$area),
    food=scale(foxes$avgfood),
    size=scale(foxes$groupsize)))
plot(coeftab(fit.weight.by.territory, fit.weight.by.groupsize,
             fit.weights, fit.weights2, fit.weights3))
