# Grabchak's t-test for unknown pairings (American Statistician, 2023)
unk.paired.t.test <- function(x, y, positive.rho=FALSE){
  n <- length(x)
  if (n!=length(y)) stop("x and y must be equal length vectors.")
  x.bar <- mean(x)
  y.bar <- mean(y)
  s.x <- sd(x)
  s.y <- sd(y)
  xy.prod.lwr <- sum(sort(x) * sort(y, decreasing=TRUE))
  xy.prod.upr <- sum(sort(x) * sort(y, decreasing=FALSE))
  r.est.denom <- (n - 1) * s.x * s.y
  r.lwr <- (xy.prod.lwr - n * x.bar * y.bar) / r.est.denom
  r.upr <- (xy.prod.upr - n * x.bar * y.bar) / r.est.denom
  if (positive.rho){
    r.lwr <- 0
  }
  t.num <- (x.bar - y.bar)
  se.lwr <- sqrt(s.x^2 + s.y^2 - 2 * r.lwr * s.x * s.y) / sqrt(n)
  se.upr <- sqrt(s.x^2 + s.y^2 - 2 * r.upr * s.x * s.y) / sqrt(n)
  t.lwr <- t.num / se.lwr
  t.upr <- t.num / se.upr
  list(statistic=c(abs(t.lwr), abs(t.upr)),
       parameter=c(ifelse(positive.rho, 2 * (n - 1), n - 1), n - 1),
       p.value=2 * c(
         pt(-abs(t.lwr), df=ifelse(positive.rho, 2 * (n - 1), n - 1)),
         pt(-abs(t.upr), df=n - 1)),
       estimate=x.bar - y.bar,
       stderr=c(se.lwr, se.upr))
}
