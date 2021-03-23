The Acceptance Rejection Method
```{r}
f <- function(x) 20*x*(1-x)^3 # This is the pdf
c <- 135/64
r_fun <- function(n) {
  flag <- 0
  attempt <- 0
  x <- numeric(n)
  while (flag < n) {
  u1 <- runif(1) # Value from g(x)
  u2 <- runif(1) # Value to comparing
  if (u2 < f(u1)/c ) {
  flag <- flag + 1
  x[flag] <- u1
  }
  attempt <- attempt + 1
  }
  if (n==1) list(x=x, attempt=attempt) # attempt is useful when n=1
  else x
}
```
