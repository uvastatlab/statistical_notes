# generate population with collinearity
n <- 10000
x1 <- runif(n, -3, 3)
x2 <- x1 * 2 + rnorm(n, sd = 0.3)
plot(x1, x2) # collinearity!
x3 <- rnorm(n, 10, 2)
x4 <- x3 * -4 + rnorm(n, sd = 0.5)
plot(x3, x4) # collinearity!
x5 <- rexp(n = n)
x6 <- runif(n = n)

# NOTE: y only generated with x1, x3, x5 and x6! No interactions!  
y <- 2 + x1 + x3 + x5 + x6 + rnorm(n, sd = 1.3) 
d <- data.frame(y, x1, x2, x3, x4, x5, x6)


# pretend we don't know model;
# sample data (n = 300) and use all predictors
# highlight and repeatedly run the next 4 lines;
# Notice the same model is hardly ever selected twice
d1 <- d[sample(nrow(d), 300),]
m <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6, d1)
s <- step(m, scope = . ~ .^2, trace = 0)
names(s$coefficients) # variables selected

# function to do this 
stepwise <- function(){
  d1 <- d[sample(nrow(d), 300),]
  m <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6, d1)
  s <- step(m, scope = . ~ .^2, trace = 0)
  names(s$coefficients)
}

# repeat function 500 times
models <- replicate(n = 500, expr = stepwise())
# collapse selected variable names into single string
coefs <- sapply(models, function(x)paste(x, collapse = " "))

# number of different models selected!
length(unique(coefs))

# proportion of time correct model is selected
mean(coefs == '(Intercept) x1 x3 x5 x6')
