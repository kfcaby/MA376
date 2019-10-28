sleep = c(3,5,5.5,4,7.5,
          5,6,4.5,4,5.5,4.5,
          5.5,3,3,5,5,5)

mu = 7
xbar = mean(sleep)
s = sd(sleep)
n = length(sleep)

t = (xbar - mu)/(s/sqrt(n))
t

#calculate p-value
pt(t, df = n-1)

