library(tidyverse)
library(rethinking)


# Chapter 4 ---------------------------------------------------------------


# data
data("Howell1")
Howell1 <- Howell1 %>% as_tibble()

# 4M1
# For the model definition below, simulate observed y values from the prior 
# (not the posterior).
# yi∼Normal(μ,σ) / μ∼Normal(0,10) / σ∼exponential(1)
sample_mu <- rnorm(1e4, 0, 10)

sample_sigma <- rexp(1e4, 1)

prior_y <- rnorm(1e4, sample_mu, sample_sigma) %>% enframe()

ggplot(prior_y) +
  geom_density(aes(value), size = 1) + theme_light()

# 4M2
# Translate the model just above into a map() formula
map_frm <- alist(y ~ dnorm(mu, sigma), 
                 mu ~ dnorm(0, 10), 
                 sigma ~ dexp(1)) 

# 4M3
# Translate the map() model formula below into a mathematical model definition.
flist <- alist(
  y ~ dnorm(mu, sigma),
  mu <- a + b*x,
  a ~ dnorm(0, 50),
  b ~ dunif(0, 10),
  sigma ~ dunif(0, 50)
)

# y1 ~ Normal(μ,σ) / μ ~ a + Bx1 / a ~ Normal(0, 50) / 
# B = Uniform(0, 10) / σ ~ Uniform(0, 50) 

# 4M4
# A sample of students is measured for height each year for 3 years.
# After the third year, you want to fit a linear regression predicting height using 
# year as a predictor. Write down the mathematical model definition for this regression, 
# using any variable names and priors you choose. 
# Be prepared to defend your choice of priors.

# height ~ Normal(μ,σ) / μ1 ~ a + Bx1 / a ~ Normal(150, 25) / 
# B = Uniform(4, 2) / σ ~ Exponential(1)

# 4M5
# Now suppose I tell you that the average height in the first year was 120 cm and 
# that every student got taller each year. Does this information lead you to change 
# your choice of priors? How?
# height ~ Normal(μ,σ) / μ1 ~ a + Bx1 / a ~ Normal(120, 10) / 
# B = Uniform(7, 1) / σ ~ Exponential(1)