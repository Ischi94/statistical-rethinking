library(tidyverse)
library(rethinking)

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
