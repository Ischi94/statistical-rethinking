library(rethinking)
library(tidyverse)
library(dagitty)

# week 3 homework: 
# https://github.com/rmcelreath/stat_rethinking_2020/blob/main/homework/week03/week03.pdf

# All three problems below are based on the same data. The data in data(foxes)
# are 116 foxes from 30 different urban groups in England. These foxes are like
# street gangs. Group size varies from 2 to 8 individuals. Each group maintains its own (almost exclusive) 
# urban territory. Some territories are larger
# than others. The area variable encodes this information. Some territories
# also have more avgfood than others. We want to model the weight of each
# fox. For the problems below, assume this DAG:

dag_foxes <- dagitty( "dag {
                      area -> avgfood -> groupsize -> weight
                      avgfood -> weight
                      }") 
coordinates(dag_foxes) <- list(x = c(area = 1, avgfood = 0, groupsize = 2, weight = 1),
                               y = c(area = 0, avgfood = 1, groupsize = 1, weight = 2))
drawdag(dag_foxes)



# homework 1 --------------------------------------------------------------


# Use a model to infer the total causal influence of area on weight . Would
# increasing the area available to each fox make it heavier (healthier)? You
# might want to standardize the variables. Regardless, use prior predictive
# simulation to show that your model’s prior predictions stay within the possible outcome range.

# load data
data(foxes)

# standardise data
foxes_std <- foxes %>% 
  as_tibble() %>% 
  mutate(across(-group, standardize))

# let's start with a simple linear regression of area on weight
m_foxes <- alist(weight ~ dnorm(mu, sigma),
                  mu <- a + B * area,
                  a ~ dnorm(0, 1),
                  B ~ dnorm(0, 1),
                  sigma ~ dexp(1)) %>%
  quap(., data = foxes_std)
  
# let's do some prior simulation 

# define number of regression lines
N <- 100

# extract samples from the prior
m_poly_prior <- extract.prior(m_foxes, n = N)

# now apply the linear equation to the priors to get predicted heights
post_mu <- link(
  m_foxes,
  post = m_poly_prior,
  data = foxes_std) %>%
  as_tibble() %>%
  pivot_longer(cols = everything(), values_to = "weight_pred") %>%
  add_column(area = rep(foxes_std$area, N), 
             weight = rep(foxes_std$weight, N), 
             type = rep(as.character(1:N), each = length(foxes_std$area)))

# plot it
ggplot(post_mu) +
  geom_line(aes(area, weight_pred, group = type), alpha = 0.5) +
  geom_point(aes(area, weight), shape = 21,
             fill = "firebrick", colour = "grey20", size = 2) +
  labs(title = "Prior predictive simulation", x = "Area (std)", y = "Weight (std)") +
  theme_minimal()

# now take a look at the posterior
post_single <- link(m_foxes, foxes_std) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  nest() %>% 
  mutate(data = map(data, "value"), 
         mean_pred = map_dbl(data, mean), 
         pi_pred = map(data, PI), 
         pi_low = map_dbl(pi_pred, pluck(1)), 
         pi_high = map_dbl(pi_pred, pluck(2))) %>% 
  select(mean_pred, pi_low, pi_high) %>% 
  add_column(area = foxes_std$area, weight = foxes_std$weight)

ggplot(post_single) +
  geom_ribbon(aes(x = area, ymin = pi_low, ymax = pi_high), fill = "grey60") +
  geom_line(aes(area, mean_pred), colour = "orange") +
  geom_point(aes(area, weight), shape = 21,
             fill = "firebrick", colour = "grey20", size = 2) +
  labs(title = "Posterior predictions", x = "Area (std)", y = "Weight (std)") +
  theme_minimal()
  
## now let's add all parameters to the model (except group), for a multiple regression
m_foxes_all <- alist(weight ~ dnorm(mu, sigma),
                 mu <- a[group] + Bfood*avgfood + Bsize*groupsize + Barea*area,
                 a[group] ~ dnorm(0, 0.25),
                 Bfood ~ dnorm(0, 0.5),
                 Bsize ~ dnorm(0, 0.5),
                 Barea ~ dnorm(0, 0.5),
                 sigma ~ dexp(1)) %>% 
  quap(., data = foxes_std)

# check priors
prior <- extract.prior(m_foxes_all, n = N)
mu <- link(m_foxes_all, post = prior, data = foxes_std) %>%
  as_tibble() %>%
  pivot_longer(cols = everything(), values_to = "weight_pred") %>%
  add_column(area = rep(foxes_std$area, N), 
             avgfood = rep(foxes_std$avgfood, N), 
             groupsize = rep(foxes_std$groupsize, N), 
             group = rep(foxes_std$group, N),
             weight = rep(foxes_std$weight, N), 
             type = rep(as.character(1:N), each = length(foxes_std$area)))

# plot it
ggplot(mu) +
  geom_line(aes(area, weight_pred, group = type), alpha = 0.5) +
  geom_point(aes(area, weight), shape = 21,
             fill = "firebrick", colour = "grey20", size = 2) +
  labs(title = "Prior predictive simulation", x = "Area (std)", y = "Weight (std)") +
  theme_minimal()

# get the total causal influence of area on weight
mod_comp <- precis(m_foxes) %>% 
  as_tibble(rownames = "estimate") %>% 
  add_row(
    precis(m_foxes_all) %>% 
      as_tibble(rownames = "estimate") %>% 
      filter(estimate == "Barea")
    ) %>% 
  filter(estimate %in% c("B", "Barea")) %>% 
  add_column(m_type = c("Single linear model", "Multiple linear model"), .before = "estimate") %>% 
  select(m_type, mean, sd, lower_pi = "5.5%", upper_pi = "94.5%")

ggplot(mod_comp) +
  geom_vline(xintercept = 0, colour = "salmon", size = 0.8) +
  geom_pointrange(aes(x = mean, xmin = lower_pi, xmax = upper_pi, y = m_type), 
                  colour = "grey20", size = 0.6) +
  labs(title = "Total causal influence of area on weight",
    y = NULL, x = "Estimate") +
  theme_minimal()

