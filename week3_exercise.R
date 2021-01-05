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

# now apply the polynomial equation to the priors to get predicted heights
m_poly_mu <- link(
  m_foxes,
  post = m_poly_prior,
  data = foxes_std) %>%
  as_tibble() %>%
  pivot_longer(cols = everything(), values_to = "weight_pred") %>%
  add_column(area = rep(foxes_std$area, N), 
             weight = rep(foxes_std$weight, N), 
             type = rep(as.character(1:N), each = length(foxes_std$area)))

# plot it
ggplot(m_poly_mu) +
  geom_line(aes(area, weight_pred, group = type), alpha = 0.5) +
  geom_point(aes(area, weight), shape = 21,
             fill = "firebrick", colour = "grey20", size = 2) +
  labs(title = "Prior predictive simulation", x = "Area (std)", y = "Weight (std)") +
  theme_minimal()

# now take a look at the posterior
post_mean <- link(m_foxes, foxes_std) %>%
  as_tibble() %>%
  summarise(across(everything(), mean)) %>%
  pivot_longer(cols = everything(), values_to = "weight_pred") %>% 
  add_column(join_col = 1:length(foxes_std$area)) %>% 
  select(-name)

post_pi <- link(m_foxes, foxes_std) %>%
  as_tibble() %>% 
  summarise(across(everything(), PI)) %>%
  add_column(type = c("lower", "upper")) %>% 
  pivot_longer(cols = -type, values_to = "intervals") %>% 
  pivot_wider(names_from = type, values_from = intervals) %>% 
  select(-name) %>% 
  add_column(join_col = 1:length(foxes_std$area))

post_mean %>% 
  left_join(post_pi, by = "join_col") %>% 
  add_column(weight = foxes_std$weight, 
             area = foxes_std$area) %>% 
  ggplot() +
  geom_ribbon(aes(x = area, ymin = lower, ymax = upper), fill = "grey60") +
  geom_line(aes(area, weight_pred), colour = "orange") +
  geom_point(aes(area, weight), shape = 21,
             fill = "firebrick", colour = "grey20", size = 2) +
  labs(title = "Posterior predictions", x = "Area (std)", y = "Weight (std)") +
  theme_minimal()
  
## now let's add all parameters to the model (except group), for a multiple regression
m_foxes <- alist(weight ~ dnorm(mu, sigma),
                 mu <- a[group] + Bfood*avgfood + Bsize*groupsize + Barea*area,
                 a[group] ~ dnorm(0, 0.5),
                 Bfood ~ dnorm(0, 0.5),
                 Bsize ~ dnorm(0, 0.5),
                 Barea ~ dnorm(0, 0.5),
                 sigma ~ dexp(1)) %>% 
  quap(., data = foxes_std)

# check priors
prior <- extract.prior(m_foxes, n = N)
mu <- link(m_foxes, post = prior, data = foxes_std) %>%
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

# this is an improved version to get the posterior mean and pi 
post <- link(m_foxes, foxes_std)
post1 <- post %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  nest() %>% 
  mutate(data = map(data, "value"), 
         mean_pred = map_dbl(data, mean), 
         pi_pred = map(data, PI), 
         pi_low = map_dbl(pi_pred, pluck(1)), 
         pi_high = map_dbl(pi_pred, pluck(2)))
  
map_dbl(data = c(value))

post_mean <- link(m_foxes, foxes_std) %>%
  as_tibble() %>%
  summarise(across(everything(), mean)) %>%
  pivot_longer(cols = everything(), values_to = "weight_pred") %>% 
  add_column(join_col = 1:length(foxes_std$area)) %>% 
  select(-name)

post_pi <- link(m_foxes, foxes_std) %>%
  as_tibble() %>% 
  summarise(across(everything(), PI)) %>%
  add_column(type = c("lower", "upper")) %>% 
  pivot_longer(cols = -type, values_to = "intervals") %>% 
  pivot_wider(names_from = type, values_from = intervals) %>% 
  select(-name) %>% 
  add_column(join_col = 1:length(foxes_std$area))

post_mean %>% 
  left_join(post_pi, by = "join_col") %>% 
  add_column(weight = foxes_std$weight, 
             area = foxes_std$area) %>% 
  ggplot() +
  geom_ribbon(aes(x = area, ymin = lower, ymax = upper), fill = "grey60") +
  geom_line(aes(area, weight_pred), colour = "orange") +
  geom_point(aes(area, weight), shape = 21,
             fill = "firebrick", colour = "grey20", size = 2) +
  labs(title = "Posterior predictions", x = "Area (std)", y = "Weight (std)") +
  theme_minimal()
