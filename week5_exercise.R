library(tidyverse)
library(rethinking)

map <- purrr::map

### 7E1 ### 

# State the three motivating criteria that define information entropy. Try to
# express each in yourown words.

# The measure of uncertainty should be continuous. Continuity enables the
# comparison of two measures.
# The measure of uncertainty should increase as the number of possible events
# in-creases. More events mean more uncertainty about which event will occurr.
# The measure of uncertainty should be additive. We need to be able to add up
# all measures.
  
### 7e2 ###

# Suppose a coin is weighted such that, when it is tossed and lands on a table,
# it comes up heads 70 % of the time. What is the entropy of this coin?

# Two events: 70 % heads and 30 % tails. 


# Using the function: 
# H(p)=−\sum_{i=1}^n p_ilog(p_i) = −(p_Hlog(p_H)+p_Tlog(p_T))
inf_entropy <- function(p){
  -sum(p*log(p))
}


c(0.7, 0.3) %>% 
  inf_entropy()

### 7E3 ###

# Suppose a four-sided die is loaded such that, when tossed onto a table, it
# shows “1” 20%, “2” 25%, ”3” 25%, and ”4” 30% of the time. What is the entropy
# of this die?  
c(0.2, 0.25, 0.25, 0.30) %>% 
  inf_entropy()

### 7E4 ###
# As information criterion is based on the deviance, which is a sum. More
# observations generally lead to a higher deviance as more values are summed up,
# rendering a comparison to a model with less observations useless.
# Suppose another four-sided die is loaded such that it never shows “4”. The
# other three sides show equally often. What is the entropy of this die.
rep(1/3, 3) %>% 
  inf_entropy()

### 7M1 ###

# Write down and compare the definitions of AIC and WAIC. Which of these
# criteria is most general? Which assumptions are required to transform the more
# general criterion into a less general one?

# AIC = D_{train} + 2p = 2lppd + 2p
# where D_{train} = in-sample deviance and p = the number of free parameters in the posterior distribution.

# Only reliable when 
# (1) The priors are flat or overwhelmed by the likelihood.  
# (2) The posterior distribution is approximately multivariate Gaussian.  
# (3) The sample size N is much greater than the number of parameters k.

# WAIC = −2(lppd −\sum_i(var_θ log p(y_i|θ))
# where y_i = observation at point i, θ = the posterior distribution, 
# lppd = log-pointwise-predictive density. 

# Only reliable when
# (1) The sample size N is much greater than the number of parameters k.  

# WAIC is similar to the AIC when the priors are flat or overwhelmed by the
# likelihood, and when the posterior distribution is approximately multivariate
# gaussian.

### 7M2 ###

# Explain the difference between model selection and model comparison. What
# information islost under model selection. 

# Model selection is the practice of choosing the model with the lowest
# criterion value and then discarding the others. Model comparison, on the other
# hand, uses uses multiple models to understand both how different variables
# influence predictions and, in combination with a causal model, implied
# conditional indendencies among variables help us infer causal relationships.  

# With model selection, we are throwing away the differences in the information
# criterions between each model which encapsulates how confident we are in a
# model.

### 7M3 ###

# When comparing models with an information criterion, why must all models be
# fit to exactly the same observations? What would happen to the information
# criterion values, if the models were fit to different numbers of observations?
# Perform some experiments, if you are not sure.  
  
# As information criterion is based on the deviance, which is a sum. More
# observations generally lead to a higher deviance as more values are summed up,
# rendering a comparison to a model with less observations useless. 

waic_sim <- function(N){
  dat <- tibble(x = rnorm(N), 
                y = rnorm(N)) %>% 
    mutate(across(everything(), standardize)) 
  
  alist(
    x ~ dnorm(mu, 1), 
    mu <- a + By*y,
    a ~ dnorm(0, 0.2), 
    By ~ dnorm(0, 0.5), 
  ) %>% 
    quap(data = dat) %>% 
    WAIC() %>% 
    as_tibble() %>% 
    pull(WAIC)
}

seq(100, 1000, by = 100) %>%
  purrr::map_dbl(waic_sim) %>% 
  enframe(name = "n_obs", value = "WAIC") %>% 
  mutate(n_obs = seq(100, 1000, by = 100)) %>% 
  ggplot(aes(n_obs, WAIC)) +
  geom_point() +
  geom_line() +
  labs(x = "Number of observations", 
       caption = "Figure 1: Simulation of WAIC values of the same data based on the number of observations") +
  theme_minimal()

### 7M4 ###

# What happens to the effective number of parameters, as measured by PSIS or
# WAIC, as a prior becomes more concentrated? Why? Perform some experiments, if
# you are not sure. 

psis_sim <- function(prior){
  dat <- tibble(x = rnorm(10), 
                y = rnorm(10)) %>% 
    mutate(across(everything(), standardize)) 
  
  suppressMessages(
    alist(
      x ~ dnorm(mu, 1), 
      mu <- a + By*y,
      a ~ dnorm(0, prior), 
      By ~ dnorm(0, prior)
    ) %>% 
      quap(data = list(x = dat$x, y = dat$y, prior = prior)) %>% 
      PSIS() %>% 
      as_tibble() %>% 
      pull(penalty)
  )
}

seq(1, 0.1, length.out = 20) %>%
  purrr::map_dbl(psis_sim) %>% 
  enframe(name = "b_prior", value = "n_params") %>% 
  mutate(b_prior = seq(1, 0.1, length.out = 20)) %>% 
  ggplot(aes(b_prior, n_params)) +
  geom_smooth(method = "lm") +
  geom_point() +
  labs(x = "Prior on the spread for alpha and beta",
       y = "Number of parameters",
       caption = "Figure 2: Simulation of the effective number of parameters as a function of a prior") +
  theme_minimal()

# The number of effective parameters decreases when the prior gets more concentrated.  
  
### 7M5 ###
 
# Overfitting means that a model captures too much noise in the data. This noise
# is not present in new (unseen) data, so the model will fail there. Setting an
# informative prior is like telling the model to ignore too unrealistic values
# or at least too lower their leverage. This then leads to an improved
# predictive performance.

### 7M6 ###

# If the priors are too concentrated, we tell the model to ignore real patterns
# in the data. This then reduces predictive performance.

### 7H1 ###

# In 2007,The Wall Street Journal published an editorial (“We’re Number One,
# Alas”) with a graph of corporate tax rates in 29 countries plotted against
# tax revenue. A badly fit curve was drawn in (reconstructed at right), seemingly
# by hand, to make the argument that the relationship between tax rate and tax
# revenue increases and then declines, such that higher tax rates can actually
# produce less tax revenue. I want you to actually fit a curve to these data,
# found in data(Laffer). Consider models that use tax rate to predict tax revenue.
# Compare, using WAIC or PSIS, a straight-line model to any curved models you
# like. What do you conclude about the relationship between tax rate and tax
# revenue?  

# https://www.erikkusch.com/post/rethinking/7H1.JPG  

data("Laffer")

dat_laffer <- Laffer %>% 
  as_tibble() %>% 
  mutate(across(everything(), standardize))

# simple regression
m1 <- alist(
  tax_revenue ~ dnorm(mu, sigma), 
  mu <- a + Br*tax_rate, 
  a ~ dnorm(0, 0.2),
  Br ~ dnorm(0, 0.5), 
  sigma ~ dexp(1)
) %>% 
  quap(data = dat_laffer)

# quadratic
m2 <- alist(
  tax_revenue ~ dnorm(mu, sigma), 
  mu <- a + Br*tax_rate + Br2*tax_rate^2, 
  a ~ dnorm(0, 0.2),
  c(Br, Br2) ~ dnorm(0, 0.5), 
  sigma ~ dexp(1)
) %>% 
  quap(data = dat_laffer)

# cubic
m3 <- alist(
  tax_revenue ~ dnorm(mu, sigma), 
  mu <- a + Br*tax_rate + Br2*tax_rate^2 + Br3*tax_rate^3, 
  a ~ dnorm(0, 0.2),
  c(Br, Br2, Br3) ~ dnorm(0, 0.5), 
  sigma ~ dexp(1)
) %>% 
  quap(data = dat_laffer)

plot(compare(m1, m2, m3))

N <- 1e3

laffer_fit <- function(model.input, model.type){
    link(model.input, n = N) %>% 
    as_tibble() %>% 
    pivot_longer(cols = everything(), values_to = "pred_revenue") %>% 
    add_column(tax_rate = rep(dat_laffer$tax_rate, N)) %>% 
    group_by(tax_rate) %>% 
    nest() %>% 
    mutate(pred_revenue = map(data, "pred_revenue"), 
           mean_revenue = map_dbl(pred_revenue, mean), 
           pi = map(pred_revenue, PI), 
           lower_pi = map_dbl(pi, pluck(1)), 
           upper_pi = map_dbl(pi, pluck(2))) %>% 
    add_column(model = model.type) %>% 
    select(model, tax_rate, mean_revenue, lower_pi, upper_pi)
}

laffer_fit(m1, "linear") %>% 
  full_join(laffer_fit(m2, "quadratic")) %>% 
  full_join(laffer_fit(m3, "cubic")) %>% 
  ggplot(aes(tax_rate, mean_revenue)) +
  geom_point(aes(tax_rate, tax_revenue), 
                 data = dat_laffer) +
  geom_ribbon(aes(ymin = lower_pi, ymax = upper_pi, 
                  fill = model),
              alpha = 0.3) +
  geom_line(aes(colour = model),
            size = 1.5) +
  labs(x = "Tax revenue (std)", 
       y = "Tax rate (std)") +
  facet_wrap(~ model) +
  theme_minimal() +
  theme(legend.position = "none")

### 7H2 ###

# In the Laffer data, there is one country with a high tax revenue that is an
# outlier. Use PSIS and WAIC to measure the importance of this outlier in the
# models you fit in the previous problem. Then use robust regression with a
# Student’s t distribution to revisit the curve fitting problem. How much does a
# curved relationship depend upon the outlier point?

get_outlier <- function(model.input, model.type){
  psis_k <- PSIS(model.input, pointwise = TRUE)$k
  waic_pen <- WAIC(model.input, pointwise = TRUE)$penalty
  tibble(psis_k = psis_k, waic_pen = waic_pen, model = model.type)
}

dat_outlier <- get_outlier(m1, model.type = "linear") %>% 
  full_join(get_outlier(m2, model.type = "quadratic")) %>% 
  full_join(get_outlier(m3, model.type = "cubic")) 

dat_outlier %>% 
  group_by(model) %>% 
  summarise(max_outlier = which.max(psis_k))

dat_laffer[12,]

ggplot(aes(psis_k, waic_pen, colour = model),
       data = dat_outlier) + 
  geom_point() +
  facet_wrap(~ model) +
  theme_bw() +
  theme(legend.position = "none")

get_outlier(m1, model.type = "linear") %>% 
  which.max(psis.k)

m4 <- alist(
  tax_revenue ~ dstudent(2, mu, sigma), 
  mu <- a + Br*tax_rate, 
  a ~ dnorm(0, 0.2),
  Br ~ dnorm(0, 0.5), 
  sigma ~ dexp(1)
) %>% 
  quap(data = dat_laffer)

plot(compare(m1, m2, m3, m4))

### 7H3 ###

# Consider three fictional Polynesian islands. On each there is a Royal
 # Ornithologist charged by the king with surveying the bird population. They have
# each found the following proportions of 5 important bird species:

dat_island <- tibble("Island" = c("Island 1", "Island 2", "Island 3"),
       "Species A" = c(0.2, 0.8, 0.05), 
       "Species B" = c(0.2, 0.1, 0.15), 
       "Species C" = c(0.2, 0.05, 0.7), 
       "Species D" = c(0.2, 0.025, 0.05), 
       "Species E" = c(0.2, 0.025, 0.05))

dat_island %>% 
knitr::kable()

# Notice that each row sums to 1, all the birds. This problem has two parts. It
# is not computationally complicated. But it is conceptually tricky. First,
# compute the entropy of each island’s bird distribution.Interpret these entropy
# values. Second, use each island’s bird distribution to predict the other
# two.This means to compute the K-L Divergence of each island from the others,
# treating each island as if it were a statistical model of the other islands.
# You should end up with 6 different K-L Divergence values. Which island predicts
# the others best? Why?

dat_island_long <- dat_island %>% 
  janitor::clean_names() %>% 
  pivot_longer(cols = -island, names_to = "species", values_to = "proportion") 

dat_island_long %>% 
  group_by(island) %>% 
  summarise(entropy = inf_entropy(proportion))

# Imagine we throw all birds of an island in a bag, ending with 3 bags full of
# birds. Entropy is just a measure for how uncertain we are which species we
# would get, if we grab a random bird from the bag. In the first bag, Island 1,
# all species are distributed equally and we just have no idea which species we
# would grab. The entropy is high. On the other hand, the second bag from island
# 2 is full of species a so we are quite certain that we would grab this
# species. The entropy is hence quite low.

# the Kullback-Leibler divergence is given as 
# D_{KL}(p,q) = \sum_i(p_i * log(p_i) - log(q_i))

kl_divergence <- function(p,q) sum( p*(log(p)-log(q)) )

dat_island_long %>% 
  select(-species) %>% 
  group_by(island) %>% 
  nest() %>% 
  pivot_wider(names_from = island, values_from = data) %>% 
  janitor::clean_names() %>% 
  mutate(one_vs_two = map2_dbl(island_1, island_2, kl_divergence),
         two_vs_one = map2_dbl(island_2, island_1, kl_divergence),
         one_vs_three = map2_dbl(island_1, island_3, kl_divergence),
         three_vs_one = map2_dbl(island_3, island_1, kl_divergence),
         two_vs_three = map2_dbl(island_2, island_3, kl_divergence),
         three_vs_two = map2_dbl(island_3, island_2, kl_divergence)
         ) %>% 
  select(one_vs_two:three_vs_two) %>% 
  pivot_longer(cols = everything(), names_to = "Comparison", values_to = "KL Divergence") %>% 
  knitr::kable()

# Read the first row as "going from island 1 to island 2 has a divergence of
# 0.97". We can see that starting at island 1 results in the overall lowest
# divergence values, due to the high entropy. If you are used to draw your birds
# from a bag 1 (from island 1), you are not very surprised of the results from
# drawing from the other bags. And now I think the metaphor reaches its limits.  
## gif
# Anyways, Island 1 predicts the other once best.

### 7H4 ###

# Recall the marriage, age, and happiness collider bias example from Chapter 6.
# Run models m6.9 and m6.10 again. Compare these two models using WAIC (or LOO, they
# will produce identical results). Which model is expected to make better
# predictions? Which model provides the correct causal inference about the
# influence of age on happiness? Can you explain why the answers to these two
# questions disagree? 

dat_happy <- sim_happiness(seed=1977, N_years=1000)

dat_happy <- dat_happy %>% 
  as_tibble() %>% 
  filter(age > 17) %>% 
  mutate(age = (age - 18)/ (65 -18)) %>% 
  mutate(mid = married +1)
  
m6.9 <- alist(
  happiness ~ dnorm( mu , sigma ),
  mu <- a[mid] + bA*age,
  a[mid] ~ dnorm( 0 , 1 ),
  bA ~ dnorm( 0 , 2 ),
  sigma ~ dexp(1)) %>% 
  quap(data = dat_happy) 

m6.10 <- alist(
  happiness ~ dnorm( mu , sigma ),
  mu <- a + bA*age,
  a ~ dnorm( 0 , 1 ),
  bA ~ dnorm( 0 , 2 ),
  sigma ~ dexp(1)) %>% 
  quap(data = dat_happy)

c(m6.9, m6.10) %>% 
  map_dfr(WAIC) %>% 
  add_column(model = c("m6.9", "m6.10"), .before = 1) %>% 
  knitr::kable()

# WAIC favours the model with the collider bias. This is because WAIC cares
# about predictive power, not causal association. It just doesn't care that age
# is not causing happiness. However, the (non-causal but still present)
# association between age and happiness results in improved predictions. 
# That's why WAIC selects the model containing the collider path. 
# This just shows that we shouldn't use model comparison without a causal model. 

### 7H5 ###

# Revisit the urban fox data, data(foxes), from the previous chapter’s practice
# problems. Use WAIC or PSIS based model comparison on five different models,
# each using weight as the outcome, and containing these sets of predictor
# variables:

# (1) avgfood + groupsize + area
# (2) avgfood + groupsize
# (3) groupsize + area
# (4) avgfood
# (5) area

# Can you explain the relative differences in WAIC scores, using the fox DAG
# from last week’s home-work? Be sure to pay attention to the standard error of
# the score differences (dSE).

data("foxes")

dat_foxes <- foxes %>% 
  as_tibble() %>% 
  mutate(across(-group, standardize))

m1 <- alist(
  weight ~ dnorm(mu, sigma), 
  mu <- a + Bf*avgfood + Bg*groupsize + Ba*area, 
  a ~ dnorm(0, 0.2), 
  c(Bf, Bg, Ba) ~ dnorm(0, 0.5), 
  sigma ~ dexp(1)) %>% 
  quap(data = dat_foxes)

m2 <- alist(
  weight ~ dnorm(mu, sigma), 
  mu <- a + Bf*avgfood + Bg*groupsize, 
  a ~ dnorm(0, 0.2), 
  c(Bf, Bg) ~ dnorm(0, 0.5), 
  sigma ~ dexp(1)) %>% 
  quap(data = dat_foxes)

m3 <- alist(
  weight ~ dnorm(mu, sigma), 
  mu <- a + Bg*groupsize + Ba*area, 
  a ~ dnorm(0, 0.2), 
  c(Bg, Ba) ~ dnorm(0, 0.5), 
  sigma ~ dexp(1)) %>% 
  quap(data = dat_foxes)

m4 <- alist(
  weight ~ dnorm(mu, sigma), 
  mu <- Bf*avgfood, 
  a ~ dnorm(0, 0.2), 
  Bf ~ dnorm(0, 0.5), 
  sigma ~ dexp(1)) %>% 
  quap(data = dat_foxes)

m5 <- alist(
  weight ~ dnorm(mu, sigma), 
  mu <- a + Ba*area, 
  a ~ dnorm(0, 0.2), 
  Ba ~ dnorm(0, 0.5), 
  sigma ~ dexp(1)) %>% 
  quap(data = dat_foxes)

compare(m1, m2, m3, m4, m5) %>% 
  as_tibble(rownames = "model") %>% 
  knitr::kable()

# here's the link: https://gregor-mathes.netlify.app/post/chapter6_files/figure-html/data%20foxes-1.png

# There is no support for the preference of a particular model. But we can see
# that m1, m2, and m3 are grouped together based on AIC as well as m4 and m5.
# Following the DAG, as long as we include the groupsize path, it does not make
# a difference if we use area or avgfood. They encapsulate the same information,
# and WAIC hence returns a similar value. m4 and m5 both don't include
# groupsize. But as avgfood and area contain mostly the same information, they
# both show similar WAIC estimates.
  


