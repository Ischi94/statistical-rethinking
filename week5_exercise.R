library(tidyverse)
library(rethinking)

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

# Two events: 70 % # As information criterion is based on the deviance, which is a sum. More
# observations generally lead to a higher deviance as more values are summed up,
# rendering a comparison to a model with less observations useless.heads and 30 % tails. 

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

