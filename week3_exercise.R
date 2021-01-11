library(rethinking)
library(tidyverse)
library(dagitty)


# homework ----------------------------------------------------------------


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

impliedConditionalIndependencies(dag_foxes)

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
                  a ~ dnorm(0, 0.2),
                  B ~ dnorm(0, 0.5),
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
                 mu <- a + Bfood*avgfood + Bsize*groupsize + Barea*area,
                 a ~ dnorm(0, 0.25),
                 Bfood ~ dnorm(0, 0.5),
                 Bsize ~ dnorm(0, 0.5),
                 Barea ~ dnorm(0, 0.5),
                 sigma ~ dexp(1)) %>% 
  quap(., data = foxes_std)

# check priors
# define range for area
a_seq <- seq(from = -2, to = 2, by = 0.5)

# sample from prior
mu <- extract.prior(m_foxes_all, n = N) %>% 
  link(m_foxes_all, post = ., 
           data = list(area = a_seq, 
                       avgfood = 0, groupsize = 0)) %>%
  as_tibble() %>%
  magrittr::set_colnames(a_seq) %>% 
  pivot_longer(cols = everything(), 
               values_to = "weight_pred", names_to = "area") %>%
  mutate(area = as.numeric(area)) %>% 
  add_column(type = rep(as.character(1:N), each = length(a_seq))) 

# plot it
ggplot(mu) +
  geom_line(aes(area, weight_pred, group = type), alpha = 0.5) +
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


# homework 2 --------------------------------------------------------------


# Now infer the causal impact of adding food ( avgfood ) to a territory.
# Would this make foxes heavier? Which covariates do you need to adjust
# for to estimate the total causal influence of food? 

# following the dag, we need to adjust for area, as it acts on avgfood
while(!exists("m_count")){
  tryCatch({
  m_count <- alist(
    
    ## area -> food -> weight <- size
    weight ~ dnorm(mu, sigma),
    mu <- a + Barea*area + Bfood*avgfood + Bsize*groupsize,
    a ~ dnorm(0, 0.2),
    Barea ~ dnorm(0, 0.5),
    Bfood ~ dnorm(0, 0.5),
    Bsize ~ dnorm(0, 0.5),
    sigma ~ dexp(1), 
    
    ## food -> size
    groupsize ~ dnorm(mu_S, sigma_S),
    mu_S <- aS + bFS*avgfood,
    aS ~ dnorm(0, 0.2),
    bFS ~ dnorm(0, 0.5 ),
    sigma_S ~ dexp(1), 
    
    ## area -> food
    avgfood ~ dnorm(mu_F, sigma_F),
    mu_F <- aF + bAF*area,
    aF ~ dnorm(0, 0.2),
    bAF ~ dnorm(0, 0.5),
    sigma_F ~ dnorm(0, 0.5)) %>% 
    quap(., data = foxes_std)
  }, error = function(e){
  }, finally = {})
}



s <- seq(from = -2, to = 2, length.out = 30) 

data_count <- s %>%
  tibble(avgfood = ., area = 0, groupsize = 0) %>%
  sim(m_count, data = ., vars = c("weight")) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), values_to = "weight_count") %>%
  group_by(name) %>% 
  nest() %>% 
  mutate(weight_lst = map(data, "weight_count"), 
         weight_count = map_dbl(weight_lst, mean), 
         weight_pi = map(weight_lst, PI), 
         lower_pi = map_dbl(weight_pi, pluck(1)), 
         upper_pi = map_dbl(weight_pi, pluck(2))) %>% 
  ungroup() %>% 
  select(weight_count, lower_pi, upper_pi) %>% 
  add_column(food_man = s)

ggplot(data_count) +
  geom_ribbon(aes(x = food_man, ymin = lower_pi, ymax = upper_pi), 
              fill = "grey60") +
  geom_line(aes(food_man, weight_count), 
            colour = "orange", size = 1.3) +
  labs(title = "Total counterfactual effect of avgfood on weight", 
       y = "Counterfactual weight", x = "Manipulated average food") +
  theme_minimal()


# Now infer the causal impact of group size. Which covariates do you need
# to adjust for? Looking at the posterior distribution of the resulting model,
# what do you think explains these data? That is, can you explain the estimates
# for all three problems? How do they make sense together? 

# we can use the same model as above, where we included all causal links inferred from the dag
data_count2 <- s %>%
  tibble(groupsize = ., area = 0, avgfood = 0) %>%
  sim(m_count, data = ., vars = c("weight")) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), values_to = "weight_count") %>%
  group_by(name) %>% 
  nest() %>% 
  mutate(weight_lst = map(data, "weight_count"), 
         weight_count = map_dbl(weight_lst, mean), 
         weight_pi = map(weight_lst, PI), 
         lower_pi = map_dbl(weight_pi, pluck(1)), 
         upper_pi = map_dbl(weight_pi, pluck(2))) %>% 
  ungroup() %>% 
  select(weight_count, lower_pi, upper_pi) %>% 
  add_column(group_man = s)

ggplot(data_count2) +
  geom_ribbon(aes(x = group_man, ymin = lower_pi, ymax = upper_pi), 
              fill = "grey60") +
  geom_line(aes(group_man, weight_count), 
            colour = "orange", size = 1.3) +
  labs(title = "Total counterfactual effect of groupsize on weight", 
       y = "Counterfactual weight", x = "Manipulated group size") +
  theme_minimal()

# it seems like weight increases with food (obviously)
# weight increases with area
# weight decreases with groupsize


# for fun -----------------------------------------------------------------

# let's see the effect of area on average food, just for fun and for me to understand
data_count3 <- s %>%
  tibble(area = ., groupsize = 0, weight = 0) %>%
  sim(m_count, data = ., vars = c("avgfood")) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), values_to = "food_count") %>%
  group_by(name) %>% 
  nest() %>% 
  mutate(weight_lst = map(data, "food_count"), 
         food_count = map_dbl(weight_lst, mean), 
         weight_pi = map(weight_lst, PI), 
         lower_pi = map_dbl(weight_pi, pluck(1)), 
         upper_pi = map_dbl(weight_pi, pluck(2))) %>% 
  ungroup() %>% 
  select(food_count, lower_pi, upper_pi) %>% 
  add_column(area_man = s)

ggplot(data_count3) +
  geom_ribbon(aes(x = area_man, ymin = lower_pi, ymax = upper_pi), 
              fill = "grey60") +
  geom_line(aes(area_man, food_count), 
            colour = "orange", size = 1.3) +
  labs(title = "Total counterfactual effect of area on average food", 
       y = "Counterfactual food", x = "Manipulated area") +
  theme_minimal()

# same for average food on group size
data_count4 <- s %>%
  tibble(avgfood = ., area = 0, weight = 0) %>%
  sim(m_count, data = ., vars = c("groupsize")) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), values_to = "group_count") %>%
  group_by(name) %>% 
  nest() %>% 
  mutate(weight_lst = map(data, "group_count"), 
         group_count = map_dbl(weight_lst, mean), 
         weight_pi = map(weight_lst, PI), 
         lower_pi = map_dbl(weight_pi, pluck(1)), 
         upper_pi = map_dbl(weight_pi, pluck(2))) %>% 
  ungroup() %>% 
  select(group_count, lower_pi, upper_pi) %>% 
  add_column(avgfood_man = s)

ggplot(data_count4) +
  geom_ribbon(aes(x = avgfood_man, ymin = lower_pi, ymax = upper_pi), 
              fill = "grey60") +
  geom_line(aes(avgfood_man, group_count), 
            colour = "orange", size = 1.3) +
  labs(title = "Total counterfactual effect of average food on groupsize", 
       y = "Counterfactual groupsize", x = "Manipulated average food") +
  theme_minimal()



# easy questions ----------------------------------------------------------

### 5E1 ###

# Which of the linear models below are multiple linear regressions?

# (1) $$\mu_i = \alpha + \beta_xi$$
# (2) $$\mu_i = \beta_x x_i + \beta_z z_i$$
# (3) $$\mu_i = \alpha + \beta(x_i – z_i)$$
# (4) $$\mu_i = \alpha + \beta_x x_i + \beta_z z_i$$

## (1) contains only one predictor variable ($$\beta_xi$$) and is therefore a bivariate linear regression. 
## (2) has two predictor variables and is a multiple linear regression without an intercept ($$\alpha$$).
## (3) the right side can written as $$\alpha + \beta x_i - \beta z_i$$ which looks like a weird multiple 
# regression with negatively correlated slopes for each predictor.
## (4) is a perfectly looking multiple linear regression. 


### 5E2 ###

# Write down a multiple regression to evaluate the claim: Animal diversity is
# linearly related to latitude, but only after controlling for plant diversity.
# You just need to write down the model definition.

##  Let $$\mu_i$$ be the mean animal diversity, **L** latitude, and **P** plant diversity.
## Then $$\mu_i = \alpha + \beta_L L_i + \beta_P P_i$$


### 5E3 ###  

# Write down a multiple regression to evaluate the claim: Neither the amount of
# funding nor size of laboratory is by itself a good predictor of time to PhD
# degree; but together these variables are both positively associated with time
# to degree. Write down the model definition and indicate which side of zero
# each slope parameter should be on.

## Let $$\mu_i$$ be the time to PhD, **F** the amount of funding, and **S** the size of laboratory.
## Then $$\mu_i = \alpha + \beta_F F_i + \beta_S S_i$$ 
## Where both $$beta_F$$ & $$beta_S > 0$$ 


### 5E4 ### 

# Suppose you have a single categorical predictor with 4 levels (unique values),
# labeled A, B, C, and D. Let Ai be an indicator variable that is 1 where case i
# is in category A. Also suppose Bi, Ci, and Di for the other categories. Now
# which of the following linear models are inferentially equivalent ways to
# include the categorical variable in a regression? Models are inferentially
# equivalent when it’s possible to compute one posterior distribution from the
# posterior distribution of another model.

# (1) $$\mu_i = \alpha + \beta_A A_i + \beta_B B_i + \beta_D D_i$$
# (2) $$\mu_i = \alpha + \beta_A A_i + \beta_B B_i + \beta_C C_i + \beta_D D_i$$
# (3) $$\mu_i = \alpha + \beta_B B_i + \beta_C C_i + \beta_D D_i$$
# (4) $$\mu_i = \alpha_A A_i + \alpha_B B_i + \alpha_C C_i + \alpha_D D_i$$
# (5) $$\mu_i = \alpha_A (1 – B_i – C_i – D_i) + \alpha_B B_i + \alpha_C C_i + \alpha_D D_i$$

# This question was a bit to complicated for me and I just copied over the
# answer from [Jeffrey Girard]():

# The first model includes a single intercept (for category C) and slopes for A,
# B, and D. The second model is non-identifiable because it includes a slope for
# all possible categories (page 156). The third model includes a single
# intercept (for category A) and slopes for B, C, and D. The fourth model uses
# the unique index approach to provide a separate intercept for each category
# (and no slopes). The fifth model uses the reparameterized approach on pages
# 154 and 155 to multiply the intercept for category A times 1 when in category
# A and times 0 otherwise. Models 1, 3, 4, and 5 are inferentially equivalent
# because they each allow the computation of each other’s posterior distribution
# (e.g., each category’s intercept and difference from each other category).


# medium questions --------------------------------------------------------


### 5M1 ###

# Invent your own example of a spurious correlation. An outcome variable should
# be correlated with both predictor variables. But when both predictors are
# entered in the same model, the correlation between the outcome and one of the
# predictors should mostly vanish (or at least be greatly reduced).

N <- 100
dfr <- tibble(pred_1 = rnorm(N), 
       pred_2 = rnorm(N, -pred_1), 
       out_var = rnorm(N, pred_1)) %>% 
  mutate(across(everything(), scale))

# outcome and predictor 1 are positively correlated in a bivariate regression
m1 <- alist(out_var ~ dnorm(mu, sigma),
      mu <- a + B1*pred_1,
      a ~ dnorm(0, 0.2), 
      B1 ~ dnorm(0, 0.5),
      sigma ~ dexp(1)) %>% 
  quap(., data = dfr) %>% 
  precis() %>% 
  as_tibble(rownames = "estimate")



# outcome and predictor 2 are negatively correlated in a bivariate regression
m2 <- alist(out_var ~ dnorm(mu, sigma),
            mu <- a + B2*pred_2,
            a ~ dnorm(0, 0.2), 
            B2 ~ dnorm(0, 0.5),
            sigma ~ dexp(1)) %>% 
  quap(., data = dfr) %>% 
  precis() %>% 
  as_tibble(rownames = "estimate")



# now the multiple linear regression 
m3 <- alist(out_var ~ dnorm(mu, sigma),
            mu <- a + B1*pred_1 + B2*pred_2,
            a ~ dnorm(0, 0.2),
            B1 ~ dnorm(0, 0.5),
            B2 ~ dnorm(0, 0.5),
            sigma ~ dexp(1)) %>% 
  quap(., data = dfr) %>% 
  precis() %>% 
  as_tibble(rownames = "estimate")

# build data frame for comparison
full_join(m1, m2) %>% 
  full_join(m3) %>% 
  add_column(model = rep(paste("Model", 1:3), c(3, 3, 4))) %>% 
  filter(estimate %in% c("B1", "B2")) %>% 
  mutate(combined = str_c(model, estimate, sep = ": ")) %>% 
  rename(lower_pi = '5.5%', upper_pi = '94.5%') %>% 
  ggplot() +
  geom_pointrange(aes(x = mean, xmin = lower_pi, xmax = upper_pi,  
                      combined, colour = estimate), size = 1, 
                  show.legend = FALSE) +
  geom_vline(xintercept = 0, colour = "grey20", 
             linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = c("firebrick", "steelblue")) +
  labs(y = NULL, x = "Estimate") +
  theme_classic()

# let's make a dag for this
dag_5M1 <- dagitty( "dag {
                      Predictor1 -> Predictor2 -> Outcome
                      Predictor1 -> Outcome
                      }") 
coordinates(dag_5M1) <- list(x = c(Predictor1 = 0, Predictor2 = 2, Outcome = 1),
                             y = c(Predictor1 = 0, Predictor2 = 0, Outcome = 1))
drawdag(dag_5M1)
                     

### 5M2 ###

# Invent your own example of a masked relationship. An outcome variable should
# be correlated with both predictor variables, but in opposite directions. And
# the two predictor variables should be correlated with one another.

N <- 100
dfr <- tibble(pred_1 = rnorm(N, sd = 3), 
              pred_2 = rnorm(N, pred_1, sd = 0.5), 
              out_var = rnorm(N, pred_1 - pred_2)) %>% 
  mutate(across(everything(), scale))

# outcome and predictor 1 are positively correlated in a bivariate regression
m1 <- alist(out_var ~ dnorm(mu, sigma),
            mu <- a + B1*pred_1,
            a ~ dnorm(0, 0.2), 
            B1 ~ dnorm(0, 0.5),
            sigma ~ dexp(1)) %>% 
  quap(., data = dfr) %>% 
  precis() %>% 
  as_tibble(rownames = "estimate")



# outcome and predictor 2 are negatively correlated in a bivariate regression
m2 <- alist(out_var ~ dnorm(mu, sigma),
            mu <- a + B2*pred_2,
            a ~ dnorm(0, 0.2), 
            B2 ~ dnorm(0, 0.5),
            sigma ~ dexp(1)) %>% 
  quap(., data = dfr) %>% 
  precis() %>% 
  as_tibble(rownames = "estimate")



# now the multiple linear regression 
m3 <- alist(out_var ~ dnorm(mu, sigma),
            mu <- a + B1*pred_1 + B2*pred_2,
            a ~ dnorm(0, 0.2),
            B1 ~ dnorm(0, 0.5),
            B2 ~ dnorm(0, 0.5),
            sigma ~ dexp(1)) %>% 
  quap(., data = dfr) %>% 
  precis() %>% 
  as_tibble(rownames = "estimate")

# build data frame for comparison
full_join(m1, m2) %>% 
  full_join(m3) %>% 
  add_column(model = rep(paste("Model", 1:3), c(3, 3, 4))) %>% 
  filter(estimate %in% c("B1", "B2")) %>% 
  mutate(combined = str_c(model, estimate, sep = ": ")) %>% 
  rename(lower_pi = '5.5%', upper_pi = '94.5%') %>% 
  ggplot() +
  geom_pointrange(aes(x = mean, xmin = lower_pi, xmax = upper_pi,  
                      combined, colour = estimate), size = 1, 
                  show.legend = FALSE) +
  geom_vline(xintercept = 0, colour = "grey20", 
             linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = c("firebrick", "steelblue")) +
  labs(y = NULL, x = "Estimate") +
  theme_classic()

# let's make a dag for this
dag_5M2 <- dagitty( "dag {
                      Predictor1 <-  Unobserved -> Predictor2
                      Predictor1 -> Outcome <- Predictor2
                      }") 
coordinates(dag_5M2) <- list(x = c(Predictor1 = 0, Outcome = 1, Unobserved = 1, Predictor2 = 2),
                             y = c(Predictor1 = 0, Unobserved = 0, Predictor2 = 0, Outcome = 1))
drawdag(dag_5M2)


### 5M3 ### 

# It is sometimes observed that the best predictor of fire risk is the presence
# of firefighters—States and localities with many firefighters also have more
# fires. Presumably firefighters do not cause fires. Nevertheless, this is not a
# spurious correlation. Instead fires cause firefighters. Consider the same
# reversal of causal inference in the context of the divorce and marriage data.
# How might a high divorce rate cause a higher marriage rate? Can you think of a
# way to evaluate this relationship, using multiple regression?

# after a divorce, there are two new individuals on the "wedding market".
# Divorce rate could hence be related to marriage rate by increasing the pool of
# potential individuals one can marry. This could be tested by tracking each
# individual after a divorce to see whether they get re-married again. This
# re-marriage rate could then be used in a multiple linear regression framework,
# where marriage rate is the outcome, and divorce rate and re-marriage rate are
# the predictors. If divorce rate was related to marriage rate in a bivariate
# regression framework, but not when adding re-marriage rate in a multiple
# regression, then re-marriage is the driving force for the spurious correlation
# between divorce and marriage rate.


### 5M4 ###

# In the divorce data, States with high numbers of Mormons (members of The
# Church of Jesus Christ of Latter-day Saints, LDS) have much lower divorce
# rates than the regression models expected. Find a list of LDS population by
# State and use those numbers as a predictor variable, predicting divorce rate
# using marriage rate, median age at marriage, and percent LDS population
# (possibly standardized). You may want to consider transformations of the raw
# percent LDS variable.

data("WaffleDivorce")

d_waffle <- WaffleDivorce %>% 
  as_tibble() %>% 
  select(marriage = Marriage, age_marriage = MedianAgeMarriage, 
         divorce = Divorce, loc = Loc)

# using the downloadable csv data from worldpoulationreview:
# https://worldpopulationreview.com/state-rankings/mormon-population-by-state
mormons <- read_csv(file = "mormons.csv") %>% 
  mutate(lds = mormonPop/Pop) %>% 
  select(loc = State, lds)

# load state data contained in base r (datasets)
data("state")

state <- tibble(abbr = state.abb, loc = state.name)

# bind with mormons data by loc
d_waffle_sd <- mormons %>% 
  full_join(state) %>% 
  select(-loc, loc = abbr) %>% 
  # bind with marriage data by loc
  full_join(d_waffle) %>% 
  drop_na() %>% 
  # standardise to z-scores
  mutate(across(is.numeric, standardize))

# already the first value for lds shows a z-value above 6 for utah. This data is too skewed 
# and I am going to use the log of lds instead

d_waffle_sd <- mormons %>% 
  full_join(state) %>% 
  select(-loc, loc = abbr) %>% 
  # bind with marriage data by loc
  full_join(d_waffle) %>% 
  drop_na() %>% 
  # log transform lds
  mutate(log_lds = log(lds)) %>% 
  # standardise to z-scores
  mutate(across(is.numeric, standardize))


m_lds <- alist(divorce ~ dnorm(mu, sigma), 
      mu <- a + Ba*age_marriage + Bm*marriage + Bl*log_lds, 
      a ~ dnorm(0, 0.2), 
      Ba ~ dnorm(0, 0.5), 
      Bm ~ dnorm(0, 0.5), 
      Bl ~ dnorm(0, 0.5),
      sigma ~ dexp(1)) %>% 
  quap(., data = d_waffle_sd)

precis(m_lds)

# make a plot
precis(m_lds) %>% 
  as_tibble(rownames = "estimate") %>% 
  filter(str_detect(estimate, "^B")) %>% 
  rename(lower_pi = '5.5%', upper_pi = '94.5%') %>% 
  mutate(estimate = c("Age at marriage", "Marriage rate", "Log Mormons [%]")) %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_pointrange(aes(x = mean, xmin = lower_pi, xmax = upper_pi, estimate),
                  colour = "grey25", size = 0.7, show.legend = FALSE) +
  labs(x = "Estimate", y = NULL, title = "Outcome = Divorce rate") +
  theme_classic()

# the magnitude in percentage of LDS per state is negatively related to divorce
# rate. there is no longer a consistent trend for marriage rate. age at marriage
# is still negatively related to divorce rate.

# states were people were getting married at a higher age as well as states with
# higher percentages of Mormons have lower divorce rates


### 5M5 ###

# One way to reason through multiple causation hypotheses is to imagine detailed
# mechanisms through which predictor variables may influence outcomes. For
# example, it is sometimes argued that the price of gasoline (predictor
# variable) is positively associated with lower obesity rates (outcome
# variable). However, there are at least two important mechanisms by which the
# price of gas could reduce obesity. First, it could lead to less driving and
# therefore more exercise. Second, it could lead to less driving, which leads to
# less eating out, which leads to less consumption of huge restaurant meals. Can
# you outline one or more multiple regressions that address these two
# mechanisms? Assume you can have any predictor data you need.

# One could use a multiple regression framework with three predictors, the first
# one being price of gasoline. For the second one, we need to track the time
# spent walking of each individual to measure the effect of driving less. For
# the third one, we need to track the frequency of meals consumed at restaurants
# for each individual. a potential model could hence be:

# $$\mu_i = \alpha + \beta_g G_i + \beta_w W_i + \beta_f  F_i$$
# where mu is the mean obesity rate, G the price of gasoline, W the walking rate
# (per day), and F the amount of restaurant food



# hard question online ----------------------------------------------------


# All three exercises below use the same data,data(foxes)(part of
# rethinking).The urban fox (Vulpes vulpes) is a successful exploiter of human
# habitat. Since urban foxes move in packs and defend territories, data on
# habitat quality and population density is also included. The data frame has
# five columns:

# - (1) group: Number of the social group the individual fox belongs to
# - (2) avgfood: The average amount of food available in the territory
# - (3) groupsize: The number of foxes in the social group
# - (4) area: Size of the territory
# - (5) weight: Body weight of the individual fox


### 5H1 ###


# Fit two bivariate Gaussian regressions, using quap: (1) body weight as a
# linear function of territory size (area), and (2) body weight as a linear
# function of groupsize. Plot the results of these regressions, displaying the
# MAP regression line and the 95% interval of the mean. Is either variable
# important for predicting fox body weight?

# data is already loaded in homework
# with standarised predictors
foxes_std

m_1 <- alist(weight ~ dnorm(mu, sigma), 
      mu <- a + Ba*area, 
      a ~ dnorm(0, 0.2), 
      Ba ~ dnorm(0, 0.5), 
      sigma ~ dexp(1)) %>% 
  quap(., data = foxes_std)

s <- seq(-2, 2, 0.1)
N <- 1e3

m_1 %>% 
  link(data = list(area = s), n = N) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), values_to = "pred_weight") %>% 
  add_column(area = rep(s, N)) %>% 
  group_by(area) %>% 
  nest() %>% 
  mutate(pred_weight = map(data, "pred_weight"), 
         mean_weight = map_dbl(pred_weight, mean), 
         pi = map(pred_weight, PI), 
         lower_pi = map_dbl(pi, pluck(1)), 
         upper_pi = map_dbl(pi, pluck(2))) %>% 
  select(area, mean_weight, lower_pi, upper_pi) %>% 
  ggplot() +
  geom_ribbon(aes(area, ymin = lower_pi, ymax = upper_pi), 
              fill = "grey40", alpha = 0.85) +
  geom_line(aes(area, mean_weight), 
            size = 1.5, colour = "orange") +
  labs(title = "Weight ~ Area", x = "Area", y = "Weight") +
  theme_minimal()

precis(m_1)

# second part
m_2 <- alist(weight ~ dnorm(mu, sigma), 
             mu <- a + Bg*groupsize, 
             a ~ dnorm(0, 0.2), 
             Bg ~ dnorm(0, 0.5), 
             sigma ~ dexp(1)) %>% 
  quap(., data = foxes_std)


m_2 %>% 
  link(data = list(groupsize = s), n = N) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), values_to = "pred_weight") %>% 
  add_column(groupsize = rep(s, N)) %>% 
  group_by(groupsize) %>% 
  nest() %>% 
  mutate(pred_weight = map(data, "pred_weight"), 
         mean_weight = map_dbl(pred_weight, mean), 
         pi = map(pred_weight, PI), 
         lower_pi = map_dbl(pi, pluck(1)), 
         upper_pi = map_dbl(pi, pluck(2))) %>% 
  select(groupsize, mean_weight, lower_pi, upper_pi) %>% 
  ggplot() +
  geom_ribbon(aes(groupsize, ymin = lower_pi, ymax = upper_pi), 
              fill = "grey40", alpha = 0.85) +
  geom_line(aes(groupsize, mean_weight), 
            size = 1.5, colour = "orange") +
  labs(title = "Weight ~ Groupsize", x = "Groupsize", y = "Weight") +
  theme_minimal()

precis(m_2)

# groupsize shows a consistent and negative relationship with weight


### 5H2 ###


# Now fit a multiple linear regression with weight as the outcome and both area
# and groupsize as predictor variables. Plot the predictions of the model for
# each predictor, holding the other predictor constant at its mean. What does
# this model say about the importance of each variable? Why do you get different
# results than you got in the exercise just above?

m_3 <- alist(weight ~ dnorm(mu, sigma), 
             mu <- a + Ba*area + Bg*groupsize, 
             a ~ dnorm(0, 0.2), 
             Ba ~ dnorm(0, 0.5), 
             Bg ~ dnorm(0, 0.5), 
             sigma ~ dexp(1)) %>% 
  quap(., data = foxes_std)

# one advantage of standardising the predictor variables is that we know that
# their mean is approximately zero
near(mean(foxes_std$area), 0)

# weight vs area while groupsize = 0
# use s and N defined above
list(area = s, groupsize = 0) %>% 
  link(m_3, data = ., n = N) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), values_to = "pred_weight") %>% 
  add_column(area = rep(s, N)) %>% 
  group_by(area) %>% 
  nest() %>% 
  mutate(pred_weight = map(data, "pred_weight"), 
         mean_weight = map_dbl(pred_weight, mean), 
         pi = map(pred_weight, PI), 
         lower_pi = map_dbl(pi, pluck(1)), 
         upper_pi = map_dbl(pi, pluck(2))) %>% 
  select(area, mean_weight, lower_pi, upper_pi) %>% 
  ggplot() +
  geom_ribbon(aes(area, ymin = lower_pi, ymax = upper_pi), 
              fill = "grey40", alpha = 0.85) +
  geom_line(aes(area, mean_weight), 
            size = 1.5, colour = "orange") +
  labs(title = "Groupsize = 0", x = "Manipulated Area", 
       y = "Counterfactual weight") +
  theme_minimal()

# same for weight vs groupsize while area = 0
list(groupsize = s, area = 0) %>% 
  link(m_3, data = ., n = N) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), values_to = "pred_weight") %>% 
  add_column(groupsize = rep(s, N)) %>% 
  group_by(groupsize) %>% 
  nest() %>% 
  mutate(pred_weight = map(data, "pred_weight"), 
         mean_weight = map_dbl(pred_weight, mean), 
         pi = map(pred_weight, PI), 
         lower_pi = map_dbl(pi, pluck(1)), 
         upper_pi = map_dbl(pi, pluck(2))) %>% 
  select(groupsize, mean_weight, lower_pi, upper_pi) %>% 
  ggplot() +
  geom_ribbon(aes(groupsize, ymin = lower_pi, ymax = upper_pi), 
              fill = "grey40", alpha = 0.85) +
  geom_line(aes(groupsize, mean_weight), 
            size = 1.5, colour = "orange") +
  labs(title = "Area = 0", x = "Manipulated Groupsize", 
       y = "Counterfactual Weight") +
  theme_minimal()

# simple example of a masked relationship. Area is positively related to weight,
# while groupsize is negatively related, cancelling each other out. The multiple
# regression can unmask this, showing the real relationships between the outcome
# and the predictors.


### 5H3 ###

# Finally, consider the avgfood variable. Fit two more multiple regressions: (1)
# body weight as an additive function of avgfood and groupsize, and (2) body
# weight as an additive function of all three variables,avgfood and groupsize
# and area. Compare the results of these models to the previous models you’ve
# fit, in the first two exercises. 


m_4 <- alist(weight ~ dnorm(mu, sigma), 
             mu <- a + Bf*avgfood + Bg*groupsize, 
             a ~ dnorm(0, 0.2), 
             Bf ~ dnorm(0, 0.5), 
             Bg ~ dnorm(0, 0.5), 
             sigma ~ dexp(1)) %>% 
  quap(. , data = foxes_std)


m_5 <- alist(weight ~ dnorm(mu, sigma), 
             mu <- a + Bf*avgfood + Bg*groupsize + Ba*area, 
             a ~ dnorm(0, 0.2), 
             Bf ~ dnorm(0, 0.5), 
             Bg ~ dnorm(0, 0.5), 
             Ba ~ dnorm(0, 0.5),
             sigma ~ dexp(1)) %>% 
  quap(. , data = foxes_std)

# define function to get tidy coefficient estimates from precis()

tidy_coef <- function(model_input) {
  suppressWarnings(
    
    model_input %>% 
      precis(.) %>% 
      as_tibble(rownames = "estimate") %>% 
      filter(str_detect(estimate, "^B"))
  
    )
}

list(m_1, m_2, m_3, m_4, m_5) %>% 
  map(tidy_coef) %>% 
  enframe(name = "model") %>%
  unnest(value) %>% 
  mutate(coef_mod = str_c("Model", model, sep = " "), 
         coef_mod = str_c(coef_mod, estimate, sep = ": ")) %>% 
  rename(lower_pi = '5.5%', upper_pi = '94.5%')  %>%  
  ggplot() +
  geom_vline(xintercept = 0, colour = "grey40")  +
  geom_pointrange(aes(x = mean, xmin = lower_pi, xmax = upper_pi, y = coef_mod, 
                  colour = estimate)) +
  scale_colour_discrete(name = "Predictor", labels = c("Area", "Food", "Groupsize")) +
  scale_y_discrete(labels = c("Model 1", "Model 2", "", "Model 3", 
                              "", "Model 4", "", "", "Model 5")) +
  geom_hline(yintercept = c(0.5, 1.5, 2.5, 4.5, 6.5, 9.5),
             linetype = "dotted", colour = "grey80") +
  labs(x = "Estimate", y = NULL) +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank())

# food is positively related to weight in a model with avgfood and groupsize as
# predictors. This relationship is lost when adding groupsize as a predictor to
# the model


# (a) Is avgfood or area a better predictor of bodyweight? If you had to choose
# one or the other to include in a model, which would it be? Support your
# assessment with any tables or plots you choose.

# Comparing Model 3, 4, and 5 (see plot above) shows that avgfood generally has
# a higher effect on weight than area, even if the uncertainty is a bit higher.
# I would therefore choose avgfood.

# However, I think that this really depends on the research question and what is
# already known about fox behaviour. Looking at the coefficient estimates, both
# are positively related to weight, but effects are reduced when they are in the
# same model (see b below) Assuming that more area for a fox group increases
# their access to food, I would use area as it is the direct causal variable.
# But it could as well be that more food increases the area you can roam as a
# fox, as you have more power. In this case, I would use food as a predictor.


# (b) When both avgfood or area are in the same model, their effects are reduced
# (closer to zero) and their standard errors are larger than when they are
# included in separate models. Can you explain this result?

# Area and avgfood are strongly correlated: 
ggplot(foxes_std) +
  geom_point(aes(area, avgfood), size = 2.5, shape = 21, 
             fill = "grey50", colour = "grey20") +
  labs(y = "Food", x = "Area") +
  theme_minimal()

# this phenomen is called multicollinearity. When adding both parameters as
# predictors in a multiple regression, the partial effect of each becomes
# smaller after controlling for the other (this is what we can see when
# comparing Model 3, 4, and 5). It could be that both parameters share a common
# unobserved cause, or that one parameter causes the other. Either way, it would
# be wiser to include only one of these parameter (food OR area) in the final
# model.



# hard question print -----------------------------------------------------

### 5H1 ###

# In the divorce example, suppose the DAG is: M -> A -> D. What are the implied
# conditional independencies of the graph? Are the data consistent with it?

# let's make a dag
DMA_dag <- dagitty('dag{ M -> A -> D }')


coordinates(DMA_dag) <- list(x = c(M = 0, A = 1, D = 2), 
                             y = c(M = 1, A = 1, D = 1))

drawdag(DMA_dag)

impliedConditionalIndependencies(DMA_dag)
# D _||_ M | A 

# D is independent of M after conditioning on A
# so let's condition on A

m_1 <- alist(divorce ~ dnorm(mu, sigma), 
             mu <- a + BM*marriage + BA*age_marriage, 
             a ~ dnorm(0, 0.2), 
             BM ~ dnorm(0, 0.5), 
             BA ~ dnorm(0, 0.5), 
             sigma ~ dexp(1)) %>% 
  quap(. , data = d_waffle_sd)

# we can use s and N defined above
list(marriage = s, age_marriage = 0) %>% 
  link(m_1, data = ., n = N) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), values_to = "divorce") %>% 
  add_column(marriage = rep(s, N)) %>% 
  group_by(marriage) %>% 
  nest() %>% 
  mutate(divorce = map(data, "divorce"), 
         mean_divorce = map_dbl(divorce, mean), 
         pi = map(divorce, PI), 
         lower_pi = map_dbl(pi, pluck(1)), 
         upper_pi = map_dbl(pi, pluck(2))) %>% 
  select(marriage, mean_divorce, lower_pi, upper_pi) %>% 
  ggplot() +
  geom_ribbon(aes(marriage, ymin = lower_pi, ymax = upper_pi), 
              fill = "grey40", alpha = 0.85) +
  geom_line(aes(marriage, mean_divorce), 
            size = 1.5, colour = "orange") +
  labs(title = "Age = 0", x = "Manipulated Marriage Rate", 
       y = "Counterfactual Divorce Rate") +
  theme_minimal()

# Indeed,  D is independent of M after conditioning on A