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
  geom_density(aes(value), size = 1) + 
  theme_light()

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

# 4m7 
# Refit model m4.3 but omit the mean weight xbar this time. Compare the new model's 
# posterior to that of the original model. In particular, look at the covariance among
# the paramaters. What is different? Then compare the posterior predictions of both 
# models.
adults <- Howell1 %>% filter(age >= 18)

# old model
xbar <- adults %>% summarise(my_mean = mean(weight)) %>% pull()

m4.3 <- alist(height ~ dnorm(mu, sigma), # likelihood
                 mu <- a + b * (weight - xbar), # linear model
                 a ~ dnorm(178, 20), # alpha
                 b ~ dlnorm(0, 1), # beta
                 sigma ~ dunif(0, 50)) %>% 
  quap(., data = adults) # quadratic approximation


m4.3new <- alist(height ~ dnorm(mu, sigma), # likelihood
                 mu <- a + b*weight, # linear model
                 a ~ dnorm(178, 20), # alpha
                 b ~ dlnorm(0, 1), # beta
                 sigma ~ dunif(0, 50)) %>% 
  quap(., data = adults) # quadratic approximation

m4.3new %>% vcov() %>% round(digits = 3) # lots of covariation
m4.3 %>% vcov() %>% round(digits = 3)

# calculate slope and intercept
m4.3new %>% extract.samples() %>% as_tibble() %>% 
  summarise(intercept = mean(a), slope = mean(b))

m4.3 %>% extract.samples() %>% as_tibble() %>% 
  summarise(intercept = mean(a) - xbar, slope = mean(b))

ggplot(adults, aes(weight, height)) +
  geom_point() +
  geom_abline(intercept = 114, slope = 0.892, size = 1, colour = "orange") + # new
  geom_abline(intercept = 110, slope = 0.903, size = 1, colour = "red") + # old
  geom_smooth(method = "lm", se = FALSE) # traditional regression

# 4m8
# We used 15 knots with the cherry blossom data. Increase the number of knots and 
# observe what happends to the resulting spline. Then adjust also the width of the
# the prior on the weights - change the standard deviation of the prior and watch
# what happens. What do you think the combination of knot number and the prior
# on the weights control?
data("cherry_blossoms")

cherry_blossoms <- cherry_blossoms %>%
  as_tibble() %>%  
  drop_na()

# define nr of knots
library(splines)

# make function for it 
cherry_spliner <- function(nr_knots, vl_sigma){

# get knot points
knot_list <- cherry_blossoms$year %>% 
  quantile(probs = seq(0, 1, length.out = nr_knots)) %>% 
  discard(~ . %in% c(851, 1980))

B <- cherry_blossoms$year %>% bs(knots = knot_list, degree = 3, intercept = TRUE) 

# run bspline regressions
m4.7 <- alist(D ~ dnorm(mu, sigma), 
              mu <- a + B %*% w, 
              a ~ dnorm(100, 10), 
              w ~ dnorm(0, 10), 
              sigma ~ dexp(vl_sigma)) %>% 
  quap(., data = list(D = cherry_blossoms$doy, B = B), 
                      start = list(w = rep(0, ncol(B))))

# et 97% posterior interval for mean
m4.7 %>% 
  link() %>% as_tibble() %>% 
  map_dfr(PI, prob = 0.97) %>% 
  select("lower" = '2%', "upper" = '98%') %>% 
  add_column(year = cherry_blossoms$year)
}


splines_nr <- cherry_spliner(5, 100)

cherry_blossoms$lower_pi <- splines_nr %>% select(lower) %>% pull()
cherry_blossoms$upper_pi <- splines_nr %>% select(upper) %>% pull()

ggplot(cherry_blossoms, aes(year, doy)) +
  geom_point() +
  geom_ribbon(aes(ymin = lower_pi, ymax = upper_pi))

# 4H1
# predict heights for individuals from model output based on weights.
# provide 89% Intervals for each. 

# data
d <- Howell1

# model formula
formula <- alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b * weight,
  a ~ dnorm(178, 20),
  b ~ dlnorm(0, 1),
  sigma ~ dunif(0, 50))

# model fit
m <- map(formula, data = d)

# new data to predict from 
new_weight <- c(46.95, 43.72, 64.78, 32.59, 54.63)

# predict height using link() 
pred_height <- link(m, data = data.frame(weight = new_weight))

expected <- pred_height %>% 
  as_tibble() %>% 
  summarise_all(mean) %>% 
  as_vector()

interval <- pred_height %>% 
  as_tibble() %>% 
  summarise_all(HPDI, prob = 0.89) %>% 
  as_vector()

# combine each in a dataframe
predictions <- tibble(individual = 1:5, weight = new_weight, expected = expected, 
       lower = interval[c(TRUE, FALSE)], upper = interval[c(FALSE, TRUE)])

# 4H2
# select out all the rows in the Howell1 data with ages below 18 years of age. 
# If you do it right, you should end up with a new data frame with 192 rows in it.
young <- d %>% filter(age < 18)

# (a) Fit a linear regression to these data, using map(). 
# Present and interpret the estimates. 
# For every 10 units of increase in weight, 
# how much taller does the model predict a child gets?
# model formula
formula_young <- alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b * weight,
  a ~ dnorm(120, 30),
  b ~ dlnorm(0, 1),
  sigma ~ dunif(0, 60))

# model fit
m_young <- map(formula_young, data = young)
# results
m_young_res <- precis(m_young) %>% as_tibble() %>% 
  add_column(parameter = rownames(precis(m_young))) %>% 
  rename("lower" = '5.5%', "upper" = '94.5%')


# make coefficient plot
ggplot(m_young_res) +
  geom_point(aes(x = mean, y = parameter)) +
  geom_linerange(aes(xmin = lower, xmax = upper, y = parameter)) +
  labs(x = "value") +
  theme_light()

# for every 10 units of increase in weight, 
# the model predicts that a child gets 27 cm taller

# Plot the raw data, with height on the vertical axis and weight on the horizontal axis.
# Superimpose the MAP regression line and 89% HPDI for the mean. 
# Also superimpose the 89% HPDI for predicted heights.

# define weight range
weight_seq <- seq(from = min(young$weight), to = max(young$weight), by = 1)

# calculate 89% intervals for each weight
intervals <- link(m_young, data = data.frame(weight = weight_seq)) %>% 
  as_tibble() %>% 
  summarise_all(HPDI, prob = 0.89) %>% 
  add_column(type = c("lower", "upper")) %>% 
  pivot_longer(cols = -type, names_to = "cols", values_to = "intervals") %>% 
  add_column("weight" = rep(weight_seq, 2)) %>% 
  pivot_wider(names_from = type, values_from = intervals)

# calculate prediction intervals
pred_intervals <- sim(m_young, data = data.frame(weight = weight_seq)) %>% 
  as_tibble() %>% 
  summarise_all(PI, prob = 0.89) %>% 
  add_column(type = c("lower", "upper")) %>% 
  pivot_longer(cols = -type, names_to = "cols", values_to = "intervals") %>% 
  add_column("weight" = rep(weight_seq, 2)) %>% 
  pivot_wider(names_from = type, values_from = intervals)

# plot it
ggplot(young) +
  geom_point(aes(x = weight, height)) +
  geom_abline(intercept = m_young_res["a","mean"], 
              slope = m_young_res["b","mean"], size = .8) + 
  geom_ribbon(aes(x = weight, ymin = lower, ymax = upper), 
              alpha=0.6, data = intervals) +
  geom_ribbon(aes(x = weight, ymin = lower, ymax = upper), 
              alpha=0.2, data = pred_intervals) +
  theme_light()
  
# What aspects of the model fit concern you? 
# Describe the kinds of assumptions you would change, if any, to improve the model. 
# You don’t have to write any new code. 
# Just explain what the model appears to be doing a bad job of, 
# and what you hypothesize would be a better model.
# --> no linear fit, generalised models would do better

