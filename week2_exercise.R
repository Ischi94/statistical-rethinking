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

# we better make a function out of it since we use it more often
tidy_intervals <- function(my_function, my_model, interval_type, 
                           x_var, x_seq){
  # preprocess dataframe
  df <- data.frame(col1 = x_seq)
  colnames(df) <- x_var
  
  # calculate 89% intervals for each weight
  my_function(my_model, data = df) %>% 
  as_tibble() %>% 
  summarise_all(interval_type, prob = 0.89) %>% 
  add_column(type = c("lower", "upper")) %>% 
  pivot_longer(cols = -type, names_to = "cols", values_to = "intervals") %>% 
  add_column(x_var = rep(x_seq, 2)) %>% 
  pivot_wider(names_from = type, values_from = intervals)
}

# highest posterior density (HPDI)
intervals <- tidy_intervals(link, m_young, HPDI, x_var = "weight", x_seq = weight_seq)

# percentily prediction 
# calculate prediction intervals
pred_intervals <- tidy_intervals(sim, m_young, PI, 
                                 x_var = "weight", x_seq = weight_seq)

# likewise, let's make a plotting function for this
plot_regression <- function(df, x, y, # results, 
                            interv = intervals, pred_interv = pred_intervals){
  ggplot(df) +
    geom_point(aes({{x}}, {{y}})) +
    # geom_abline(intercept = {{results}} %>% filter(parameter == "a") %>%
    #               select(mean) %>% pull,
    #             slope = {{results}} %>% filter(parameter == "b") %>%
    #               select(mean)%>% pull,
    #             size = .8) +
    geom_ribbon(aes(x = x_var, ymin = lower, ymax = upper),
                alpha=0.8, data = interv) +
    geom_ribbon(aes(x = x_var, ymin = lower, ymax = upper),
                alpha=0.2, data = pred_interv) +
    theme_light()
}

# plot it
plot_regression(young, weight, height)

  

# What aspects of the model fit concern you? 
# Describe the kinds of assumptions you would change, if any, to improve the model. 
# You don’t have to write any new code. 
# Just explain what the model appears to be doing a bad job of, 
# and what you hypothesize would be a better model.
# --> no linear fit, generalised models would do better

# Suppose a colleague of yours, who works on allometry, glances at the practice 
# problems just above. Your colleague exclaims, “That’s silly. 
# Everyone knows that it’s only the logarithm of body weight that scales with height!”
# Let’s take your colleague’s advice and see what happens.
# 
# (a) Model the relationship between height (cm) and the natural logarithm of weight 
# (log-kg). Use the entire Howell1 data frame, all 544 rows, adults and non-adults. 
# Fit this model, using quadratic approximation:
# The function for computing a natural log in R is just log().
# Can you interpret the resulting estimates?
# model formula
formula_log <- alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b * log(weight),
  a ~ dnorm(178, 100),
  b ~ dlnorm(0, 1),
  sigma ~ dunif(0, 50))

# model fit
m_log <- map(formula_log, data = d)
m_log_res <- precis(m_log) %>% as_tibble() %>% 
  add_column(parameter = rownames(precis(m_log))) %>% 
  rename("lower" = '5.5%', "upper" = '94.5%')

# Begin with this plot:

plot(height ~ weight, data = Howell1, col = col.alpha(rangi2, 0.4))

# Then use samples from the quadratic approximate posterior of the model in (a) 
# to superimpose on the plot: 
# (1) the predicted mean height as a function of weight, 
# (2) the 97% HPDI for the mean, and 
# (3) the 97% HPDI for predicted heights.

# define weight range
weight_seq <- seq(from = min(d$weight), to = max(d$weight), by = 1)

# calculate 89% intervals for each weight
intervals <- tidy_intervals(link, m_log, HPDI, 
                            x_var = "weight", x_seq = weight_seq)

# calculate means
reg_line <- link(m_log, data = data.frame(weight = weight_seq)) %>% 
  apply(., 2, mean) %>% 
  as_tibble() %>% 
  add_column(weight = weight_seq)


# calculate prediction intervals
pred_intervals <- tidy_intervals(sim, m_log, PI, 
                                 x_var = "weight", x_seq = weight_seq)


# plot it 
plot_regression(d, weight, height)


# 4H4
# plot the prior predictive distribution for the parabolic polynomial regression
# model by modifying the code that plots the linear regression prior predictive 
# distribution. Can you modify the prior distributions of a, b1, and b2 so that the
# prior predictions stay withing the biologically reasonable outcome space?

# standardise weight
d_stand <- d %>% mutate(weight_s = (weight - mean(weight)) / sd(weight), 
              weight_s2 = weight_s^2) %>% 
  as_tibble()

# fit model
m_poly <- alist(height ~ dnorm(mu, sigma), 
               mu <- a + b1 * weight_s + b2 * weight_s2,
               a ~ dnorm(125, 30), 
               b1 ~ dlnorm(0, 1), 
               b2 ~ dnorm(0, 1), 
               sigma ~ dunif(0, 50)) %>% 
  quap(data = d_stand)

# set seed
set.seed(123)

m_poly_prior <- extract.prior(m_poly)
m_poly_mu <- link(m_poly, post = m_poly_prior, data = list(weight_s = c(-3, 3), 
                                                           weight_s2 = c(-3, 3))) %>% 
  as_tibble() %>% 
  rename("lower" = V1, "upper" = V2) %>% 
  add_column(x1 = -2, x2 = 2) %>% 
  # convert back to natural scale
  mutate(x1n = x1 * sd(d_stand$weight) + mean(d_stand$weight), 
         x2n = x2 * sd(d_stand$weight) + mean(d_stand$weight))

# plot it
ggplot(d_stand, aes(x = weight_s)) +
  geom_hline(yintercept = c(0, 272)) +
  geom_segment(aes(x = x1n, y = lower, xend = x2n, yend = upper), 
               alpha = 0.1, data = m_poly_mu) 


# seems like the prior for a is to high but thats ok. However, many lines have
# a negative slope for the prior b1 ~ dnorm(0, 10), 
# so we need to change the prior to b1 ~ dlnorm(0, 1) to force positive slopes. Better, 
# but maybe the prior for height is to detailled, set it to a ~ dnorm(125, 30). 

# 4H5
# return to cherry blossom data and model the association between blossom data (doy)
# and march temperature (temp). Note that there are many missing values in both 
# variables. You may consider a linear model, a polynomial, or a spline on temperature. 
# how well does temperature rend predict the blossom trend? 
data("cherry_blossoms")

cherry_blossoms <- cherry_blossoms %>%
  as_tibble() %>% 
  select(doy, temp) %>% 
  drop_na() %>% 
  mutate(temp_sc = (temp - mean(temp))/ sd(temp))

ggplot(cherry_blossoms) +
  geom_point(aes(temp_sc, doy))

# linear model
# define average temp
xbar <- cherry_blossoms %>% summarise(mean_temp = mean(temp)) %>% pull()

# fit modell
cherry_linear <- 
  # define formula
  alist(doy ~ dnorm(mu, sigma),
                       mu <- a + b * (temp - xbar), 
                       a ~ dnorm(115, 30),
                       b ~ dlnorm(0, 1), 
                       sigma ~ dunif(0, 50)) %>% 
  # calculate maximum a posterior
  quap(data = cherry_blossoms)

# make intervals
# define sequence of temp
temp_seq <- seq(from = min(cherry_blossoms$temp), 
                to = max(cherry_blossoms$temp), by = 0.5)

# calculate 89% intervals for each weight
intervals <- tidy_intervals(link, cherry_linear, HPDI, "temp", temp_seq)
  


# # calculate means
# reg_line <- link(cherry_linear, data = data.frame(temp = temp_seq)) %>% 
#   apply(., 2, mean) %>% 
#   as_tibble() %>% 
#   add_column(temp = temp_seq)


# calculate prediction intervals
pred_intervals <- tidy_intervals(sim, cherry_linear, PI, "temp", temp_seq)

  
# plot it 
plot_regression(cherry_blossoms, temp, doy)


# spline model
# define knots
num_knots <- 15
knot_list <- quantile(cherry_blossoms$temp, probs = seq(0, 1, length.out = num_knots))

# construct basis function
B <- bs(cherry_blossoms$temp, knots = knot_list[-c(1, num_knots)], 
        degree = 3, intercept = TRUE)

# fit modell
cherry_spline <- 
  # define formula
  alist(doy ~ dnorm(mu, sigma),
        mu <- a + B %*% w, 
        a ~ dnorm(100, 30),
        w ~ dnorm(0, 10), 
        sigma ~ dexp(1)) %>% 
  # calculate maximum a posterior
  quap(data = list(doy = cherry_blossoms$doy, B = B), 
       start = list(w = rep(0, ncol(B))))

intervals <- link(cherry_spline, data = cherry_blossoms) %>% 
  as_tibble() %>% 
  summarise_all(PI, prob = 0.89) %>% 
  add_column(type = c("lower", "upper")) %>% 
  pivot_longer(cols = -type, names_to = "cols", values_to = "intervals") %>% 
  add_column(temp = rep(quantile(cherry_blossoms$temp, 
                             probs = seq(0, 1, length.out = 787)), 2)) %>% 
  pivot_wider(names_from = type, values_from = intervals) %>% 
  rename(x_var = temp)

# calculate prediction intervals
pred_intervals <- sim(cherry_spline, data = cherry_blossoms) %>% 
  as_tibble() %>% 
  summarise_all(HPDI, prob = 0.89) %>% 
  add_column(type = c("lower", "upper")) %>% 
  pivot_longer(cols = -type, names_to = "cols", values_to = "intervals") %>% 
  add_column(temp = rep(quantile(cherry_blossoms$temp, 
                                 probs = seq(0, 1, length.out = 787)), 2)) %>% 
  pivot_wider(names_from = type, values_from = intervals) %>% 
  rename(x_var = temp)

# plot it 
plot_regression(cherry_blossoms, temp, doy)

# 4H6
# simulate the prior predictive distribution for the cherry blossom spline in the 
# chapter. Adjust the prior on the weights and observe what happens. What do you think
# the prior on the weights is doing?
cherry_blossoms <- cherry_blossoms %>%
  as_tibble() %>% 
  select(doy, year) %>% 
  drop_na(doy) 

# knots number
num_knots <- 15
# make knots
knot_list <- quantile(cherry_blossoms$year, probs = seq(0, 1, length.out = num_knots))

# construct basis function
B <- bs(cherry_blossoms$year, knots = knot_list[-c(1, num_knots)], 
        degree = 3, intercept = TRUE)

# fit modell
cherry_spline <- 
  # define formula
  alist(doy ~ dnorm(mu, sigma),
        mu <- a + B %*% w, 
        a ~ dnorm(100, 30),
        w ~ dnorm(0, 10), 
        sigma ~ dexp(1)) %>% 
  # calculate maximum a posterior
  quap(data = list(doy = cherry_blossoms$doy, B = B), 
       start = list(w = rep(0, ncol(B))))

# set seed
set.seed(123)

cherry_spline_prior <- extract.prior(cherry_spline)
cherry_spline_prior_dist <- link(cherry_spline, post = cherry_spline_prior, 
                  data = list(year = cherry_blossoms$year)) %>% 
  as_tibble() %>% 
  summarise_all(PI, prob = 0.89) %>% 
  add_column(type = c("lower", "upper")) %>% 
  pivot_longer(cols = -type, names_to = "cols", values_to = "intervals") %>% 
  add_column(year = rep(cherry_blossoms$year, 2)) %>% 
  pivot_wider(names_from = type, values_from = intervals) 

# plot it
ggplot(cherry_blossoms) +
  geom_point(aes(x = year, y = doy)) +
  geom_ribbon(aes(x = year, ymin = lower, ymax = upper),
              alpha=0.8, data = cherry_spline_prior_dist) +
  theme_minimal()

# percentile intervals fange from 150 to 50 for w ~ dnorm(0, 10), increasing the 
# standard deviation widens the PI. This probably means that w gives weight to each
# basis function at point B. 

# 4H8
# the cherry blossom spline in the chapter used an intercept a, but technically it
# doesn't require one. The first basis function could substitute for the intercept. 
# try refitting the cherry blossom spline without the intercept. what else about
# the model do you need to change to make this work?
# fit modell
cherry_spline <- 
  # define formula
  alist(doy ~ dnorm(mu, sigma),
        mu <- B %*% w, # removed intercept
        # a ~ dnorm(100, 30), -> removed intercept
        w ~ dnorm(100, 10), # change the mean of the weight prior to previous intercept
        sigma ~ dexp(10)) %>% # increase prior sigma, otherwise vcov will get negative
  # calculate maximum a posterior
  quap(data = list(doy = cherry_blossoms$doy, B = B), 
       start = list(w = rep(0, ncol(B))))

# calculate intervals
intervals <- link(cherry_spline, data = cherry_blossoms) %>% 
  as_tibble() %>% 
  summarise_all(PI, prob = 0.89) %>% 
  add_column(type = c("lower", "upper")) %>% 
  pivot_longer(cols = -type, names_to = "cols", values_to = "intervals") %>% 
  add_column(x_var = rep(cherry_blossoms$year, 2)) %>% 
  pivot_wider(names_from = type, values_from = intervals) 

# calculate prediction intervals
pred_intervals <- sim(cherry_spline, data = cherry_blossoms) %>% 
  as_tibble() %>% 
  summarise_all(HPDI, prob = 0.89) %>% 
  add_column(type = c("lower", "upper")) %>% 
  pivot_longer(cols = -type, names_to = "cols", values_to = "intervals") %>% 
  add_column(x_var = rep(cherry_blossoms$year, 2)) %>% 
  pivot_wider(names_from = type, values_from = intervals) 

# plot it 
plot_regression(cherry_blossoms, year, doy)
