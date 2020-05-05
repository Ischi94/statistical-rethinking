library(tidyverse)
library(rethinking)

# exercise 1 --------------------------------------------------------------

# Suppose the globe tossing data had turned out to be 8 water in 15 tosses.
# Construct the posterior distribution, using grid approximation. Use the
# same flat prior as before.

# define grid
p_grid <- seq(0, 1, length.out = 1000)

# set up probability vector with same length
prob_p <- rep(1, 1000)

# calculate probability data
prob_data_new <- dbinom(8, size = 15, prob = p_grid)
prob_data_old <- dbinom(6, size = 9, prob = p_grid)

# calculate posterior distribution
posterior_new <- prob_data_new * prob_p
posterior_old <- prob_data_old * prob_p

# normalise it
posterior_new <- posterior_new / sum(posterior_new)
posterior_old <- posterior_old / sum(posterior_old)

post_dist <- tibble(my_value = c(1:length(prob_p), 1:length(prob_p)), 
                    post_prob = c(posterior_old, posterior_new),
                    type = c(rep("A", 1000), rep("B", 1000)))

ggplot(post_dist, aes(x = my_value, y = post_prob, colour = type)) +
  geom_line(size = 2) +
  scale_colour_manual(values =c("grey70", "grey30")) +
  labs(x = "Index", y = "posterior") +
  theme_light()


# exercise 2 --------------------------------------------------------------

# Start over in 1, but now use a prior that is zero below p = 0:5 and a constant
# above p = 0:5. This corresponds to prior information that a majority
# of the Earth’s surface is water. What difference does the better prior make?
# If it helps, compare posterior distributions (using both priors) to the true
# value p = 0:7.

# set up probability vector with same length, with new values
prob_p <- c(rep(0, 500), rep(1, 500))

# calculate probability data
prob_data_upd <- dbinom(8, size = 15, prob = p_grid)

# calculate posterior distribution
posterior_upd <- prob_data_upd * prob_p

# normalise it
posterior_upd <- posterior_upd / sum(posterior_upd)

post_dist <- post_dist %>% add_row(my_value = 1:length(prob_p), 
                      post_prob = posterior_upd, 
                      type = rep("C", 1000))

ggplot(post_dist, aes(x = my_value, y = post_prob, colour = type)) +
  geom_line(size = 2) +
  scale_colour_manual(values =c("grey60", "grey40",  "grey5")) +
  labs(x = "Index", y = "posterior") +
  theme_light()

samples <- sample(p_grid, prob = posterior_upd, size = 1e4, replace = TRUE)

PI(samples, prob = 0.5)

# This problem is more open-ended than the others. Feel free to collaborate
# on the solution. Suppose you want to estimate the Earth’s proportion of
# water very precisely. Specifically, you want the 99% percentile interval of the
# posterior distribution of p to be only 0.05 wide. This means the distance between
# the upper and lower bound of the interval should be 0.05. How many
# times will you have to toss the globe to do this? I won’t require a precise
# answer. I’m honestly more interested in your approach.



# Chapter 2 ---------------------------------------------------------------

# 2M1 
# Recall the globe tossing model from the chapter. 
# Compute and plot the grid approximate posterior distribution for each of the 
# following sets of observations. In each case, assume a uniform prior for p.

# W,W,W
# W,W,W,L
# L,W,W,L,W,W,W

# define grid
p_grid <- seq(from = 0, to = 1, length.out = 20)

# define prior 
prior <- rep(1, 20)

# make function with uniform prior and grid of size 20
grid_approx <- function(W, L){
# compute likelihood at each value in grid
likelihood <- dbinom(W, size = W+L, prob = p_grid)
# compute product of likelihood and prior
unstd_posterior <- likelihood * prior
# standardise the posterior, so it sums to 1
posterior <- unstd_posterior / sum(unstd_posterior)
# plot it
plot(p_grid, posterior, type = "b")
}

grid_approx(3, 0)
grid_approx(3, 1)
grid_approx(5, 2)

# 2M2
# Now assume a prior for p that is equal to zero when p<0.5 and is a positive 
# constant when p≥0.5. Again compute and plot the grid approximate posterior 
# distribution for each of the sets of observations in the problem just above.

# update prior
prior <- ifelse(p_grid < 0.5, 0, 1)

grid_approx(3, 0)
grid_approx(3, 1)
grid_approx(5, 2)


# Chapter 3 ---------------------------------------------------------------

p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, size = 9, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE) %>% enframe()

# 3E1
# How much posterior probability lies below p = 0.2
samples %>% filter(value < 0.2) %>% summarise(p = n()/1e4)

# 3E2
# How much posterior probability lies above p = 0.8
samples %>% filter(value > 0.8) %>% summarise(p = n()/1e4)

# 3E3
# How much posterior probability lies between p = 0.2 and p = 0.8
samples %>% filter(value > 0.2 & value < 0.8) %>% summarise(p = n()/1e4)

# 3E4
# 20% of the posterior probability lies below which value of p?
samples %>% summarise(quants = quantile(value, probs = 0.2))

# 3E5
# 20% of the posterior probability lies above which value of p?
samples %>% summarise(quants = quantile(value, probs = 1 - 0.2))

# 3E6
# Which values of p contain the narrowest interval equal to 66% of the 
# posterior probability?
samples %>% {HPDI(.$value, prob = 0.66)}

# 3E7
# Which values of p contain 66% of the posterior probability, 
# assuming equal posterior probability both below and above the interval?
samples %>% {PI(.$value, prob = 0.66)}

# 3M1
# Suppose the globe tossing data had turned out to be 8 water in 15 tosses. 
# Construct the posterior distribution, using grid approximation. 
# Use the same flat prior as before.
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(8, size = 15, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
plot(p_grid, posterior, type = "l")

# 3M2
# Draw 10,000 samples from the grid approximation from above. 
# Then use the samples to calculate the 90% HPDI for p.
samples <- p_grid %>% 
  sample(size = 1e4, prob = posterior, replace = TRUE) %>% 
  enframe()

samples %>% {HPDI(.$value, prob = 0.9)}

# 3M3
# Construct a posterior predictive check for this model and data. 
# This means simulate the distribution of samples, averaging over the posterior
# uncertainty in p. What is the probability of observing 8 water in 15 tosses?
w_eight <- rbinom(1e4, size = 15, prob = samples$value) %>%
  enframe(name = "n_row", value = "trials")

ggplot(w_eight) +
  geom_histogram(aes(x = trials), binwidth = .5) +
  scale_x_continuous(breaks = seq(0,15,1))

w_eight %>% filter(trials == 8) %>% summarise(p_eight = n()/1e4)

# 3M4
# Using the posterior distribution constructed from the new (8/15) data,
# now calculate the probability of observing 6 water in 9 tosses.
w_six <- rbinom(1e4, size = 9, prob = samples$value) %>%
  enframe(name = "n_row", value = "trials")
w_six %>% filter(trials == 6) %>% summarise(p_six = n()/1e4)
ggplot(w_six) +
  geom_histogram(aes(x = trials), binwidth = .5) +
  scale_x_continuous(breaks = seq(0,9,1))

# 3M5
# Start over at 3M1, but now use a prior that is zero below p=0.5 and a 
# constant above p=0.5. This corresponds to prior information that a majority of the 
# Earth’s surface is water. Repeat each problem above and compare the inferences. 
# What difference does the better prior make? If it helps, 
# compare inferences (using both priors) to the true value p=0.7
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- if_else(p_grid < 0.5, 0,1)
likelihood <- dbinom(8, size = 15, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
plot(p_grid, posterior, type = "l")

# start with 3M2 
samples <- p_grid %>% 
  sample(size = 1e4, prob = posterior, replace = TRUE) %>% 
  enframe() 
samples %>% 
  {HPDI(.$value, prob = 0.9)}

# then 3M3
rbinom(1e4, size = 15, prob = samples$value) %>%
  enframe(name = "n_row", value = "trials") %>%
  filter(trials == 8) %>% summarise(p_eight = n()/1e4)

# then 3M4
rbinom(1e4, size = 9, prob = samples$value) %>%
  enframe(name = "n_row", value = "trials") %>% 
  filter(trials == 6) %>% summarise(p_six = n()/1e4)

# hard
data(homeworkch3)
birth1
birth2

# 3H1
# Using grid approximation, compute the posterior distribution for the probability
# of a birth being a boy. Assume a uniform prior probability.
# Which parameter value maximizes the posterior probability?
allbirths <- c(birth1, birth2) %>% enframe(name = "n_row", value = "child")
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(sum(allbirths$child), size = length(allbirths$n_row), 
                     prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
plot(p_grid, posterior, type = "l")

posterior %>% which.max(.) %>% p_grid[.]

# 3H2
# Using the sample() function, draw 10000 random parameter values from the posterior 
# distribution you calculated above. Use these samples to estimate the 50%, 
# 89% and 97% highest posterior density intervals.

samples <- posterior %>% sample(p_grid, prob = .,  size = 1e4, replace = TRUE)

samples %>% HPDI(., prob = 0.5)
samples %>% HPDI(., prob = 0.89)
samples %>% HPDI(., prob = 0.97)

#3H3
# Use rbinom() to simulate 10000 replicates of 200 births. 
# You should end up with 10000 numbers, each one a count of boys out of 200 births.
# Compare the distribution of predicted numbers of boys to the actual count in the 
#data (111 boys out of 200 births). 
# There are many good ways to visualize the simulations, but the dens() command 
# (part of the rethinking package) is probably the easiest way in this case. 
# Does it look like the model fits the data well? 
# That is, does the distribution of predictions include the actual observation as a 
# central, likely outcome?
birth_sim <- rbinom(1e4, 200, prob = samples) %>% enframe()

ggplot(birth_sim) +
  geom_density(aes(x = value), size = 1.3) +
  geom_vline(xintercept = sum(allbirths$child), colour = "coral", size = 2)

# 3H4
# Now compare 10000 counts of boys from 100 simulated first borns only to the number 
# of boys in the first births, birth1. How does the model look in this light?
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(sum(birth1), size = length(birth1), 
                     prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

samples <- posterior %>% sample(p_grid, prob = .,  size = 1e4, replace = TRUE)

birth_sim <- rbinom(1e4, 100, prob = samples) %>% enframe()

ggplot(birth_sim) +
  geom_density(aes(x = value), size = 1.3) +
  geom_vline(xintercept = sum(birth1), colour = "coral", size = 2)

# 3H5
# The model assumes that sex of first and second births are independent. 
# To check this assumption, focus now on second births that followed female first borns. 
# Compare 10000 simulated counts of boys to only those second births that followed 
# girls. To do this correctly, you need to count the number of first borns who 
# were girls and simulate that many births, 10000 times. 
# Compare the counts of boys in your simulations to the actual observed count of boys
# following girls. How does the model look in this light? 
# Any guesses what is going on in these data?
second_births <- birth2[birth1 == 0] %>% sum(.)
nr_girls <- 100 - sum(birth1)

boys_sim <- rbinom(1e4, nr_girls, prob = samples) %>% enframe()

ggplot(boys_sim) +
  geom_density(aes(x = value)) +
  geom_vline(xintercept = second_births)
