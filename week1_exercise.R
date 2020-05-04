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
