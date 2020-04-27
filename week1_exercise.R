library(tidyverse)
library(rstan)

devtools::install_github("rmcelreath/rethinking")

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

# This problem is more open-ended than the others. Feel free to collaborate
# on the solution. Suppose you want to estimate the Earth’s proportion of
# water very precisely. Specifically, you want the 99% percentile interval of the
# posterior distribution of p to be only 0.05 wide. This means the distance between
# the upper and lower bound of the interval should be 0.05. How many
# times will you have to toss the globe to do this? I won’t require a precise
# answer. I’m honestly more interested in your approach.


