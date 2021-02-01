library(dagitty)
library(ggdag)
library(tidyverse)
library(rethinking)

map <- purrr::map

coral <- "#CD7672"
mint <- "#138086"
purple <- "#534666"
yellow <- "#EEb462"

tibble(colours = c(coral, mint, purple, yellow), 
       colourname = c("coral", "mint", "purple", "yellow")) %>% 
  arrange(colours) %>% 
  mutate(colourname = fct_reorder(colourname, colours)) %>% 
  ggplot() +
  geom_bar(aes(y = colours, fill = colours)) +
  scale_fill_identity() +
  geom_text(aes(x = 0.5, y = colours, label = colourname), 
            size = 6, colour = "white") +
  theme_void()

### 6E1 ###
# List three mechanisms by which multiple regression can produce false inferences about causal effect. 

# collinearity, post-treatment bias, collider bias.

### 6E2 ###
# For one of the mechanisms in the previous problem, provide an example of your
# choice, perhaps from your own research.

# Collinearity
# If you want to estimate the effect of geographic range on the extinction risk
# organism in the fossil record, you can choose between a range of potential
# parameters that express geographic range. For example, you can use the convex
# hull area or the maximum pairwise great circle distance. However, if you add
# both parameters in a model their true magnitude of association to extinction
# risk is lowered or even hidden, as they both encapsulate the same information.

# Post-treatment bias
# Assume you want to estimate the effect of global mean temperature on the
# extinction risk of marine species in the fossil record. Additionally, you have
# an amazing data set on continental shelve area through time and would love to
# include that as well. However, temperature is quite likely causally related to
# shelve area as it drives eustatic sea level. So including shelve area in a
# model would shut the path between temperature and extinction risk, even though
# there is a real causal association.

### 6E3 ###
# List the four elemental confounds. Can you explain the conditional dependencies of each?  

# Pipe
# X -> Y -> Z
dagitty("dag{
        X -> Y -> Z}") %>% 
  impliedConditionalIndependencies()
# X _||_ Z | Y
# If we condition on Y, we shut the path between X and Z. 

# Fork
# X <- Y -> Z
dagitty("dag{
        X <- Y -> Z}") %>% 
  impliedConditionalIndependencies()
# X _||_ Z | Y
# If we condition on Y, then learning X tells us nothing about Z. All the information is in Y.

# Collider
# X -> Y <- Z
dagitty("dag{
        X -> Y <- Z}") %>% 
  impliedConditionalIndependencies()
# X _||_ Z
# X is independent of Z. But conditioning on Y would open the path, and then X
# would be dependent on Z conditional on Y.

# Descendant
# X -> Y -> Z
# Y -> W
dagitty("dag{
        X -> Y -> Z
        Y -> W}") %>% 
  impliedConditionalIndependencies()
# W _||_ X | Y
# W _||_ Z | Y
# X _||_ Z | Y
# This is interesting. In the chapter, it says that if we would condition on W,
# we would condition on Y as well (to a lesser extent). So I would have expected
# X _||_ Z | W here.

### 6E4 ###
# How is a biased sample like conditioning on a collider? Think of the example at the open of the chapter. 

# Assume the collider X -> Y <- Z
# Conditioning on a collider Y  opens a path between X and Z, and leads to a
# spurious correlation between between these. This is similar to selection bias,
# where the researcher that sampled the data (or nature itself) cared about both
# X and Z when generating the sample.

### 6M1 ###

# Modify the DAG on page 190 (page 186 in print) to include the variable V, an unobserved cause of C
# and Y: C <- V -> Y. Reanalyze the DAG. How many paths connect X to Y? Which
# must be closed? Which variables should you condition on now?
tribble(
  ~ name,  ~ x,  ~ y,  
    "A",    1,     3,     
    "U",    0,     2,     
    "C",    2,     2,     
    "V",    3,     1,     
    "B",    1,     1,     
    "X",    0,     0,  
    "Y",    2,     0
) %>%  
  dagify(
    Y ~ X + C + V,
         C ~ V + A,
         B ~ C + U,
         U ~ A,
         X ~ U,
         coords = .) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_node(internal_colour = mint, alpha = 0.8, colour = "white") +
  geom_dag_text(aes(label = name), color = purple, size = 5) +
  geom_dag_edges(edge_color = purple) +
  labs(caption = "Figure 1: Adjusted DAG from the chapter, including V.") +
  theme_void()

# We are interested in the X -> Y path (1). 
# (1) X -> Y
# (2) X <- U <- A -> C -> Y
# (3) X <- U <- A -> C <- V -> Y
# (4) X <- U -> B <- C -> Y
# (5) X <- U -> B <- C <- V -> Y

# C is now a collider in path (3) and the path is closed. Additionally, both
# paths passing through B are closed as well as B stays a collider. The only
# open backdoor path is (2), and to close this path we can condition on A.

### 6M2 ###
# Sometimes, in order to avoid multicollinearity, people inspect pairwise
# correlations among predictors before including them in a model. This is a bad
# procedure, because what matters is the conditional association, not the
# association before the variables are included in the model. To highlight this,
# consider the DAG X -> Z -> Y. Simulate data from this DAG so that the
# correlation between X and Z is very large. The include both in a model
# prediction Y. Do you observe any multicollinearity? Why or why not? What is
# different from the legs example in the chapter?

sim_dat <- tibble(x = rnorm(1e4),  
                  z = rnorm(1e4, x, 0.5), 
                  y = rnorm(1e4, z)) %>% 
  mutate(across(everything(), standardize))

sim_dat %>% 
  summarise(correlation = cor(x, z))

alist(y ~ dnorm(mu, sigma), 
      mu <- a + bx*x + bz*z, 
      a ~ dnorm(0, 0.2), 
      c(bx, bz) ~ dnorm(0, 0.5), 
      sigma ~ dexp(1)) %>% 
  quap(data = sim_dat) %>% 
  precis() %>% 
  as_tibble(rownames = "estimate") %>% 
  filter(estimate %in% c("bx", "bz")) %>% 
  rename(lower_pi = `5.5%`, upper_pi = `94.5%`) %>% 
  ggplot() +
  geom_linerange(aes(xmin = lower_pi, xmax = upper_pi, y = estimate), 
                 size = 1.5, colour = mint) +
  geom_point(aes(x = mean, y = estimate), 
             shape = 21, colour = "grey20", stroke = 1, 
             size = 5, fill = purple) +
  labs(y = NULL, x = "Estimate", caption = "Figure 2: Coefficient plot for simulated data with collinearity.") +
  theme_minimal()

# Not a big surprise, the effect of X is completely hidden as we condition on Z
# in a pipe. The difference to the legs example here is that X and Z are
# causally related as X causes Z. It is therefore an example for a
# post-treatment bias. The leg lengths, on the other side, where not causing
# each other, but were caused by a common parent instead.

# Learning to analyze DAGs requires practice. For each of the four DAGs below,
# state which variables, if any, you must adjust for (condition on) to estimate
# the total causal influence of X on Y.
tribble(
  ~ name,  ~ x,  ~ y,  
  "A",    2,     1,     
  "Z",    1,     1,     
  "X",    0,     0,     
  "Y",    2,     0
) %>%  
  dagify(
    X ~ Z,
    Z ~ A, 
    Y ~ X + Z + A,
    coords = .) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_node(internal_colour = mint, alpha = 0.8, colour = "white") +
  geom_dag_text(aes(label = name), color = purple, size = 5) +
  geom_dag_edges(edge_color = purple) +
  labs(caption = "Figure 3: Directed acyclic graph number one.") +
  theme_void()

# The are two backdoor paths, X <- Z -> Y and X <- Z <- A -> Y. 
# Both are open and go through Z, so we can simply condition on Z.

tribble(
  ~ name,  ~ x,  ~ y,  
  "A",    2,     1,     
  "Z",    1,     1,     
  "X",    0,     0,     
  "Y",    2,     0
) %>%  
  dagify(
    Z ~ A + X, 
    Y ~ X + Z + A,
    coords = .) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_node(internal_colour = coral, alpha = 0.8, colour = "white") +
  geom_dag_text(aes(label = name), color = purple, size = 5) +
  geom_dag_edges(edge_color = purple) +
  labs(caption = "Figure 4: Directed acyclic graph number two") +
  theme_void()

# There are two backdoor paths, X -> Z -> Y and X -> Z <- A -> Y. We want to
# keep X -> Z -> Y as it encapsulates information flowing from X to Y (and we
# want to capture the total causal influence). The second path is closed as Z is
# a collider here. We don't need to adjust for anything.

tribble(
  ~ name,  ~ x,  ~ y,  
  "A",    0,     1,     
  "Z",    1,     1,     
  "X",    0,     0,     
  "Y",    2,     0
) %>%  
  dagify(
    X ~ A,
    Z ~ A + X + Y, 
    Y ~ X,
    coords = .) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_node(internal_colour = yellow, alpha = 0.8, colour = "white") +
  geom_dag_text(aes(label = name), color = purple, size = 5) +
  geom_dag_edges(edge_color = purple) +
  labs(caption = "Figure 5: Directed acyclic graph number three") +
  theme_void()

# There are two backdoor paths: X <- A -> Z <- Y and X -> Z <- Y. Both paths are
# closed as Z is a collider for both. We don't need to condition on anything. 

tribble(
  ~ name,  ~ x,  ~ y,  
  "A",    0,     1,     
  "Z",    1,     1,     
  "X",    0,     0,     
  "Y",    2,     0
) %>%  
  dagify(
    X ~ A,
    Z ~ A + X, 
    Y ~ X + Z,
    coords = .) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_node(colour = purple) +
  geom_dag_text(aes(label = name), color = "white", size = 5) +
  geom_dag_edges(edge_color = coral) +
  labs(caption = "Figure 5: Directed acyclic graph number four") +
  theme_void()

# There are two backdoor paths: X <- A -> Z -> Y and X -> Z -> Y. We want to
# keep the second one but close the first one. For this, we can condition on A.
# Conditioning on Z would close the first path as well, but also the second one
# which we want to keep as true causal.


### 6H1 ###
# Use the Waffle House data, data(WaffleDivorce), to find the total causal
# influence of number of Waffle Houses on divorce rate. Justify your model or
# models with a causal graph.

data("WaffleDivorce")

dat_waffle <- WaffleDivorce %>% 
  as_tibble() %>% 
  mutate(across(where(is.numeric), standardize)) %>% 
  select(divorce = Divorce, age = MedianAgeMarriage,
         m_rate = Marriage, waffle = WaffleHouses,
         south = South)

# divorce <- age -> m_rate
# waffle <- south -> age

tribble(
  ~ name,    ~ x,  ~ y,  
  "south",     0,    2,     
  "age",       1,    2,     
  "m_rate",    2,    1,     
  "waffle",    0,    0, 
  "divorce",   1,    0
) %>%  
  dagify(
    divorce ~ age + waffle,
    waffle ~ south, 
    age ~ south,
    m_rate ~ age,
    coords = .) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_node(colour = purple, shape = 1, size = 20) +
  geom_dag_text(aes(label = name), color = "black", size = 4) +
  geom_dag_edges(edge_color = coral) +
  labs(caption = "Figure 6: The total causal influence of number of Waffle Houses on divorce rate") +
  theme_void()

m_waffle <- alist(
  divorce ~ dnorm(mu, sigma),
  mu <- a + Bwaffle * waffle + Bsouth * south,
  a ~ dnorm(0, 0.2),
  c(Bwaffle, Bsouth) ~ dnorm(0, 0.5),
  sigma ~ dexp(1)) %>% 
  quap(data = dat_waffle) 


s <- seq(from = -2, to = 2, length.out = 30)
N <- 1e3

m_waffle %>% 
  link(data = list(waffle = s, 
                   south = 0)) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), values_to = "pred_divorce") %>% 
  add_column(waffle = rep(s, N)) %>% 
  group_by(waffle) %>% 
  nest() %>% 
  mutate(pred_divorce = map(data, "pred_divorce"), 
         mean_divorce = map_dbl(pred_divorce, mean), 
         pi = map(pred_divorce, PI), 
         lower_pi = map_dbl(pi, pluck(1)), 
         upper_pi = map_dbl(pi, pluck(2))) %>% 
  select(waffle, mean_divorce, lower_pi, upper_pi) %>% 
  ggplot() +
  geom_ribbon(aes(waffle, ymin = lower_pi, ymax = upper_pi), 
              fill = mint, alpha = 0.3) +
  geom_line(aes(waffle, mean_divorce), 
            size = 1.5, colour = coral) +
  coord_cartesian(ylim = c(-2, 2)) +
  labs(x = "Waffle (std)", y = "Divorce (std)", 
       caption = "Figure 7: Total causal effect of number of Waffle House on Divorce, 
       while keeping 'South' constant.") +
  theme_minimal()

### 6H2 ###
# Build a series of models to test the implied conditional independencies of the
# causal graph you used in the previous problem. If any of the tests fail, how
# do you think the graph needs to be amended? Does the graph need more or fewer
# arrows? Feel free to nominate variables that are not in the data.

dagitty("dag{
        waffle -> divorce <- age <- south -> waffle
        age -> m_rate}") %>% 
  impliedConditionalIndependencies()


# age _||_ waffle | south
alist(
  waffle ~ dnorm(mu, sigma),
  mu <- a + Bsouth*south + Bage*age,
  a ~ dnorm(0, 0.2),
  c(Bsouth, Bage) ~ dnorm(0, 0.5),
  sigma ~ dexp(1)) %>% 
  quap(data = dat_waffle) %>% 
  precis() %>% 
  as_tibble(rownames = "estimate") %>% 
  filter(estimate == "Bage") %>% 
  mutate(across(where(is.numeric), round, digits = 2)) %>% 
  knitr::kable()

test_independence <- function(model.input, coeff.input){
  suppressWarnings(
    precis(model.input) %>%
    as_tibble(rownames = "estimate") %>%
    filter(estimate == {{coeff.input}}) %>%
    mutate(across(where(is.numeric), round, digits = 2)) %>%
    knitr::kable()
  )
}

# divorce _||_ m_rate | age
alist(
  m_rate ~ dnorm(mu, sigma),
  mu <- a + Bdivorce*divorce + Bage*age,
  a ~ dnorm(0, 0.2),
  c(Bdivorce, Bage) ~ dnorm(0, 0.5),
  sigma ~ dexp(1)) %>% 
  quap(data = dat_waffle) %>% 
  test_independence(coeff.input = "Bdivorce")
  

# divorce _||_ south | age, waffle
alist(
  south ~ dnorm(mu, sigma),
  mu <- a + Bdivorce*divorce + Bage*age + Bwaffle*waffle,
  a ~ dnorm(0, 0.2),
  c(Bdivorce, Bage, Bwaffle) ~ dnorm(0, 0.5),
  sigma ~ dexp(1)) %>% 
  quap(data = dat_waffle) %>% 
  test_independence(coeff.input = "Bdivorce")

# m_rate _||_ south | age
alist(
  south ~ dnorm(mu, sigma),
  mu <- a + Bm_rate*m_rate + Bage*age,
  a ~ dnorm(0, 0.2),
  c(Bm_rate, Bage) ~ dnorm(0, 0.5),
  sigma ~ dexp(1)) %>% 
  quap(data = dat_waffle) %>% 
  test_independence(coeff.input = "Bm_rate")

# m_rate _||_ waffle | south
alist(
  waffle ~ dnorm(mu, sigma),
  mu <- a + Bm_rate*m_rate + Bsouth*south,
  a ~ dnorm(0, 0.2),
  c(Bm_rate, Bsouth) ~ dnorm(0, 0.5),
  sigma ~ dexp(1)) %>% 
  quap(data = dat_waffle) %>% 
  test_independence(coeff.input = "Bm_rate")

# m_rate _||_ waffle | age
alist(
  waffle ~ dnorm(mu, sigma),
  mu <- a + Bm_rate*m_rate + Bage*age,
  a ~ dnorm(0, 0.2),
  c(Bm_rate, Bage) ~ dnorm(0, 0.5),
  sigma ~ dexp(1)) %>% 
  quap(data = dat_waffle) %>% 
  test_independence(coeff.input = "Bm_rate")

