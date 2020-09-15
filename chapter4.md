---
title: "Rethinking Chapter 4"
author: "Gregor Mathes"
date: "2020-01-08"
output:
  html_document: 
    toc: true
    toc_float: true
    number_sections: true  
    theme: journal
    keep_md: true
---



# Introduction 

This is the third part of a series where I work through the practice questions of the second edition of Richard McElreaths [Statistical Rethinking](https://xcelab.net/rm/statistical-rethinking/). Each post covers a new chapter. There are already some awesome sources for this book online like [Jeffrey Girard](https://jmgirard.com/statistical-rethinking-ch2/) working through the exercises of the first edition, or [Solomon Kurz](https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/) leading through each example of the book with the *brms* and the *tidyverse* packages. You can even watch the [lectures of McElreath](https://www.youtube.com/playlist?list=PLDcUM9US4XdNM4Edgs7weiyIguLSToZRI) on Youtube and work through the [homework and solutions](https://github.com/rmcelreath/statrethinking_winter2019/tree/master/homework).
However, so far I couldn't find a source providing solutions for the practice questions of the second edition, or the homework practices, in a tidy(-verse) way. My aim here is therefore to provide solutions for each homework and practice question of the second edition, using the *tidyverse* and the *rethinking* packages. The third part of the series will cover chapter 4, which corresponds to week 2 of the lectures and homework. 

# Homework

## Question 1

**The weights listed below were recorded in the !Kung census, but heights were not recorded for these individuals. Provide predicted heights and 89% intervals for each of these individuals. That is, fill in the table below, using model-based predictions.**


| Individual | weight | expected height| 89% Interval|
|:----------:|:------:|---------------:|------------:|
|     1      | 46.95  |                |             |
|     2      | 43.72  |                |             |
|     3      | 64.78  |                |             |
|     4      | 32.59  |                |             |
|     5      | 54.63  |                |             |

We can use a linear regression model to predict height from weight. First, let's load the census data and the new weights:


```r
# data
data("Howell1")
d <- Howell1

# new data to predict from 
new_weight <- c(46.95, 43.72, 64.78, 32.59, 54.63)
```

For the model, we use the same structur and priors as given on page 102. Further, we use all data (both juveniles and adults) and use quadratic approximation for the posterior. 


```r
# model formula
m <- alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b * (weight - mean(weight)),
  a ~ dnorm(178, 20),
  b ~ dlnorm(0, 1),
  sigma ~ dunif(0, 50)) %>% 
  # model fit using quadratic approximation
  quap(data = d)
```

Now we can calculate the posterior distribution of heights for each individual weight value in the table using the `link()` function, as explained on page 105. From these posterior distributions, we can calculate the mean and the 89% percentile interval using `summarise_all()`. 


```r
# predict height 
pred_height <- link(m, data = data.frame(weight = new_weight))

# calculate means
expected <- pred_height %>% 
  as_tibble() %>% 
  summarise_all(mean) %>% 
  as_vector()

# calculate percentile interval
interval <- pred_height %>% 
  as_tibble() %>% 
  summarise_all(HPDI, prob = 0.89) %>% 
  as_vector()
```

Now we just have to add the predicted values to the table:


```r
table1 <- tibble(individual = 1:5, weight = new_weight, expected = expected, 
       lower = interval[c(TRUE, FALSE)], upper = interval[c(FALSE, TRUE)]) %>% 
  knitr::kable(align = "cccrr")

table1
```



| individual | weight | expected |    lower|    upper|
|:----------:|:------:|:--------:|--------:|--------:|
|     1      | 46.95  | 135.4874 | 134.8411| 136.0906|
|     2      | 43.72  | 129.7934 | 129.1717| 130.4746|
|     3      | 64.78  | 166.9194 | 165.9695| 167.8875|
|     4      | 32.59  | 110.1726 | 109.2341| 111.0758|
|     5      | 54.63  | 149.0263 | 148.3964| 149.7410|

## Question 2

**Model the relationship between height (cm) and the natural logarithm of weight (log-kg). Use the entire `Howell1` data frame, all 544 rows, adults and non-adults. Fit this model, using quadratic approximation. Use any model type from chapter 4 that you think useful: an ordinary linear regression, a polynomial or a spline. Plot the posterior predictions against the raw data.**

First, let's take a look at the data:


```r
d %>% 
  mutate(log.weight = log(weight)) %>% 
  ggplot() +
  geom_point(aes(log.weight, height), alpha = 0.5) +
  theme_minimal()
```

![](chapter4_files/figure-html/question 2 part 1-1.png)<!-- -->

It actually looks like a decent linear relationship, so a simple linear regression should be sufficient. All we need to change from the previous model is to log-transform the weight.


```r
m_log <- alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b * log(weight),
  a ~ dnorm(178, 20),
  b ~ dlnorm(0, 1),
  sigma ~ dunif(0, 50)) %>% 
  quap(data = d)
```

Let's glimpse at the results:

```r
precis(m_log) %>% as_tibble() %>% 
  add_column(parameter = rownames(precis(m_log))) %>% 
  rename("lower" = '5.5%', "upper" = '94.5%') %>% 
  select(parameter, everything()) %>% 
  knitr::kable(align = "lcrrr")
```



|parameter |    mean    |        sd|      lower|      upper|
|:---------|:----------:|---------:|----------:|----------:|
|a         | -22.872951 | 1.3342982| -25.005417| -20.740485|
|b         | 46.817403  | 0.3823260|  46.206372|  47.428433|
|sigma     |  5.137101  | 0.1558858|   4.887965|   5.386236|

Instead of trying to read these estimates, we can just visualise our model. Let's calculate the predicted mean height as a function of weight, the 97% PI for the mean, and the 97% PI for predicted heights as explained on page 108.  
  
As we will repeat these steps throughout the exercises, we can set up a function for the interval calculation:


```r
# we better make a function out of it since we use it more often
tidy_intervals <- function(my_function, my_model, interval_type, 
                           x_var, x_seq){
  # preprocess dataframe
  df <- data.frame(col1 = x_seq)
  colnames(df) <- x_var
  
  # calculate 89% intervals for each weight
  # either link or sim
  my_function(my_model, data = df) %>% 
  as_tibble() %>% 
  #  either PI or HPDI
  summarise_all(interval_type, prob = 0.89) %>% 
  add_column(type = c("lower", "upper")) %>% 
  pivot_longer(cols = -type, names_to = "cols", values_to = "intervals") %>% 
  add_column(x_var = rep(x_seq, 2)) %>% 
  pivot_wider(names_from = type, values_from = intervals)
}
```
  
And for the mean:  
  

```r
tidy_mean <- function(my_model, data){
  link(my_model, data) %>%
    as_tibble() %>%
    summarise_all(mean) %>%
    pivot_longer(cols = everything()) %>% 
    add_column(x_var = data[[1]])
}
```

Now let's use these functions to calculate the intervals:


```r
# define weight range
weight_seq <- seq(from = min(d$weight), to = max(d$weight), by = 1)

# calculate 89% intervals for each weight
intervals <- tidy_intervals(link, m_log, HPDI, 
                            x_var = "weight", x_seq = weight_seq)

# calculate mean
reg_line <- tidy_mean(m_log, data = data.frame(weight = weight_seq))


# calculate prediction intervals
pred_intervals <- tidy_intervals(sim, m_log, PI, 
                                 x_var = "weight", x_seq = weight_seq)
```

Now we can plot the raw data, the posterior mean from, the distribution of mu (which corresponds to the 89% HPDI of the mean), and the region within which the model expects to find 89% of actual heights in the population.


```r
ggplot(d) +
  geom_point(aes(weight, height), alpha = 0.5) +
  geom_ribbon(aes(x = x_var, ymin = lower, ymax = upper),
                alpha=0.8, data = intervals) +
  geom_ribbon(aes(x = x_var, ymin = lower, ymax = upper),
                alpha=0.2, data = pred_intervals) +
  geom_line(aes(x = x_var, y = value), data = reg_line, colour = "coral") +
  theme_light()
```

![](chapter4_files/figure-html/question 2 part 7-1.png)<!-- -->
  
This looks like a decent fit. Let's make a function of the ggplot, so we can call it later on:


```r
plot_regression <- function(df, x, y, # results, 
                            interv = intervals, pred_interv = pred_intervals){
  ggplot(df) +
    geom_point(aes({{x}}, {{y}}), alpha = 0.5) +
    geom_ribbon(aes(x = x_var, ymin = lower, ymax = upper),
                alpha=0.8, data = interv) +
    geom_ribbon(aes(x = x_var, ymin = lower, ymax = upper),
                alpha=0.2, data = pred_interv) +
    geom_line(aes(x = x_var, y = value), data = reg_line, colour = "coral") +
    theme_light()
}
```

## Question 3  
  
**Plot the prior predictive distribution for the polynomial regression model in Chapter 4. You can modify the prior predictive distribution. 20 or 30 parabolas from the prior should suffice to show where the prior probability resides. Can you modify the prior distributions of a, b1, and b2 so that the prior predictions stay within the biologically reasonable outcome space? That is to say: Do not try to fit the data by hand. But do try to keep the curves consistent with what you know about height and weight, before seeing these exact data.**  
  
Let us first recap how the polynomial regression model in Chapter 4 was built: The first thing was to standardise the predictor variables, in our case the `weight` variable. Additionally, we calculate the square of the standardised weight. 


```r
# standardise weight
d_stand <- d %>% mutate(weight_s = (weight - mean(weight)) / sd(weight), 
              weight_s2 = weight_s^2) %>% 
  as_tibble()
```

And here's the notation of the model with the priors used in the chapter:


```r
# fit model
m_poly <- alist(height ~ dnorm(mu, sigma), 
               mu <- a + b1 * weight_s + b2 * weight_s2,
               a ~ dnorm(178, 20), 
               b1 ~ dlnorm(0, 1), 
               b2 ~ dnorm(0, 1), 
               sigma ~ dunif(0, 50)) %>% 
  quap(data = d_stand)
```

For this quap, we can sample from the prior distribution using the `extract.prior()` function. We then pass these samples from the prior to the link function for the weight space we are interested in. After transforming the weight back to natural scale, we can take a look at the prior probabilities. 


```r
# set seed
set.seed(123)

# get 30 samples for each parameter (a, b1, b2, sigma) 
m_poly_prior <- extract.prior(m_poly, n = 30)

# now pass the samples from the prior distribution to the link function 
m_poly_mu <- link(m_poly, post = m_poly_prior, 
     data = list(weight_s = seq(from = min(d_stand$weight_s),
                                to = max(d_stand$weight_s), 
                                length.out = 50), 
                 weight_s2 = seq(from = min(d_stand$weight_s2),
                                 to = max(d_stand$weight_s2), 
                                 length.out = 50))) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), names_to = "name", values_to = "height") %>% 
  add_column(weight_s = rep(seq(from = min(d_stand$weight_s),
                              to = max(d_stand$weight_s), 
                              length.out = 50), 30)) %>% 
  mutate(weight = weight_s * sd(d_stand$weight) + mean(d_stand$weight), 
         name = rep(1:30, each = 50), 
         name = as.factor(name))
```



```r
# plot it
ggplot(m_poly_mu, aes(x = weight_s, y = height)) +
  geom_hline(yintercept = c(0, 272), colour = "coral") +
  geom_line(aes(group = name)) +
  theme_light()
```

![](chapter4_files/figure-html/question 3 part 4-1.png)<!-- -->

Unfortunately, I get straight lines and no parabolas. I couldn't figure out why this is. To compare my results with those from McElreath himself, follow [this link](https://github.com/rmcelreath/statrethinking_winter2019/blob/master/homework/week02_solutions.pdf).  
  
  
# Easy practices  
  
All the easy questions don't require new R code and are already covered by [Jeffrey Girard](https://jmgirard.com/statistical-rethinking-ch4/). 

# Medium practices 
  
## Question 4M1
  
**For the model definition below, simulate observed y values from the prior (not the posterior).**  
  
$y_{i} ∼ Normal(μ,σ)$  
$μ ∼ Normal(0,10)$  
$σ ∼ Exponential(1)$  
  
We can simply sample from each prior distribution:  


```r
# sample mu
sample_mu <- rnorm(1e4, 0, 10)

# sample sigma
sample_sigma <- rexp(1e4, 1)

# sample y
prior_y <- rnorm(1e4, sample_mu, sample_sigma) %>% 
  enframe()

# plot
ggplot(prior_y) +
  geom_density(aes(value), size = 1) + 
  theme_light()
```

![](chapter4_files/figure-html/question 4M1-1.png)<!-- -->


## Question 4M2  
  
**Translate the model just above into a `map()` formula**
  

```r
map_frm <- alist(y ~ dnorm(mu, sigma), 
                 mu ~ dnorm(0, 10), 
                 sigma ~ dexp(1)) 
```

## Question 4M3  

**Translate the `map()` model formula below into a mathematical model definition.**  
  

```r
flist <- alist(
  y ~ dnorm(mu, sigma),
  mu <- a + b*x,
  a ~ dnorm(0, 50),
  b ~ dunif(0, 10),
  sigma ~ dunif(0, 50))
```
  
$y_{1} ∼ Normal(μ,σ)$  
$μ_{1} ∼ α + βx_{1}$  
$α ∼ Normal(0, 50)$   
$β = Uniform(0, 10)$  
$σ ∼ Uniform(0, 50)$
  
  
## Question 4M4  
  
**A sample of students is measured for height each year for 3 years. After the third year, you want to fit a linear regression predicting height using year as a predictor. Write down the mathematical model definition for this regression, using any variable names and priors you choose. Be prepared to defend you choice of priors.**  
  
We can use a simple linear regression approach for this.  
First the likelihood, assuming that height is normally distributed:  
$h_{1} ∼ Normal(μ,σ)$  
  
The linear model:  
$μ_{1} ∼ α + βx_{1}$  
  
The prior for the intercept. It says nothing about the age of the students, so I use a weak prior for the intercept, covering elementary school to adults as shown in the plot below:  
$α ∼ Normal(150, 20)$  
  
  

```r
tibble(height = rnorm(10000, 150, 20)) %>% 
  ggplot() +
  geom_density(aes(height)) +
  theme_minimal()
```

![](chapter4_files/figure-html/question 4M4 part 1-1.png)<!-- -->
  
Likewise, the prior for the growth rate needs to adjust for fast growing juveniles and for smaller growing older students. We can't use a normal distribution because students don't shrink (to my knowledge). A uniform distribution forces only positive growth rates.  
$β = Uniform(0, 7)$  


```r
tibble(growth = runif(10000, 0, 7)) %>% 
  ggplot() +
  geom_density(aes(growth)) +
  labs(x = "Growth rate in cm/year") +
  theme_minimal()
```

![](chapter4_files/figure-html/question 4M4 part 2-1.png)<!-- -->
  
Now we just need to add a prior for sigma, the standard deviation of the height. I chosse a weak prior, which is within the reasonable height space (the first plot). A sigma above 40 would lead to height values outside of this range (second plot):  
$σ ∼ Uniform(0, 30)$
  

```r
tibble(height = rnorm(10000, 150, 30)) %>% 
  ggplot() +
  geom_density(aes(height)) +
  labs(x = "Height in cm", title = "sigma = 30") +
  theme_minimal()

tibble(height = rnorm(10000, 150, 40)) %>% 
  ggplot() +
  geom_density(aes(height)) +
  labs(x = "Height in cm", title = "sigma = 40") +
  theme_minimal()
```

<img src="chapter4_files/figure-html/question 4M4 part 3-1.png" width="50%" /><img src="chapter4_files/figure-html/question 4M4 part 3-2.png" width="50%" />
  
## Question 4M5  
  
**Now suppose I tell you that the average height in the first year was 120 cm and that every student got taller each year. Does this information lead you to change your choice of priors? How?**

An average height of 120cm tells us that the students are children. We can keep our likelihood and linear model, but need to adjust the prior for alpha, beta, and sigma slightly. For alpha, we can use the new mean of 120. For beta (the growth rate), we can use a log normal distribution to force positive values and make them slightly bigger as before, as children tend to grow faster. For sigma, we can reduce it slightly as the spread is probably lower within children. 
  
$h_{1} ∼ Normal(μ,σ)$  
  
$μ_{1} ∼ α + βx_{1}$  
  
$α ∼ Normal(120, 20)$  

$β = LogNormal(2, 0.5)$  
  
$σ ∼ Uniform(0, 20)$  
  
## Question 4M6  
  
**Now suppose I tell you that the variance among heights for students of the same age is never more than 64 cm. How does this lead you to revise your priors?**

The variance is the square of σ. If the variance is never more than 64 cm, then sigma is never higher than 8 cm. We can update our prior:  
  
$σ ∼ Uniform(0, 8)$  
  
Such a low standard deviation of height has implication for our intercept prior alpha. We are now more certain that the intercept is around 120cm and can narrow the spread a bit:  
  
$α ∼ Normal(120, 10)$  
  
## Question 4M7  
  
**Refit model `m4.3` from the chapter but omit the mean weight `xbar`. Compare the new model’s posterior to that of the original model. In particular, look at the covariance among the parameters. What is difference?**
  
First, let us take a look at the old model `m4.3`.  


```r
# only adults
adults <- Howell1 %>% filter(age >= 18)

# mean weight
xbar <- adults %>% 
  summarise(my_mean = mean(weight)) %>% 
  pull()

# fit model
m4.3 <- alist(height ~ dnorm(mu, sigma), # likelihood
                 mu <- a + b * (weight - xbar), # linear model
                 a ~ dnorm(178, 20), # alpha
                 b ~ dlnorm(0, 1), # beta
                 sigma ~ dunif(0, 50)) %>% # sigma
  quap(., data = adults) # quadratic approximation
```
  
Now do the same but without `xbar`.
  

```r
m4.3new <- alist(height ~ dnorm(mu, sigma), # likelihood
                 mu <- a + b*weight, # linear model
                 a ~ dnorm(178, 20), # alpha
                 b ~ dlnorm(0, 1), # beta
                 sigma ~ dunif(0, 50)) %>% # sigma
  quap(., data = adults) # quadratic approximation
```
  
We shall look at the covariance of each model.  
  

```r
m4.3new %>% vcov() %>% round(digits = 3) # lots of covariation
```

```
##            a      b sigma
## a      3.612 -0.079 0.010
## b     -0.079  0.002 0.000
## sigma  0.010  0.000 0.037
```

```r
m4.3 %>% vcov() %>% round(digits = 3) # low covariation 
```

```
##           a     b sigma
## a     0.073 0.000 0.000
## b     0.000 0.002 0.000
## sigma 0.000 0.000 0.037
```

So we seem to increase the covariation by not centering. Let's dig deeper by looking at summaries of the posterior distribution for each parameter:  
  

```r
m4.3new %>% extract.samples() %>% as_tibble() %>% 
  summarise(alpha = mean(a), beta = mean(b), sigma = mean(sigma))
```

```
## # A tibble: 1 x 3
##   alpha  beta sigma
##   <dbl> <dbl> <dbl>
## 1  115. 0.890  5.08
```

```r
m4.3 %>% extract.samples() %>% as_tibble() %>% 
  summarise(alpha = mean(a), beta = mean(b), sigma = mean(sigma))
```

```
## # A tibble: 1 x 3
##   alpha  beta sigma
##   <dbl> <dbl> <dbl>
## 1  155. 0.904  5.07
```

While beta and sigma are the same, alpha is drastically lower in the new model without centering. But what does this mean? Before alpha was the average height for when $x − x$ was 0 (when the weight is equal to the average weight). In the new model, alpha is the average height for observation with weight equal 0. As there are no people with a weight of zero, this alpha is harder to interpret.  
But does this actually change the way our model works? We can test this by comparing the posterior predictions by appling our prediction functions to the data:  
  



```r
m4.3.plot
m4.3new.plot
```

<img src="chapter4_files/figure-html/question 4M7 part 6-1.png" width="50%" /><img src="chapter4_files/figure-html/question 4M7 part 6-2.png" width="50%" />
  
Short answer: No, it does not. We get the same prediction intervals.  
  
## Question 4M8  
  
**In the chapter, we used 15 knots with the cherry blossom spline. Increase the number of knots and observe what happens to the resulting spline. Then adjust also the width of the prior on the weights - change the standard deviation of the prior and watch what happens. What do you think the combination of knot number and the prior on the weights controls?**  
  
First of all, set up the model environment by importing the `cherry_blossoms` data and loading the `splines` package:  
  

```r
data("cherry_blossoms")

# remove na's
cherry_blossoms <- cherry_blossoms %>%
  as_tibble() %>%  
  drop_na()

# define nr of knots
library(splines)
```
  
Instead of typing out the code all the time whenever we change the number of knots or the prior on the weights as needed, we make a function dependant on these two parameters and just call the function each time. All we change within the function is the number of knots and the prior on weights, everything else stays as it is. I already process and plot the output of the splines regression in the function.  
  

```r
# make function for it, dependant on number of knots and the prior on weights
cherry_spliner <- function(nr_knots, vl_sigma){

# get knot points
knot_list <- cherry_blossoms$year %>% 
  quantile(probs = seq(0, 1, length.out = nr_knots)) %>% 
  discard(~ . %in% c(851, 1980))

# construct basis functions
B <- cherry_blossoms$year %>% bs(knots = knot_list, degree = 3, intercept = TRUE) 

# run bspline regression
m4.7 <- alist(D ~ dnorm(mu, sigma), 
              mu <- a + B %*% w, 
              a ~ dnorm(100, 10), 
              w ~ dnorm(0, my_sigma), 
              sigma ~ dexp(1)) %>% 
  quap(., data = list(D = cherry_blossoms$doy, B = B, my_sigma = vl_sigma), 
                      start = list(w = rep(0, ncol(B))))

# get 97% posterior interval for mean
post_int <- m4.7 %>% 
  link() %>% as_tibble() %>% 
  map_dfr(PI, prob = 0.97) %>% 
  select("lower_pi" = '2%', "upper_pi" = '98%') %>% 
  add_column(year = cherry_blossoms$year) %>% 
  # add doy
  left_join(cherry_blossoms, by = "year")

# plot it and add nr_knots and vl_sigma to plot title
ggplot(post_int, aes(year, doy)) +
  geom_point(colour = "steelblue4", alpha = 0.8) +
  geom_ribbon(aes(ymin = lower_pi, ymax = upper_pi), alpha = 0.9) +
  labs(y = "Day in year", x = "year", 
       title = paste(nr_knots, " knots, prior on weights ~ N(0,", vl_sigma, ")")) +
  theme_minimal()
}
```
  
Now we can increase the number of knots. We start with the normal model with 15 knots and sigma = 10.  
  

```r
cherry_spliner(nr_knots = 15, vl_sigma = 10) 

cherry_spliner(nr_knots = 20, vl_sigma = 10)

cherry_spliner(nr_knots = 30, vl_sigma = 10)
```

<img src="chapter4_files/figure-html/question 4M8 part 3-1.png" width="50%" /><img src="chapter4_files/figure-html/question 4M8 part 3-2.png" width="50%" /><img src="chapter4_files/figure-html/question 4M8 part 3-3.png" width="50%" />

The more knots we have, the *wigglier* our trend line gets, as we capture more signal.  
  
  
Now we can play around with the prior on weights. First, we decrease it significantly to 1, and then increase it to 100, while keeping the number of knots equal. 
  

```r
cherry_spliner(nr_knots = 15, vl_sigma = 1) 

cherry_spliner(nr_knots = 15, vl_sigma = 10)

cherry_spliner(nr_knots = 15, vl_sigma = 100)
```

<img src="chapter4_files/figure-html/question 4M8 part 4-1.png" width="50%" /><img src="chapter4_files/figure-html/question 4M8 part 4-2.png" width="50%" /><img src="chapter4_files/figure-html/question 4M8 part 4-3.png" width="50%" />

So by increasing the prior or weights, we render our trend line more *wigglier*  as well. This makes sense as the weights will be closer to 0 and hence wiggle around closer to the mean line if the standard deviation is small. If the weights become larger, we allow the curve to have more peaks.  
  
  
# Hard practices  
  
## Question 4H1  
  
**The weights listed below were recorded in the !Kung census, but heights were not recorded for these individuals. Provide predicted heights and 89% intervals (either HPDI or PI) for each of these individuals. That is, fill in the table below, using model-based predictions.**  
  

| Individual | weight | expected height| 89% Interval|
|:----------:|:------:|---------------:|------------:|
|     1      | 46.95  |                |             |
|     2      | 43.72  |                |             |
|     3      | 64.78  |                |             |
|     4      | 32.59  |                |             |
|     5      | 54.63  |                |             |
  
This question is similar to the question 1 in the homework. We solved it there by using a linear regression, but this time we model the relationship between height (cm) and the natural logarithm of weight (log-kg). Let's see if that makes any difference at all.  
  

```r
# new data to predict from 
new_weight <- c(46.95, 43.72, 64.78, 32.59, 54.63)

# fit the logarithmic model
m_log <- alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b * log(weight),
  a ~ dnorm(178, 20),
  b ~ dlnorm(0, 1),
  sigma ~ dunif(0, 50)) %>% 
  quap(data = d)
```
  
Now we can make predictions from our model:  
  

```r
# predict height 
pred_height <- link(m_log, data = data.frame(weight = new_weight))

# calculate means
expected <- pred_height %>% 
  as_tibble() %>% 
  summarise_all(mean) %>% 
  as_vector()

# calculate percentile interval
interval <- pred_height %>% 
  as_tibble() %>% 
  summarise_all(HPDI, prob = 0.89) %>% 
  as_vector()
```
  
And add the predictions to the table.  
  

```r
tibble(individual = 1:5, weight = new_weight, expected = expected, 
       lower = interval[c(TRUE, FALSE)], upper = interval[c(FALSE, TRUE)]) %>% 
  knitr::kable(align = "cccrr")
```



| individual | weight | expected |    lower|    upper|
|:----------:|:------:|:--------:|--------:|--------:|
|     1      | 46.95  | 157.3245 | 156.8922| 157.7907|
|     2      | 43.72  | 153.9881 | 153.5386| 154.3864|
|     3      | 64.78  | 172.3929 | 171.8946| 173.0614|
|     4      | 32.59  | 140.2357 | 139.8477| 140.5806|
|     5      | 54.63  | 164.4161 | 163.9305| 164.9549|
  
And now we can compare our results to the predictions from the regular model, which we named `table1`. 


```r
table1
```



| individual | weight | expected |    lower|    upper|
|:----------:|:------:|:--------:|--------:|--------:|
|     1      | 46.95  | 135.4874 | 134.8411| 136.0906|
|     2      | 43.72  | 129.7934 | 129.1717| 130.4746|
|     3      | 64.78  | 166.9194 | 165.9695| 167.8875|
|     4      | 32.59  | 110.1726 | 109.2341| 111.0758|
|     5      | 54.63  | 149.0263 | 148.3964| 149.7410|
  
And we can see that it actually makes a big difference. Our logarithmic model generally predicts that people are larger compared to the old model. This is particularly visible for low weights.  
  
## Question 4H2



