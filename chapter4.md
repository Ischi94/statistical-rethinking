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
tibble(individual = 1:5, weight = new_weight, expected = expected, 
       lower = interval[c(TRUE, FALSE)], upper = interval[c(FALSE, TRUE)]) %>% 
  knitr::kable(align = "cccrr")
```



| individual | weight | expected |    lower|    upper|
|:----------:|:------:|:--------:|--------:|--------:|
|     1      | 46.95  | 135.4944 | 134.8623| 136.1539|
|     2      | 43.72  | 129.7956 | 129.1653| 130.5251|
|     3      | 64.78  | 166.9527 | 166.0004| 167.8746|
|     4      | 32.59  | 110.1584 | 109.1964| 111.1321|
|     5      | 54.63  | 149.0446 | 148.3473| 149.7302|

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
|a         | -22.874333 | 1.3342912| -25.006788| -20.741878|
|b         | 46.817794  | 0.3823241|  46.206767|  47.428822|
|sigma     |  5.137088  | 0.1558847|   4.887954|   5.386222|

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

Now let's use this function to calculate the intervals:


```r
# define weight range
weight_seq <- seq(from = min(d$weight), to = max(d$weight), by = 1)

# calculate 89% intervals for each weight
intervals <- tidy_intervals(link, m_log, HPDI, 
                            x_var = "weight", x_seq = weight_seq)

# calculate means
reg_line <- link(m_log, data = data.frame(weight = weight_seq)) %>%
  as_tibble() %>%
  summarise_all(mean) %>%
  pivot_longer(cols = everything()) %>% 
  add_column(weight = weight_seq)
    

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
    theme_light()
```

![](chapter4_files/figure-html/question 2 part 6-1.png)<!-- -->
  
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
    theme_light()
}
```




 
 
