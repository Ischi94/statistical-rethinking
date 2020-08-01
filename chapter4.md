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

For the model, we use the same structur and priors as given on page 102:


```r
# model formula
m <- alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b * (weight - mean(weight)),
  a ~ dnorm(178, 20),
  b ~ dlnorm(0, 1),
  sigma ~ dunif(0, 50)) %>% 
  # model fit
  map(data = d)
```

Now we can calculate the posterior distribution of heights for each individual weight value in the table using the `link()` function, as explained on page 105. From these posterior distributions, we can calculate the mean and the 89% percentile interval. 


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
|     1      | 46.95  | 135.4876 | 134.9288| 136.1790|
|     2      | 43.72  | 129.7929 | 129.1542| 130.4785|
|     3      | 64.78  | 166.9232 | 166.0108| 167.8885|
|     4      | 32.59  | 110.1698 | 109.1935| 111.0188|
|     5      | 54.63  | 149.0280 | 148.2945| 149.6619|

