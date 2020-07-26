---
title: "Rethinking Chapter 3"
author: "Gregor Mathes"
date: "2020-26-07"
output:
  html_document: 
    toc: true
    toc_float: true
    number_sections: true  
    theme: journal
    keep_md: true
---



# Introduction 

This is the second part of a series where I work through the practice questions of the second edition of Richard McElreaths [Statistical Rethinking](https://xcelab.net/rm/statistical-rethinking/). Each post covers a new chapter. There are already some awesome sources for this book online like [Jeffrey Girard](https://jmgirard.com/statistical-rethinking-ch2/) working through the exercises of the first edition, or [Solomon Kurz](https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/) leading through each example of the book with the *brms* and the *tidyverse* packages. You can even watch the [lectures of McElreath](https://www.youtube.com/playlist?list=PLDcUM9US4XdNM4Edgs7weiyIguLSToZRI) on Youtube and work through the [homework and solutions](https://github.com/rmcelreath/statrethinking_winter2019/tree/master/homework).
However, so far I couldn't find a source providing solutions for the practice questions of the second edition, or the homework practices, in a tidy(-verse) way. My aim here is therefore to provide solutions for each homework and practice question of the second edition, using the *tidyverse* and the *rethinking* packages. The second part of the series will cover chapter 3, which corresponds to week 1 of the lectures and homework. 

# Homework

All homework questions for week 1 are already solved in my previous post *Rethinking Chapter 2*.

# Easy practices

The easy problems use the samples from the posterior distribution for the globe tossing example. This code will give you a specific set of samples, so that you can check your answers exactly: 


```r
# set the seed for reproducibility
set.seed(100)
samples <- tibble(# define grid
  p.grid = seq(from = 0, to = 1, length.out = 1000
  ),
  # define prior
  prior = rep(1, 1000)) %>%
  # compute likelihood at each value in grid for
  mutate(
    likelihood = dbinom(6, size = 9, prob = p.grid),
    # compute product of likelihood and prior
    unstd.likelihood = likelihood * prior,
    # standardise the posterior so it sums to 1
    posterior = unstd.likelihood / sum(unstd.likelihood)
  ) %>%
  select(posterior, p.grid) %>%
  # draw samples
  sample_n(size = 1000, weight = posterior, replace = TRUE) %>% 
  select(values = p.grid)
```

## Question 3E1

**How much posterior probability lies below p = 0.2?**

First we need to select all samples below p = 0.2, and then sum these up and finally divide by the total number of values. 


```r
samples %>% 
  filter(values < 0.2) %>%
  summarise(p = n()/1000) %>% 
  mutate(p_perc = p * 100)
```

```
## # A tibble: 1 x 2
##       p p_perc
##   <dbl>  <dbl>
## 1 0.001    0.1
```

So obviously not much. To be precise, 0.1% of the posterior probability lies below p = 0.2, using our seed.

## Question 3E2

**How much posterior probability lies above p = 0.8?**

Same procedure:


```r
samples %>% 
  filter(values > 0.8) %>%
  summarise(p = n()/1000) %>% 
  mutate(p_perc = p * 100)
```

```
## # A tibble: 1 x 2
##       p p_perc
##   <dbl>  <dbl>
## 1 0.091    9.1
```

A bit more, 9.1 %

## Question 3E3

**How much posterior probability lies between p = 0.2 and p = 0.8?**

Modify the `select()` function. 


```r
samples %>% 
  filter(values > 0.2 & values < 0.8) %>%
  summarise(p = n()/1000) %>% 
  mutate(p_perc = p * 100)
```

```
## # A tibble: 1 x 2
##       p p_perc
##   <dbl>  <dbl>
## 1 0.908   90.8
```

Well, the rest lies between. 