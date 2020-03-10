In this assignment we learn how to assess rates from a binomial distribution, using the case of assessing your teachers’ knowledge of CogSci
--------------------------------------------------------------------------------------------------------------------------------------------

N.B. there is a second part at the bottom for next week.

### First part

You want to assess your teachers’ knowledge of cognitive science. “These
guys are a bunch of drama(turgist) queens, mindless philosophers,
chattering communication people and Russian spies. Do they really know
CogSci?”, you think.

To keep things simple (your teachers should not be faced with too
complicated things): - You created a pool of equally challenging
questions on CogSci - Each question can be answered correctly or not (we
don’t allow partially correct answers, to make our life simpler). -
Knowledge of CogSci can be measured on a scale from 0 (negative
knowledge, all answers wrong) through 0.5 (random chance) to 1 (awesome
CogSci superpowers)

This is the data: - Riccardo: 3 correct answers out of 6 questions -
Kristian: 2 correct answers out of 2 questions (then he gets bored) -
Josh: 160 correct answers out of 198 questions (Josh never gets bored) -
Mikkel: 66 correct answers out of 132 questions

Questions:

1.  What’s Riccardo’s estimated knowledge of CogSci? What is the
    probability he knows more than chance (0.5) \[try figuring this out.
    if you can’t peek into chapters 3.1 and 3.2 and/or the slides\]?

-   First implement a grid approximation (hint check paragraph 2.4.1!)
    with a uniform prior, calculate the posterior and plot the results

``` r
set.seed(8) #for reproducibility


n <- 101 #setting length of grid
n_success <- 3 #setting number of correct questions
n_trials  <- 6 #setting number of questions in total

#grid approximation for Riccardo
(
  Ric_tib <-
    tibble(p_grid     = seq(from = 0, to = 1, length.out = n), #setting p_grid
           # note we're using a flat uniform prior
           prior      = 1) %>%  #setting prior
    mutate(likelihood = dbinom(n_success, size = n_trials, prob = p_grid)) %>% #computing likelihood              using dbinom
    mutate(posterior  = (likelihood * prior) / sum(likelihood * prior)) #computing posterior using                Bayes
)
```

    ## # A tibble: 101 x 4
    ##    p_grid prior likelihood  posterior
    ##     <dbl> <dbl>      <dbl>      <dbl>
    ##  1   0        1  0         0         
    ##  2   0.01     1  0.0000194 0.00000136
    ##  3   0.02     1  0.000151  0.0000105 
    ##  4   0.03     1  0.000493  0.0000345 
    ##  5   0.04     1  0.00113   0.0000793 
    ##  6   0.05     1  0.00214   0.000150  
    ##  7   0.06     1  0.00359   0.000251  
    ##  8   0.07     1  0.00552   0.000386  
    ##  9   0.08     1  0.00797   0.000558  
    ## 10   0.09     1  0.0110    0.000769  
    ## # ... with 91 more rows

``` r
#Plotting the posterior distribution of Riccardo's knowledge along with the prior
Ric_tib %>% 
  ggplot(aes(x = p_grid, y = posterior)) +
  geom_point() +
  geom_line() +
  geom_line(aes (p_grid, prior/n, color = "Prior"))+
  labs(subtitle = "101 point grid approximation of Riccardo's cogsci knowledge",
       x = "cogsci knowledge",
       y = "posterior probability") +
  theme(panel.grid = element_blank())
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
#Computing the probability that Riccardo's knowledge is above chance
Ric_tib %>%  filter(p_grid >.5) %>% 
  summarise(sum = sum(posterior)) #summing the probability of each possible value of p (proportion of cogsci knowledge above 0.5)
```

    ## # A tibble: 1 x 1
    ##     sum
    ##   <dbl>
    ## 1 0.489

-   Then implement a quadratic approximation (hint check paragraph
    2.4.2!).
-   N.B. for the rest of the exercise just keep using the grid
    approximation (we’ll move to quadratic approximations in two
    classes)

``` r
set.seed(8)

#Quadratic approximation

globe_qa <-
  rethinking::map(
    alist(
      c ~ dbinom(6, p),  # binomial likelihood 6 = number of questions
      p ~ dunif(0, 1)    # uniform prior
    ), 
    data = list(c = 3)) #3 = number of correct questions

precis(globe_qa) #obtaining summary of quadratic approximation
```

    ##   Mean StdDev 5.5% 94.5%
    ## p  0.5    0.2 0.17  0.83

``` r
#Plotting grid approximation against quadratic approximation
n_grid <- 101 #defining length of grid

tibble(p_grid                  = seq(from = 0, to = 1, length.out = n_grid),
       prior                   = 1,
       c                       = 3, #correct questions
       n                       = 6, #questions in total
       m                       = 0.5, #mean from precis
       s                       = 0.2) %>% #SD form precis
  mutate(likelihood            = dbinom(c, size = n, prob = p_grid)) %>%
  mutate(unstd_grid_posterior  = likelihood * prior, #unstandardized posteriors from grid approximation
         unstd_quad_posterior  = dnorm(p_grid, m, s)) %>% #unstandardized posteriors from quadratic approximation
  group_by(c) %>% 
  mutate(grid_posterior        = unstd_grid_posterior / sum(unstd_grid_posterior), #standardizing posteriors
         quad_posterior        = unstd_quad_posterior / sum(unstd_quad_posterior)) %>% #standardizing posteriors

  #plotting
  ggplot(aes(x = p_grid)) +
  geom_line(aes(y = grid_posterior, colour = "Grid")) +
  geom_line(aes(y = quad_posterior, colour = "Quadratic")) +
  labs(x = "proportion ",
       y = "density") +
  ggtitle("Grid approximation vs quadratic approximation")+
  theme(panel.grid = element_blank())
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-2-1.png)

1.  Estimate all the teachers’ knowledge of CogSci. Who’s best? Use grid
    approximation. Comment on the posteriors of Riccardo and Mikkel.

``` r
#creating tibble of teacher data
teachers <- tibble(Teacher = c("Riccardo", "Kristian", "Josh", "Mikkel"),
                   Correct = c(3,2,160,66),
                   Questions = c(6,2,198,132))
```

2a. Produce plots of the prior, and posterior for each teacher.

``` r
set.seed(8) #for reproducibility

#the 2 following loops are used multiple times throughout the assignment and will only be commented once

n <- 1001 #setting length of grid

n_samples <- 1e4 #specifying number of samples used for sampling the posterior



#adimittedly quite limited function, takes a "teacher-tibble", grid length n, number of samples and a prior

grid_function <- function(dat, n, n_samples, prior){
grid_teacher <- tibble() #creating empty tibble for grid approximations of teachers
samples <- tibble() #creating empty tibble for samples

for (i in seq_along(dat$Teacher)){ #looping through sequence of teachers
    tib <- tibble( #saving grid in tibble
      p_grid     = seq(from = 0, to = 1, length.out = n), #setting p_grid
      prior      = prior) %>% #setting uniform prior
    mutate(likelihood = dbinom(dat$Correct[i], size = dat$Questions[i], prob = p_grid)) %>% #computing likelihood using dbinom 
    mutate(posterior  = (likelihood * prior) / sum(likelihood * prior), #computing standardized posterior using bayes
           teacher = dat$Teacher[i]) #saving which teacher the grid belongs to
    samples <- bind_rows(samples, #binding rows of previous samples tibble with samples for each teacher in the sequence 
                         tib %>% #piping current grid approximation into sample function 
  sample_n(size = n_samples, weight = posterior, replace = T)) #sampling grid n times based on posteriors in grid
    grid_teacher <- bind_rows(grid_teacher,tib) #binding rows of previous grid tibble with grid for each teacher in the sequence
}
return(list(samples = samples, grid_teacher = grid_teacher)) #function returns a list containing samples tibble and grid approximation tibble
}

res1 <- grid_function(teachers, 1001, 1e4, 1) #applying function with uniform prior

#specifying function for getting HPDI by giving a samples tibble
get_hdi <- function(samples){
hdi_df <- tibble()#creating empty tibble for highest posterior density intervals (HPDI)
for (i in seq_along(unique(samples$teacher))){ #looping through sequence of teachers
  hdi_df <- bind_rows(hdi_df, #binding rows of previous HPDI tibble with HPDI for each teacher in the sequence
                      tibble(l_95 = hdi(samples$p_grid[samples$teacher == unique(samples$teacher)[i]]
                                        ,.width = .95)[1], #saving lower boundary of 95% HPDI in tibble
                      u_95 = hdi(samples$p_grid[samples$teacher == unique(samples$teacher)[i]]
                                        , .width = .95)[2], #saving upper boundary of 95% HPDI in tibble
                      teacher = unique(samples$teacher)[i],
                      mean = mean(samples$p_grid[samples$teacher == unique(samples$teacher)[i]]), #saving mean of sampled p_grid values
                      std.dev = sd(samples$p_grid[samples$teacher == unique(samples$teacher)[i]]))) #saving standard deviation of sampled p_grid values
  
}
return(hdi_df)}

hdi_df <- get_hdi(res1$samples) #applying function


#the 2 following plots are used multiple times throughout the assignment and will only be commented once

#specifying function for plotting the prior and posterior for each teacher in one plot
one_plot <- function(grid_teacher){
grid_teacher %>% 
  ggplot(aes(x = p_grid, y = posterior)) +
  geom_line(aes (p_grid, prior/n, color = "Prior"))+
  geom_point(aes(color = teacher)) +
  geom_line(aes(color = teacher)) +
  labs(subtitle = "Grid approximation of teachers",
       x = "cogsci knowledge",
       y = "posterior probability") +
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  theme(panel.grid = element_blank())}

#plotting
one_plot(res1$grid_teacher)
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
#specifying function for plotting the prior and posterior for each teacher in multiple plots using facet wrap
more_plots <- function(grid_teacher){
grid_teacher %>% 
  ggplot(aes(x = p_grid, y = posterior, color = teacher)) +
  geom_point() +
  geom_line() +
  geom_line(aes (p_grid, prior/n, color = "Prior"))+
  labs(subtitle = "Grid approximation of teachers",
       x = "cogsci knowledge",
       y = "posterior probability") +
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  facet_wrap(.~teacher) +
  theme(panel.grid = element_blank())}

#plotting
more_plots(res1$grid_teacher)
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-4-2.png)

1.  Change the prior. Given your teachers have all CogSci jobs, you
    should start with a higher appreciation of their knowledge: the
    prior is a normal distribution with a mean of 0.8 and a standard
    deviation of 0.2. Do the results change (and if so how)? 3a. Produce
    plots of the prior and posterior for each teacher.

``` r
set.seed(8)

res1 <- grid_function(teachers, 1001, 1e4, dnorm(seq(from = 0, to = 1, length.out = 1001), 0.8, 0.2)) #specifying new normally distributed prior

hdi_df <- get_hdi(res1$samples)
hdi_df
```

    ## # A tibble: 4 x 5
    ##    l_95  u_95 teacher   mean std.dev
    ##   <dbl> <dbl> <chr>    <dbl>   <dbl>
    ## 1 0.383 0.854 Riccardo 0.630  0.123 
    ## 2 0.555 1     Kristian 0.804  0.134 
    ## 3 0.749 0.856 Josh     0.805  0.0275
    ## 4 0.426 0.589 Mikkel   0.513  0.0417

``` r
one_plot(res1$grid_teacher)
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
more_plots(res1$grid_teacher)
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-5-2.png)

1.  You go back to your teachers and collect more data (multiply the
    previous numbers by 100). Calculate their knowledge with both a
    uniform prior and a normal prior with a mean of 0.8 and a standard
    deviation of 0.2. Do you still see a difference between the results?
    Why?

``` r
teachers2 <- tibble(Teacher = c("Riccardo", "Kristian", "Josh", "Mikkel")) %>% #creating new tibble of teacher data
  mutate(Correct = c(3,2,160,66)*100, #multiplying previous correct questions by 100
         Questions = c(6,2,198,132)*100) #multiplying previous questions by 100

set.seed(8)


#grid approximation with uniform prior using new data


res1 <- grid_function(teachers2, 1001, 1e4, 1) #specifying uniform prior

hdi_df <- get_hdi(res1$samples)
hdi_df
```

    ## # A tibble: 4 x 5
    ##    l_95  u_95 teacher   mean std.dev
    ##   <dbl> <dbl> <chr>    <dbl>   <dbl>
    ## 1 0.46  0.539 Riccardo 0.500 0.0205 
    ## 2 0.986 1     Kristian 0.995 0.00502
    ## 3 0.803 0.813 Josh     0.808 0.00281
    ## 4 0.491 0.508 Mikkel   0.500 0.00437

``` r
#Only using the facet wrapped plots
more_plots(res1$grid_teacher)
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
#####

#grid approximation with normally distributed prior using new data
set.seed(8)

res1 <- grid_function(teachers2, 1001, 1e4, dnorm(seq(from = 0, to = 1, length.out = 1001), 0.8, 0.2)) #specifying normally distributed prior

hdi_df <- get_hdi(res1$samples)
hdi_df
```

    ## # A tibble: 4 x 5
    ##    l_95  u_95 teacher   mean std.dev
    ##   <dbl> <dbl> <chr>    <dbl>   <dbl>
    ## 1 0.462 0.541 Riccardo 0.503 0.0204 
    ## 2 0.985 1     Kristian 0.995 0.00513
    ## 3 0.803 0.813 Josh     0.808 0.00281
    ## 4 0.491 0.508 Mikkel   0.500 0.00437

``` r
#Only using the facet wrapped plots
more_plots(res1$grid_teacher)
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-6-2.png)

1.  Imagine you’re a skeptic and think your teachers do not know
    anything about CogSci, given the content of their classes. How would
    you operationalize that belief?

``` r
set.seed(8)

res1 <- grid_function(teachers, 1001, 1e4, dnorm(seq(from = 0, to = 1, length.out = 1001), 0.5, 0.05)) #specifying skeptic normally distributed prior


hdi_df <- get_hdi(res1$samples)
hdi_df
```

    ## # A tibble: 4 x 5
    ##    l_95  u_95 teacher   mean std.dev
    ##   <dbl> <dbl> <chr>    <dbl>   <dbl>
    ## 1 0.405 0.594 Riccardo 0.500  0.0486
    ## 2 0.41  0.605 Kristian 0.510  0.0495
    ## 3 0.659 0.771 Josh     0.718  0.0286
    ## 4 0.433 0.561 Mikkel   0.500  0.0328

``` r
#Only using the facet wrapped plots
more_plots(res1$grid_teacher)
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-7-1.png)

1.  Optional question: Can you estimate the difference between
    Riccardo’s estimated knowledge and that of each of the other
    teachers? Would you deem it credible (that is, would you believe
    that it is actually different)?

2.  Bonus knowledge: all the stuff we have done can be implemented in a
    lme4-like fashion using the brms package. Here is an example.

``` r
# library(brms)
# 
# d <- data.frame(
#   Correct=c(3,2,160,66),
#   Questions=c(6,2,198,132),
#   Teacher=c("RF","KT","JS","MW"))
# 
# # Model sampling only from the prior (for checking the predictions your prior leads to)
# FlatModel_priorCheck <- brm(Correct|trials(Questions) ~ 1, 
#                  data = subset(d, Teacher=="RF"),
#                  prior = prior("uniform(0,1)", class = "Intercept"),
#                  family = binomial,
#                  sample_prior = "only") # here we tell the model to ignore the data
# 
# # Plotting the predictions of the model (prior only) against the actual data
# pp_check(FlatModel_priorCheck, nsamples = 100)
# 
# # Model sampling by combining prior and likelihood
# FlatModel <- brm(Correct|trials(Questions) ~ 1, 
#                  data = subset(d, Teacher=="RF"),
#                  prior = prior("uniform(0,1)", class = "Intercept"),
#                  family = binomial,
#                  sample_prior = T)
# # Plotting the predictions of the model (prior + likelihood) against the actual data
# pp_check(FlatModel, nsamples = 100)
# 
# # plotting the posteriors and the sampling process
# plot(FlatModel)
# 
# 
# PositiveModel_priorCheck <- brm(Correct|trials(Questions) ~ 1,
#                      data = subset(d, Teacher=="RF"),
#                      prior = prior("normal(0.8,0.2)", 
#                                    class = "Intercept"),
#                      family=binomial,
#                      sample_prior = "only")
# pp_check(PositiveModel_priorCheck, nsamples = 100)
# 
# PositiveModel <- brm(Correct|trials(Questions) ~ 1,
#                      data = subset(d, Teacher=="RF"),
#                      prior = prior("normal(0.8,0.2)", 
#                                    class = "Intercept"),
#                      family=binomial,
#                      sample_prior = T)
# pp_check(PositiveModel, nsamples = 100)
# plot(PositiveModel)
# 
# SkepticalModel_priorCheck <- brm(Correct|trials(Questions) ~ 1, 
#                       data = subset(d, Teacher=="RF"),
#                       prior=prior("normal(0.5,0.01)", class = "Intercept"),
#                       family=binomial,
#                       sample_prior = "only")
# pp_check(SkepticalModel_priorCheck, nsamples = 100)
# 
# SkepticalModel <- brm(Correct|trials(Questions) ~ 1, 
#                       data = subset(d, Teacher=="RF"),
#                       prior = prior("normal(0.5,0.01)", class = "Intercept"),
#                       family = binomial,
#                       sample_prior = T)
# pp_check(SkepticalModel, nsamples = 100)
# plot(SkepticalModel)
```

If you dare, try to tweak the data and model to test two hypotheses: -
Is Kristian different from Josh? - Is Josh different from chance?

### Second part: Focusing on predictions

Last year you assessed the teachers (darned time runs quick!). Now you
want to re-test them and assess whether your models are producing
reliable predictions. In Methods 3 we learned how to do machine-learning
style assessment of predictions (e.g. rmse on testing datasets).
Bayesian stats makes things a bit more complicated. So we’ll try out how
that works. N.B. You can choose which prior to use for the analysis of
last year’s data.

Questions to be answered (but see guidance below): 1- Write a paragraph
discussing how assessment of prediction performance is different in
Bayesian vs. frequentist models 2- Provide at least one plot and one
written line discussing prediction errors for each of the teachers.

This is the old data: - Riccardo: 3 correct answers out of 6 questions -
Kristian: 2 correct answers out of 2 questions (then he gets bored) -
Josh: 160 correct answers out of 198 questions (Josh never gets bored) -
Mikkel: 66 correct answers out of 132 questions

This is the new data: - Riccardo: 9 correct answers out of 10 questions
(then he freaks out about teaching preparation and leaves) - Kristian: 8
correct answers out of 12 questions - Josh: 148 correct answers out of
172 questions (again, Josh never gets bored) - Mikkel: 34 correct
answers out of 65 questions

Guidance Tips

``` r
teachers_new <- #creating tibble of teacher data
  tibble(Teacher = c("Riccardo", "Kristian", "Josh", "Mikkel"),
                   Correct = c(9,8,148,34),
                   Questions = c(10,12,172,65))
```

1.  There are at least two ways of assessing predictions.
2.  Last year’s results are this year’s expectations.

``` r
set.seed(8)

res1 <- grid_function(teachers, 1001, 1e4, dnorm(seq(from = 0, to = 1, length.out = 1001), 0.8, 0.2)) #specifying new prior

hdi_df <- get_hdi(res1$samples)
hdi_df
```

    ## # A tibble: 4 x 5
    ##    l_95  u_95 teacher   mean std.dev
    ##   <dbl> <dbl> <chr>    <dbl>   <dbl>
    ## 1 0.383 0.854 Riccardo 0.630  0.123 
    ## 2 0.555 1     Kristian 0.804  0.134 
    ## 3 0.749 0.856 Josh     0.805  0.0275
    ## 4 0.426 0.589 Mikkel   0.513  0.0417

``` r
more_plots(res1$grid_teacher)
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-10-1.png)

1.  Are the parameter estimates changing? (way 1)

``` r
set.seed(8)

#The function from earlier doesn't work because the prior is changing during each iteration
n <- 1001
n_samples <- 1e4
grid_teacher <- tibble()
samples <- tibble()

for (i in seq_along(teachers_new$Teacher)){
    tib <- tibble(p_grid     = seq(from = 0, to = 1, length.out = n),
           prior      = dnorm(p_grid, hdi_df$mean[i], hdi_df$std.dev[i])) %>% #setting normally distributed prior based on the priors from previous
    mutate(likelihood =dbinom(teachers_new$Correct[i], size = teachers_new$Questions[i], prob = p_grid)) %>% 
    mutate(posterior  = (likelihood * prior) / sum(likelihood * prior),
           teacher = teachers_new$Teacher[i]) %>% 
    mutate(errors = posterior-(prior/n)) #adding error
    samples <- bind_rows(samples, tib %>% 
  sample_n(size = n_samples, weight = posterior, replace = T))
    grid_teacher <- bind_rows(grid_teacher,tib)
}


hdi_df <- get_hdi(samples)
hdi_df
```

    ## # A tibble: 4 x 5
    ##    l_95  u_95 teacher   mean std.dev
    ##   <dbl> <dbl> <chr>    <dbl>   <dbl>
    ## 1 0.556 0.922 Riccardo 0.744  0.0955
    ## 2 0.554 0.88  Kristian 0.720  0.0847
    ## 3 0.79  0.869 Josh     0.831  0.0202
    ## 4 0.447 0.580 Mikkel   0.515  0.0343

``` r
more_plots(grid_teacher)
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
#plotting the difference between this year and last years posterior distribution
grid_teacher %>% 
  ggplot(aes(x = p_grid, y = errors, color = teacher)) +
  geom_point() +
  geom_line() +
  labs(subtitle = "Grid approximation of teachers",
       x = "cogsci knowledge",
       y = "posterior probability") +
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  facet_wrap(.~teacher) +
  theme(panel.grid = element_blank())
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-11-2.png)

1.  How does the new data look in last year’s predictive posterior?
    (way 2)

``` r
#apparently we don't want to do this?? Why??
```
