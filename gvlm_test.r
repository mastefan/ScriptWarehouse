# gvlma test

## test that GVLMA should pass on all assumptions
# create time series dataset
time <- 1900:1999
temp <- rnorm(n = 100, mean = 8, sd = 10)
data <- data.frame(time, temp)

# create linear model and plot to observe characteristics
data.lm <- lm(temp ~ time, data)
coefficients(data.lm)
summary(data.lm)

ggplot2::ggplot(data, ggplot2::aes(x = time, y = temp)) + 
  ggplot2::theme_classic() + 
  ggplot2::geom_point() + 
  ggplot2::geom_abline(slope = coefficients(data.lm)[[2]],
                       intercept = coefficients(data.lm)[[1]]) + 
  ggplot2::labs(title = "GVLMA Test 1 - Passable")

# basic gvlma test
library(gvlma)

gvlma(temp ~ time, data)

## test that gvlma should fail for kurtosis (deviation from normal distribution)
# create bimodal dataset
temp <- c(rnorm(n = 10, mean = 2, sd = 10),
          rnorm(n = 20, mean = 50, sd = 5),
          rnorm(n = 40, mean = 12, sd = 2),
          rnorm(n = 20, mean = 50, sd = 5),
          rnorm(n = 10, mean = 2, sd = 10))
data <- data.frame(time, temp)

# create linear model and plot to observe characteristics
data.lm <- lm(temp ~ time, data)
coefficients(data.lm)
summary(data.lm)

ggplot2::ggplot(data, ggplot2::aes(x = time, y = temp)) + 
  ggplot2::theme_classic() + 
  ggplot2::geom_point() + 
  ggplot2::geom_abline(slope = coefficients(data.lm)[[2]],
                       intercept = coefficients(data.lm)[[1]]) + 
  ggplot2::labs(title = "GVLMA Test 1 - Kurtosis")

gvlma(temp ~ time, data)

## test that gvlma should fail for the link function
# create bimodal dataset
temp <- c(rnorm(n = 10, mean = 50, sd = 5),
          rnorm(n = 10, mean = 40, sd = 5),
          rnorm(n = 10, mean = 30, sd = 5),
          rnorm(n = 10, mean = 20, sd = 5),
          rnorm(n = 10, mean = 18, sd = 5),
          rnorm(n = 10, mean = 18, sd = 5),
          rnorm(n = 10, mean = 20, sd = 5),
          rnorm(n = 10, mean = 30, sd = 5),
          rnorm(n = 10, mean = 40, sd = 5),
          rnorm(n = 10, mean = 50, sd = 5))
data <- data.frame(time, temp)

# create linear model and plot to observe characteristics
data.lm <- lm(temp ~ time, data)
coefficients(data.lm)
summary(data.lm)

ggplot2::ggplot(data, ggplot2::aes(x = time, y = temp)) + 
  ggplot2::theme_classic() + 
  ggplot2::geom_point() + 
  ggplot2::geom_abline(slope = coefficients(data.lm)[[2]],
                       intercept = coefficients(data.lm)[[1]]) + 
  ggplot2::labs(title = "GVLMA Test 1 - Link Function")

gvlma(temp ~ time, data)

## test that gvlma should fail for heteroscedasticity (looks like a triangle when plotted)
# create bimodal dataset
temp <- c(rnorm(n = 10, mean = 5, sd = 2),
          rnorm(n = 10, mean = 10, sd = 4),
          rnorm(n = 10, mean = 15, sd = 6),
          rnorm(n = 10, mean = 20, sd = 8),
          rnorm(n = 10, mean = 25, sd = 10),
          rnorm(n = 10, mean = 30, sd = 12),
          rnorm(n = 10, mean = 35, sd = 14),
          rnorm(n = 10, mean = 40, sd = 16),
          rnorm(n = 10, mean = 45, sd = 18),
          rnorm(n = 10, mean = 50, sd = 20))
data <- data.frame(time, temp)

# create linear model and plot to observe characteristics
data.lm <- lm(temp ~ time, data)
coefficients(data.lm)
summary(data.lm)

ggplot2::ggplot(data, ggplot2::aes(x = time, y = temp)) + 
  ggplot2::theme_classic() + 
  ggplot2::geom_point() + 
  ggplot2::geom_abline(slope = coefficients(data.lm)[[2]],
                       intercept = coefficients(data.lm)[[1]]) + 
  ggplot2::labs(title = "GVLMA Test 1 - Heteroscedasticity")

gvlma(temp ~ time, data)

## test that gvlma should fail for skewness
# create bimodal dataset
temp <- c(rnorm(n = 10, mean = 20, sd = 5),
          rnorm(n = 10, mean = 35, sd = 5),
          rnorm(n = 10, mean = 50, sd = 5),
          rnorm(n = 10, mean = 25, sd = 5),
          rnorm(n = 10, mean = 5, sd = 5),
          rnorm(n = 10, mean = 5, sd = 5),
          rnorm(n = 10, mean = 2, sd = 2),
          rnorm(n = 10, mean = 2, sd = 2),
          rnorm(n = 10, mean = 2, sd = 2),
          rnorm(n = 10, mean = 2, sd = 2))
data <- data.frame(time, temp)

# create linear model and plot to observe characteristics
data.lm <- lm(temp ~ time, data)
coefficients(data.lm)
summary(data.lm)

ggplot2::ggplot(data, ggplot2::aes(x = time, y = temp)) + 
  ggplot2::theme_classic() + 
  ggplot2::geom_point() + 
  ggplot2::geom_abline(slope = coefficients(data.lm)[[2]],
                       intercept = coefficients(data.lm)[[1]]) + 
  ggplot2::labs(title = "GVLMA Test 1 - Heteroscedasticity")

gvlma(temp ~ time, data)
