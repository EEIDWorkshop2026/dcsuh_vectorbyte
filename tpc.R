#playing with vectraits and TPCs

library(here)

library(magrittr)
library(tidyverse)

#functions for accessing vectraits data
source(here("VecTraits_Dataset_Access.R"))

theme_set(theme_bw())

# fitting TPC intro -------------------------------------------------------



# Put some data in here
x <- c(1,2,3,4)
y <- c(9.5, 11, 19.6, 20)

# Plot it - looks kind of like a line
plot(x, y)

#set beta
beta0 <- seq(0, 10, 0.1)
beta1 <- seq(0, 10, 0.1)

#calculate sum of squared residuals
betas <- 
  expand_grid(beta0 = beta0,
              beta1 = beta1)

betas %<>%
  rowwise() %>%
  mutate(SSR = sum((y - (beta0 + beta1*x))^2))

#make 3d plot
plot_ly(betas, x = ~beta0, y = ~beta1, z = ~SSR,
        color = ~SSR)



# Surfaces are difficult to visualize, so let's break it down into   # the components for ease

betas_0 <- betas %>%group_by(beta0) %>% summarize(SSRmin = min(SSR))
betas_1 <- betas %>%group_by(beta1) %>% summarize(SSRmin = min(SSR))

# Notice that the minimum SSR happens when beta0 = 5
plot(betas_0$beta0, betas_0$SSRmin)
plot(betas_1$beta1, betas_1$SSRmin)




# building TPC from vectraits data ----------------------------------------

mosq <- getDataset(579)

dat <- mosq[[1]]

table(dat$SecondStressorValue)
table(dat$Interactor1Temp)
table(dat$OriginalTraitValue)

dat_grouped <-
  dat %>%
  group_by(Interactor1Temp, SecondStressorValue) %>%
  summarize(mean_long = mean(OriginalTraitValue),
            sd_long = sd(OriginalTraitValue),
            n = n(),
            se_long = sd_long/sqrt(n))

dat_grouped %>%
  ggplot(., aes(x = Interactor1Temp, y = mean_long)) +
  geom_point() +
  geom_linerange(aes(ymin = mean_long - 1.96*se_long, 
                     ymax = mean_long + 1.96*se_long)) +
  scale_x_continuous(n.breaks = 10) + 
  facet_wrap(~SecondStressorValue)


#Fit briere function



#explicit resource levels

briere_55 <- 
  nls(OriginalTraitValue ~ a*Interactor1Temp*(Interactor1Temp-tmin)*(tmax-Interactor1Temp)^(1/2),
      start = list(a = 1, tmin = 5, tmax = 30),
      #start = list(a = 1, tmin = 20, tmax = 35),
      data = dat %>% filter(SecondStressorValue == 55))

briere_110 <- nls(OriginalTraitValue ~ a*Interactor1Temp*(Interactor1Temp-tmin)*(tmax-Interactor1Temp)^(1/2),
              #start = list(a = 1, tmin = 22, tmax = 34),
              #start = list(a = 1, tmin = 20, tmax = 35),
              start = list(a = 1, tmin = 10, tmax = 36),
              data = dat %>% filter(SecondStressorValue == 110))

briere_165 <- nls(OriginalTraitValue ~ a*Interactor1Temp*(Interactor1Temp-tmin)*(tmax-Interactor1Temp)^(1/2),
                  start = list(a = 1, tmin = 22, tmax = 34),
                  #start = list(a = 1, tmin = 20, tmax = 35),
                  data = dat %>% filter(SecondStressorValue == 165))

briere_220 <- nls(OriginalTraitValue ~ a*Interactor1Temp*(Interactor1Temp-tmin)*(tmax-Interactor1Temp)^(1/2),
                  start = list(a = 1, tmin = 22, tmax = 34),
                  #start = list(a = 1, tmin = 20, tmax = 35),
                  data = dat %>% filter(SecondStressorValue == 220))

summary(briere_110)
coef(briere_110)

hist(residuals(briere_110))

plot(residuals(briere_110), predict(briere_110))


pred_dat <-
  expand.grid(Interactor1Temp = seq(0, 36, length.out = 1000))

pred_low <-
  pred_dat %>%
  mutate(resource = 110)

pred_med <-
  pred_dat %>%
  mutate(resource = 165)

pred_high <-
  pred_dat %>%
  mutate(resource = 220)

pred_low$output <- predict(briere_110, newdata = pred_dat)
pred_med$output <- predict(briere_165, newdata = pred_dat)
pred_high$output <- predict(briere_220, newdata = pred_dat)

pred_full <-
  rbind(pred_low, pred_med, pred_high)

pred_full %>%
  rename(SecondStressorValue = resource) %>%
  ggplot(., aes(x = Interactor1Temp, y = output)) +
  geom_line() +
  geom_point(data = dat %>% filter(SecondStressorValue != 55),
             aes(x = Interactor1Temp, y = OriginalTraitValue)) +
  geom_point(data = dat_grouped %>% filter(SecondStressorValue != 55),
             aes(x = Interactor1Temp, y = mean_long),
             color = "red",
             size = 5,
             shape = 15) +
  facet_wrap(~SecondStressorValue, nrow = 1)


#additive version of resource model

briere_add <- 
  nls(OriginalTraitValue ~ 
        a*Interactor1Temp*(Interactor1Temp-tmin)*(tmax-Interactor1Temp)^(1/2) + 
        b*SecondStressorValue,
      start = list(a = 1, tmin = 22, tmax = 34, b = 0.01),
      data = dat)

coef(briere_add)

pred_add <-
  expand.grid(Interactor1Temp = seq(5, 36, length.out = 1000),
              SecondStressorValue = seq(110, 220, by = 1))

pred_add$output <-
  predict(briere_add, pred_add)

pred_add %>%
  filter(SecondStressorValue %in% c(110, 130, 150, 170, 190, 210)) %>%
  ggplot(., aes(x = Interactor1Temp, y = output, 
                group = SecondStressorValue, color = SecondStressorValue)) +
  geom_line()

pred_add %>%
  filter(SecondStressorValue %in% c(110, 165, 220)) %>%
  ggplot(., aes(x = Interactor1Temp, y = output)) +
  geom_line() +
  geom_point(data = dat %>% filter(SecondStressorValue != 55),
             aes(x = Interactor1Temp, y = OriginalTraitValue)) +
  geom_point(data = dat_grouped %>% filter(SecondStressorValue != 55),
             aes(x = Interactor1Temp, y = mean_long),
             color = "red",
             size = 5,
             shape = 15) +
  facet_wrap(~SecondStressorValue, nrow = 1)
