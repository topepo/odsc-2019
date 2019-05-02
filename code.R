## -----------------------------------------------------------------------------
# R code for Modeling in the tidyverse @ 2019 ODSC East

library(tidyverse)
library(tidymodels)

## -----------------------------------------------------------------------------

thm <- theme_bw() + 
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), 
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

## -----------------------------------------------------------------------------

library(tidyverse)

ames_prices <- 
  "http://bit.ly/2whgsQM" %>%
  read_delim(delim = "\t", guess_max = 2000) %>%
  rename_at(vars(contains(' ')), list(~gsub(' ', '_', .))) %>%
  dplyr::rename(Sale_Price = SalePrice) %>%
  dplyr::filter(!is.na(Electrical)) %>%
  dplyr::select(-Order, -PID, -Garage_Yr_Blt)

ames_prices %>%
  group_by(Alley) %>%
  summarize(
    mean_price = mean(Sale_Price / 1000),
    n = sum(!is.na(Sale_Price))
  )


## -----------------------------------------------------------------------------
# purrr loaded with tidyverse or tidymodels package

mini_ames <- ames_prices %>%
  dplyr::select(Alley, Sale_Price, Yr_Sold) %>%
  dplyr::filter(!is.na(Alley))

head(mini_ames, n = 5)

by_alley <- split(mini_ames, mini_ames$Alley)

# map(.x, .f, ...)
map(by_alley, head, n = 2)

map(by_alley, nrow)

map_int(by_alley, nrow)

map(
  by_alley, 
  ~summarise(.x, max_price = max(Sale_Price))
)


## -----------------------------------------------------------------------------

ames_lst_col <- nest(mini_ames, -Alley)
ames_lst_col

ames_lst_col %>%
  mutate(
    n_row = map_int(data, nrow),
    max   = map_dbl(data, ~max(.x$Sale_Price))
  )


## -----------------------------------------------------------------------------

library(AmesHousing)
ames <- make_ames()

## -----------------------------------------------------------------------------

# resample loaded with tidyverse or tidymodels package
ames <- 
  make_ames() %>% 
  # Remove quality-related predictors
  dplyr::select(-matches("Qu"))
nrow(ames)

# resample functions
# Make sure that you get the same random numbers
set.seed(4595)
data_split <- initial_split(ames, strata = "Sale_Price")

ames_train <- training(data_split)
ames_test  <- testing(data_split)

nrow(ames_train)/nrow(ames)

## -----------------------------------------------------------------------------

# result of initial_split()
# <training / testing / total>
data_split

training(data_split)

## -----------------------------------------------------------------------------

## model_fn(Sale_Price ~ Neighborhood + Year_Sold + Neighborhood:Year_Sold, data = ames_train)

## model_fn(Sale_Price ~ ., data = ames_train)

## model_fn(log10(Sale_Price) ~ ns(Longitude, df = 3) + ns(Latitude, df = 3), data = ames_train)

## # Usually, the variables must all be numeric
## pre_vars <- c("Year_Sold", "Longitude", "Latitude")
## model_fn(x = ames_train[, pre_vars],
##          y = ames_train$Sale_Price)


## -----------------------------------------------------------------------------

simple_lm <- lm(log10(Sale_Price) ~ Longitude + Latitude, data = ames_train)

simple_lm_values <- augment(simple_lm)
names(simple_lm_values)


## -----------------------------------------------------------------------------

# parsnip loaded with tidymodels
spec_lin_reg <- linear_reg()
spec_lin_reg

spec_lm <- set_engine(spec_lin_reg, "lm")
spec_lm


## -----------------------------------------------------------------------------

fit_lm <- fit(
  spec_lm,
  log10(Sale_Price) ~ Longitude + Latitude,
  data = ames_train
)

fit_lm


## -----------------------------------------------------------------------------

ames_train_log <- ames_train %>%
  mutate(Sale_Price_Log = log10(Sale_Price))

fit_xy(
  spec_lm,
  y = ames_train_log$Sale_Price_Log,
  x = ames_train_log %>% dplyr::select(Latitude, Longitude)
)


## -----------------------------------------------------------------------------

spec_stan <- 
  spec_lin_reg %>%
  # Engine specific arguments are passed through here
  set_engine("stan", chains = 4, iter = 1000)

# Otherwise, looks exactly the same!
fit_stan <- fit(
  spec_stan,
  log10(Sale_Price) ~ Longitude + Latitude,
  data = ames_train
)


coef(fit_stan$fit)
coef(fit_lm$fit)


## -----------------------------------------------------------------------------

fit_knn <- 
  nearest_neighbor(mode = "regression", neighbors = 5) %>%
  set_engine("kknn") %>% 
  fit(log10(Sale_Price) ~ Longitude + Latitude, data = ames_train)
fit_knn


# Numeric predictions always in a df
# with column `.pred`
test_pred <- fit_lm %>%
  predict(ames_test) %>%
  bind_cols(ames_test) %>%
  mutate(log_price = log10(Sale_Price))

test_pred %>% 
  dplyr::select(log_price, .pred) %>% 
  slice(1:3)

# yardstick loaded by tidymodels

# yardstick has many metrics for assessing performance. 
# They can be bundled together
perf_metrics <- metric_set(rmse, rsq, ccc)

# A tidy result back:
test_pred  %>% 
  perf_metrics(truth = log_price, estimate = .pred)


## -----------------------------------------------------------------------------

ggplot(ames_train, aes(x = Neighborhood)) + geom_bar() + coord_flip() + xlab("")


## -----------------------------------------------------------------------------

# recipes loaded by tidymodels
mod_rec <- 
  recipe(Sale_Price ~ Longitude + Latitude, data = ames_train) %>%
  step_log(Sale_Price, base = 10)


mod_rec <- recipe(
    Sale_Price ~ Longitude + Latitude + Neighborhood, 
    data = ames_train
  ) %>%
  step_log(Sale_Price, base = 10) %>%
  
  # Lump factor levels that occur in 
  # <= 5% of data as "other"
  step_other(Neighborhood, threshold = 0.05) %>%
  
  # Create dummy variables for _any_ factor variables
  step_dummy(all_nominal())

mod_rec


## -----------------------------------------------------------------------------

mod_rec_trained <- prep(mod_rec, training = ames_train, verbose = TRUE)

mod_rec_trained

ames_test_dummies <- bake(mod_rec_trained, new_data = ames_test)
names(ames_test_dummies)


## -----------------------------------------------------------------------------

## recipe(..., data = data_set)
## prep(...,   training = data_set)
## bake(...,   new_data = data_set)

## -----------------------------------------------------------------------------

price_breaks <- (1:6)*(10^5)

ggplot(
    ames_train, 
    aes(x = Year_Built, y = Sale_Price)
  ) + 
  geom_point(alpha = 0.4) +
  scale_x_log10() + 
  scale_y_continuous(
    breaks = price_breaks, 
    trans = "log10"
  ) +
  geom_smooth(method = "loess")

## -----------------------------------------------------------------------------

library(MASS) # to get robust linear regression model

ggplot(
    ames_train, 
    aes(x = Year_Built, 
        y = Sale_Price)
  ) + 
  geom_point(alpha = 0.4) +
  scale_y_continuous(
    breaks = price_breaks, 
    trans = "log10"
  ) + 
  facet_wrap(~ Central_Air, nrow = 2) +
  geom_smooth(method = "rlm") 


## -----------------------------------------------------------------------------

mod1 <- lm(log10(Sale_Price) ~ Year_Built + Central_Air, data = ames_train)
mod2 <- lm(log10(Sale_Price) ~ Year_Built + Central_Air + Year_Built:Central_Air, data = ames_train)
anova(mod1, mod2)


## -----------------------------------------------------------------------------

recipe(Sale_Price ~ Year_Built + Central_Air, data = ames_train) %>%
  step_log(Sale_Price) %>%
  step_dummy(Central_Air) %>%
  step_interact(~ starts_with("Central_Air"):Year_Built) %>%
  prep(training = ames_train) %>%
  juice() %>%
  # select a few rows with different values
  slice(153:157)


## -----------------------------------------------------------------------------

ames_rec <- 
  recipe(Sale_Price ~ Bldg_Type + Neighborhood + Year_Built + 
           Gr_Liv_Area + Full_Bath + Year_Sold + Lot_Area +
           Central_Air + Longitude + Latitude,
         data = ames_train) %>%
  step_log(Sale_Price, base = 10) %>%
  step_BoxCox(Lot_Area, Gr_Liv_Area) %>%
  step_other(Neighborhood, threshold = 0.05)  %>%
  step_dummy(all_nominal()) %>%
  step_interact(~ starts_with("Central_Air"):Year_Built) %>%
  step_bs(Longitude, Latitude, options = list(df = 5))


## -----------------------------------------------------------------------------

ggplot(ames_train, 
       aes(x = Longitude, y = Sale_Price)) + 
  geom_point(alpha = .5) + 
  geom_smooth(
    method = "lm", 
    formula = y ~ splines::bs(x, 5), 
    se = FALSE
  ) + 
  scale_y_log10()

ggplot(ames_train, 
       aes(x = Latitude, y = Sale_Price)) + 
  geom_point(alpha = .5) + 
  geom_smooth(
    method = "lm", 
    formula = y ~ splines::bs(x, 5), 
    se = FALSE
  ) + 
  scale_y_log10()

## -----------------------------------------------------------------------------

ames_rec <- prep(ames_rec)

# Since we want the training set back so use `juice()`
train_data <- juice(ames_rec)   
# For other data
test_data  <- bake(ames_rec, ames_test) 

lm_fit <- 
  linear_reg() %>% 
  set_engine("lm") %>% 
  fit(Sale_Price ~ ., data = train_data)

broom::glance(lm_fit$fit)

## -----------------------------------------------------------------------------

test_data <- 
  test_data %>% 
  bind_cols(predict(lm_fit, test_data))

test_data %>% 
  perf_metrics(truth = Sale_Price, estimate = .pred)

ggplot(test_data, aes(x = 10^Sale_Price, y = 10^.pred)) + 
  geom_point(alpha = .4) +
  geom_abline(lty = 2) +
  scale_x_log10()  + scale_y_log10() +
  coord_equal()