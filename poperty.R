# title: "NYC Property Sale Price Predictor"
# author: "Donghan Lee"
# date: "10/27/2021"

## Introduction

# due to the authorizing issue (needs login),
# a zip file was downloaded from https://www.kaggle.com/new-york-city/nyc-property-sales
# and unziped to "nyc-rolling-sales.csv"

# libraries loading

library(tidyverse)
library(lubridate)
library(dplyr)
library(knitr)

# Benchmark RMSE function

RMSE <- function(predictions, true_ratings){
  sqrt(mean((true_ratings - predictions)^2))
}


# After inspecting the file, I found that the delimiter is ",".
# Thus, tidyverse::read_csv() is used and assigned to sale.

sale <- read_csv("nyc-rolling-sales.csv")

# row: 84548 column: 22

# inspection
head(sale)
tail(sale)
str(sale)
names(sale)

# sale contains 22 columns.
# "...1" : property index, double
# "BOROUGH" : administration district, double
# "NEIGHBORHOOD" : neighborhodd name, chr
# "BUILDING CLASS CATEGORY" : what kind of property, chr
# "TAX CLASS AT PRESENT" : tax category, chr
# "BLOCK" : double                        
# "LOT" : double
# "EASE-MENT" : logical                   
# "BUILDING CLASS AT PRESENT" : chr
# "ADDRESS" : chr                       
# "APARTMENT NUMBER" : chr
# "ZIP CODE" : double                 
# "RESIDENTIAL UNITS" : double
# "COMMERCIAL UNITS" : double       
# "TOTAL UNITS" : double
# "LAND SQUARE FEET" : chr
# "GROSS SQUARE FEET" : chr
# "YEAR BUILT" : double                  
# "TAX CLASS AT TIME OF SALE" : double
# "BUILDING CLASS AT TIME OF SALE" : chr
# "SALE PRICE" : chr
# "SALE DATE" : POSIXct

# **House Price** which we want to predict.

price <- sale["SALE PRICE"] 

n_distinct(price)

# price contains unique 10008 values.

head(price)

# Price contians "-". 
# "-" will be replaced with average value of prices.
# change to numeric.

price <- as.numeric(t(price))

# replace NA with the average.

mprice <- round(mean(price, na.rm = T), digits = 0)

price <- sapply(price, function(l) {
  ifelse(!is.na(l), l, mprice)
})

price <- round(price, digits = -4)
# correcting the actual data. Starting from price

dat <- data.frame(price = price)

# "...1" seems to be index for property, propId.

propId <- sale["...1"]

n_distinct(propId)

# propId has unique 26736 values. It is over our price.
# Thus, we don't need this.

# "BOROUGH" : administration district, double --> borough, chr
# according to
# "A Property's Borough, Block and Lot Number". NYC.gov. City of New York.
# 1. Manhattan (New York County)
# 2. Bronx (Bronx County)
# 3. Brooklyn (Kings County)
# 4. Queens (Queens County)
# 5. Staten Island (Richmond County)

boro <- sale["BOROUGH"]
n_distinct(boro)

# change borough number to a name accordingly.

boro <- sapply(boro, function(l) {
  ifelse(l == 1, "Manhattan", 
         ifelse(l == 2, "Bronx",
                ifelse(l == 3, "Brooklyn",
                       ifelse(l == 4, "Queens", "Staten Island")
                )
         )
  )
})

boro <- factor(boro)

# collecting borough information

dat <- dat %>% mutate(boro = boro)

# table for the distribution

tab <- dat %>% count(boro) 

tab %>% kable()

# "NEIGHBORHOOD" : neighborhodd name, chr --> neigh

neigh <- as.matrix(sale["NEIGHBORHOOD"])

n_distinct(neigh)

neigh <- factor(neigh)

# In neighorhood, 254 categories exist.

# Thus, a histogram may be easier than a table. 

dat <- dat %>% mutate(neigh = neigh)

dat %>%
  count(neigh) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Neighborhood")

# "BUILDING CLASS CATEGORY" : what kind of property, chr -> b_cat

b_cat <- as.matrix(sale["BUILDING CLASS CATEGORY"])

n_distinct(b_cat)

b_cat <- factor(b_cat)

# collecting b_cat & distribution table

dat <- dat %>% mutate(b_cat = b_cat)

tab <- dat %>% count(b_cat)

tab %>% kable()

# "TAX CLASS AT PRESENT" : tax category, chr -> tax_c_a_p

tax_c_a_p <- as.matrix(sale["TAX CLASS AT PRESENT"])

n_distinct(tax_c_a_p)

tax_c_a_p <- tax_c_a_p

# collecting tax_c_a_p & distribution table

dat <- dat %>% mutate(tax_c_a_p = tax_c_a_p)

tab <- dat %>% count(tax_c_a_p)

tabtax <- tab

tab %>% kable()

# Tax class is four. But there are NA and sub-categories.
# sub categories will be merged and NA should be removed.
# this should be done after collecting all the parameters.

# "BLOCK" : double -> block

block <- as.matrix(sale["BLOCK"])

n_distinct(block)

# block has 11566, which is over price unique value.
# Thus, block won't be used in predicton.

# "LOT" : double -> lot

lot <- as.matrix(sale["LOT"])

n_distinct(lot)

lot <- as.numeric(lot)

# In lot, 2627 categories exist.

# Thus, a histogram may be easier than a table. 

datx <- dat %>% mutate(lot = lot)

datx %>%
  count(lot) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Lot")


# "BUILDING CLASS AT PRESENT" : chr --> b_c_a_p

b_c_a_p <- as.matrix(sale["BUILDING CLASS AT PRESENT"])

n_distinct(b_c_a_p)

b_c_a_p <- factor(b_c_a_p)

# In lot, 167 categories exist.

# Thus, a histogram may be easier than a table. 

dat <- dat %>% mutate(b_c_a_p = b_c_a_p)

dat %>%
  count(b_c_a_p) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Building Class at Present")

# "ADDRESS" : chr --> address                     

address <- as.matrix(sale["ADDRESS"])

n_distinct(address)

# address has 67563, which is over price unique value.
# Thus, address won't be used in predicton.

# "APARTMENT NUMBER" : chr
# Because address is discarded, apartment number is useless.

# "ZIP CODE" : double  --> zip

zip <- as.matrix(sale["ZIP CODE"])

n_distinct(zip)

zip <- factor(zip)

# In zip code case, 186 categories exist.
# Thus, a histogram may be easier than a table. 

dat <- dat %>% mutate(zip = zip)

dat %>%
  count(zip) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Zip code")

# "RESIDENTIAL UNITS" : double  --> resi

resi <- as.matrix(sale["RESIDENTIAL UNITS"])

n_distinct(resi)

resi <- factor(resi)

# In residential unit case, 176 categories exist.
# Thus, a histogram may be easier than a table. 

dat <- dat %>% mutate(resi = resi)

dat %>%
  count(resi) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Residential Units")

# "COMMERCIAL UNITS" : double --> comm

comm <- as.matrix(sale["COMMERCIAL UNITS"])

n_distinct(comm)

comm <- factor(comm)

# In commerical unit case, 55 categories exist.

dat <- dat %>% mutate(comm = comm)

tab <- dat %>% count(comm) 

tab %>% kable()

# "TOTAL UNITS" : double -> tot

tot <- as.matrix(sale["TOTAL UNITS"])

n_distinct(tot)

tot <- factor(tot)

# In total unit case, 192 categories exist.
# Thus, a histogram may be easier than a table. 

dat <- dat %>% mutate(tot = tot)

dat %>%
  count(tot) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Total Units")

# "LAND SQUARE FEET" : chr --> l_sqf

l_sqf <- as.matrix(sale["LAND SQUARE FEET"])

n_distinct(l_sqf)

# Land Square feet has 6062 unique values, which is more than a half of price unique values. Thus, not used.
# however, one can round the numbers.

l_sqf <- as.numeric(l_sqf)
l_sqf <- round(l_sqf, digits = -3)

n_distinct(l_sqf)

# unique number is reduced to 170.

# Thus, a histogram may be easier than a table. 

datx <- dat %>% mutate(l_sqf = l_sqf)

datx %>%
  count(l_sqf) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Land Feet")

# NA is replaced with the average.

ml_sqf = mean(l_sqf, na.rm = T)

l_sqf <- sapply(l_sqf, function(l) {
  ifelse(!is.na(l), l, ml_sqf)
})

dat <- dat %>% mutate(l_sqf = l_sqf)


# "GROSS SQUARE FEET" : chr --> g_sqrf

g_sqrf <- as.matrix(sale["GROSS SQUARE FEET"])

n_distinct(g_sqrf)

# gross Square feet has 5691 unique values, which is more than a half of price unique values. Thus, not used.

# however, one can round the numbers.

g_sqrf <- as.numeric(g_sqrf)
g_sqrf <- round(g_sqrf, digits = -3)

n_distinct(g_sqrf)

# unique number is 239

# NA is replaced with the average.

mg_sqf = mean(g_sqrf, na.rm = T)

g_sqrf <- sapply(g_sqrf, function(l) {
  ifelse(!is.na(l), l, mg_sqf)
})

dat <- dat %>% mutate(g_sqrf = g_sqrf)

# "YEAR BUILT" : double --> year_buit

year_buit <- as.matrix(sale["YEAR BUILT"])

n_distinct(year_buit)

year_buit <- factor(year_buit)
# In year built case, 158 categories exist.
# Thus, a histogram may be easier than a table. 

dat <- dat %>% mutate(year_buit = year_buit)

dat %>%
  count(year_buit) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Year Built")

# "TAX CLASS AT TIME OF SALE" : double --> t_c_a_s

t_c_a_s <- as.matrix(sale["TAX CLASS AT TIME OF SALE"])

n_distinct(t_c_a_s)

t_c_a_s <- factor(as.character(t_c_a_s))

# Tax class at the time of sale has 4 univque values.
# table is shown.

dat <- dat %>% mutate(t_c_a_s = t_c_a_s)

tab <- dat %>% count(t_c_a_s)

tab %>% kable()

# "BUILDING CLASS AT TIME OF SALE" : chr --> b_c_a_s

b_c_a_s <- as.matrix(sale["BUILDING CLASS AT TIME OF SALE"])

n_distinct(b_c_a_s)

b_c_a_s <- factor(b_c_a_p)

# In building class at the time of sale case, 166 categories exist.
# Thus, a histogram may be easier than a table. 

dat <- dat %>% mutate(b_c_a_s = b_c_a_s)

dat %>%
  count(b_c_a_s) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Building Class at the time of Sale")

# check the dat.

names(dat)

# Now, the tax class at present need to be modified.
# first remove NA and associated values.

dat <- dat %>% filter(tax_c_a_p != "NA")

# original table

tabtax %>% kable()

# table after removing

tab <- dat %>% count(tax_c_a_p)

tabtax <- tab

tab %>% kable()

# Now, combine 1, 1A, 1B, and 1C.
# 2, 2A, 2B, and 2C.
# table for comparison

dat$tax_c_a_p[dat$tax_c_a_p %in%  c("1A", "1B", "1C")] <- "1"

#dat$tax_c_a_p[dat$tax_c_a_p == "1B"] <- "1"
#dat$tax_c_a_p[dat$tax_c_a_p == "1C"] <- "1"
dat$tax_c_a_p[dat$tax_c_a_p %in%  c("2A", "2B", "2C")] <- "2"
#dat$tax_c_a_p[dat$tax_c_a_p == "2B"] <- "2"
#dat$tax_c_a_p[dat$tax_c_a_p == "2C"] <- "2"


tax_c_a_p <- as.numeric(dat$tax_c_a_p)
tax_c_a_p <- as.character(tax_c_a_p)
tax_c_a_p <- factor(tax_c_a_p)

dat <- dat %>% select(price, boro, neigh, b_cat, b_c_a_p, 
                      zip, resi, comm, tot, l_sqf, 
                      g_sqrf, year_buit, t_c_a_s, b_c_a_s)

dat <- dat %>% mutate(tax_c_a_p = tax_c_a_p)

names(dat)

tab <- dat %>% count(tax_c_a_p)

tabtax <- tab

tab %>% kable()

# data analysis
str(dat)

# get correlation among parameters and between parameters and intrested values
# For this, polycor library is needed

#library(polycor)
# using hetcor function for correlation coefficiency.

#x <- hetcor(dat, std.err = F)


# it took so long. The result was not obtained.
# Instead correlate function that ignores non-numerics.

library(lsr)

x <- correlate(dat)
x

#x <- round(cor(dat,dat),2)
#x %>% kable()

#heatmap(as.matrix(x))

# Final data check before predictors

names(dat)
dim(dat)

# final dat has 83810 rows and 15 columns.
# column names are "price", "boro", "neigh", "b_cat", "b_c_a_p",
# "zip", "resi", "comm", "tot", "l_sqf", 
# "g_sqrf", "year_buit", "t_c_a_s", "b_c_a_s", "tax_c_a_p".


# with a laptop computer, predictors are extremely slow.
# In order to check the validity, 1000 data are sampled.

set.seed(1, sample.kind = "Rounding")

dat_index <- sample(seq_len(nrow(dat)), size = 1000)

dats <- dat[dat_index,]

# dats split for a hang-out test

######## predictor ########
# load caret library

library(caret)

# set random seed to 1
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

# split sale into two data sets, train and a hold-out test sets.
# 90% train
# 10% test

test_index <- createDataPartition(y = dats$price, times = 1, p = 0.1, list = FALSE)

trainset <- dats[-test_index,]
testset <- dats[test_index,]

# check the trainset and testset size

nrow(trainset)
nrow(testset)

# trainset 75427 and testset 8383
# short version 898 trainset 102 testset
# ratio = 9 which is exactly what is expected (0.9/0.1 = 9)
# short version, ratio = 8.803922

# Average model

mu <- mean(trainset$price)

# prediction; rating is expected to be average for all.
predictions <- rep(mu, nrow(testset))

# scatterplot prediction and actual
data.frame(predition = predictions, actual = testset$price) %>%
ggplot(aes(predition, actual)) +
geom_point()

# rmse from average
naive_rmse <- RMSE(predictions, testset$rating)

# storing the rmse to rmse_results
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

# table for rmse

rmse_results %>% kable()

# Since the calculation is slow even if smaller data, 
# parallel computing is implemented.


library(parallel)

detectCores()

# 4 cores

# caret package parallel computing

library(doParallel)


# start of parallel computing

rt_f <- makePSOCKcluster(4)
registerDoParallel(rt_f)

# Predictor using 

fit_rt <- train(price ~ ., method = "rpart",
                tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                data = trainset)

## When the calculation is finished.
stopCluster(rt_f)

# plot optimization

ggplot(fit_rt)

# predicting price 
y_hat_rt <- predict(fit_rt, testset)

# plot of prediction and actual price
data.frame(prediction = y_hat_rt, actual = testset$price) %>%
  ggplot(aes(prediction, actual)) +
  geom_point()

# calcuate rmse
rmse_rt <- RMSE(y_hat_rt, testset$price)

# store rmse
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="rpart",
                                     RMSE = rmse_rt ))

# print table for RMSE
rmse_results %>% kable()

# knn3 parameter optimization 

control <- trainControl(method = "cv", number = 10, p = .9)

# starting parallel computing

knn_f <- makePSOCKcluster(4)
registerDoParallel(knn_f)

# Predictor using knn3

fit_knn <- train(price ~ . , method = "knn", 
                 tuneGrid = data.frame(k = c(1,3,5,7)),
                 trControl = control,
                 data = trainset)

## When the calculation is finished.
stopCluster(knn_f)

ggplot(fit_knn)

# predicting price 
y_hat_knn <- predict(fit_knn, testset)

# plot of prediction and actual price
data.frame(prediction = y_hat_rt, actual = testset$price) %>%
  ggplot(aes(prediction, actual)) +
  geom_point()

# calculate rmse
rmse_knn <- RMSE(y_hat_knn, testset$price)

# store rmse
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="knn",
                                     RMSE = rmse_knn ))

# print table for RMSE

rmse_results %>% kable()

