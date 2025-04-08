# This file contains replication code for estimating transparency in Indian Courts using 
# MUVE, Coarsened German tank estimator, and k-th largest German tank estimator.
# To run transparency estimates for Indian courts, download the Indian Court data from:
# https://www.devdatalab.org/judicial-data
# This code uses cases from the year 2018 as an example.
# Load functions from Indian_transparency_functions.R before running this file.

library(lmtest)

# Load dataset
dataset <- read.csv(file = '/Users/wuxiaohan/Dropbox/Research/TTD/ttd-Reorganize/ttd_replication/Indian/cases/cases_2010.csv', header = TRUE, sep = ',')

# Data cleaning
cleanData <- dataClean(dataset)

# Compute German Tank Solutions and Coarsen German Tank Solution
GTS <- getGermanTankSolution(cleanData)
GTS <- getCoarsenGTS(GTS, 50)

# Uniformality test for MVUE
complete_division_list <- GTS[!duplicated(GTS$division), ]$division
GTS$KSpercentMVUE <- GTS$clean_case_number / GTS$max_case_number
kstest <- data.frame(division = c(), ks_p_value_mvue = c())

for (i in 1:length(complete_division_list)) {
  if (i %% 100 == 0) { print(i) }
  target_batch <- subset(GTS, division == complete_division_list[i])
  target_batch <- subset(target_batch, KSpercentMVUE != 1)
  if (length(target_batch$division) == 0) {
    temp <- data.frame(division = complete_division_list[i], ks_p_value_mvue = 0)
  } else {
    ks <- ks.test(target_batch$KSpercentMVUE, "punif", 0, 1)
    temp <- data.frame(division = complete_division_list[i], ks_p_value_mvue = ks[["p.value"]])
  }
  kstest <- rbind(kstest, temp)
}

GTS$ks_p_value_mvue <- GTS$ks_p_value_mvue

GTS <- merge(GTS, kstest, by = 'division')

# Uniformity test for Coarsened GTS
complete_division_list <- GTS[!duplicated(GTS$division), ]$division
GTS$KSpercentCoarsen <- GTS$coarsened_clean_case_number / GTS$max_case_number_coarsen
GTS_drop <- GTS[!duplicated(GTS[c("division", "coarsened_clean_case_number")]), ]
kstest <- data.frame(division = c(), ks_p_value_coarsen = c())

for (i in 1:length(complete_division_list)) {
  if (i %% 100 == 0) { print(i) }
  target_batch <- subset(GTS_drop, division == complete_division_list[i])
  target_batch <- subset(target_batch, KSpercentCoarsen != 1)
  if (length(target_batch$division) == 0) {
    temp <- data.frame(division = complete_division_list[i], ks_p_value_coarsen = 0)
  } else {
    ks <- ks.test(target_batch$KSpercentCoarsen, "punif", 0, 1)
    temp <- data.frame(division = complete_division_list[i], ks_p_value_coarsen = ks[["p.value"]])
  }
  kstest <- rbind(kstest, temp)
}

GTS <- merge(GTS, kstest, by = 'division')

# Estimate K-th largest MVUE in eligible batches, up to 5th largest.
GTS_temp <- subset(GTS, (case_in_division > 10) & (ks_p_value_mvue < 0.05))
GTS_temp$average_gap <- (GTS_temp$max_case_number - GTS_temp$case_in_division) / GTS_temp$case_in_division
GTS_temp <- subset(GTS_temp, average_gap > 3)

# Compute largest k-th case numbers
f <- function(x) { sort(x, TRUE)[2] }
bycase_number <- tapply(GTS_temp$clean_case_number, GTS_temp$division, f)
iddata <- data.frame(division = names(bycase_number), largest2 = bycase_number)
GTS_temp <- merge(GTS_temp, iddata, by = "division")

f <- function(x) { sort(x, TRUE)[3] }
bycase_number <- tapply(GTS_temp$clean_case_number, GTS_temp$division, f)
iddata <- data.frame(division = names(bycase_number), largest3 = bycase_number)
GTS_temp <- merge(GTS_temp, iddata, by = "division")

f <- function(x) { sort(x, TRUE)[4] }
bycase_number <- tapply(GTS_temp$clean_case_number, GTS_temp$division, f)
iddata <- data.frame(division = names(bycase_number), largest4 = bycase_number)
GTS_temp <- merge(GTS_temp, iddata, by = "division")

f <- function(x) { sort(x, TRUE)[5] }
bycase_number <- tapply(GTS_temp$clean_case_number, GTS_temp$division, f)
iddata <- data.frame(division = names(bycase_number), largest5 = bycase_number)
GTS_temp <- merge(GTS_temp, iddata, by = "division")

# Compute gaps
GTS_temp$gap1 <- GTS_temp$max_case_number - GTS_temp$largest2
GTS_temp$gap2 <- GTS_temp$largest2 - GTS_temp$largest3
GTS_temp$gap3 <- GTS_temp$largest3 - GTS_temp$largest4
GTS_temp$gap4 <- GTS_temp$largest4 - GTS_temp$largest5

# Decide how many observations to drop
GTS_throw4 <- subset(GTS_temp, gap4 > 10 * average_gap)
GTS_throw3 <- subset(GTS_temp, (gap3 > 10 * average_gap) & (gap4 < 10 * average_gap))
GTS_throw2 <- subset(GTS_temp, (gap2 > 10 * average_gap) & (gap3 < 10 * average_gap) & (gap4 < 10 * average_gap))
GTS_throw1 <- subset(GTS_temp, (gap1 > 10 * average_gap) & (gap2 < 10 * average_gap) & (gap3 < 10 * average_gap) & (gap4 < 10 * average_gap))

# Identify throw-away cases
GTS_throw4$throw <- 4
GTS_throw3$throw <- 3
GTS_throw2$throw <- 2
GTS_throw1$throw <- 1

GTS_throw4 <- subset(GTS_throw4, clean_case_number < largest5)
GTS_throw3 <- subset(GTS_throw3, clean_case_number < largest4)
GTS_throw2 <- subset(GTS_throw2, clean_case_number < largest3)
GTS_throw1 <- subset(GTS_throw1, clean_case_number < largest2)

# Estimate k-th largest GTS and its confidence interval
GTS_throw4$mvuekth <- GTS_throw4$largest5 * (GTS_throw4$case_in_division + 1) / (GTS_throw4$case_in_division - 4) - 1
GTS_throw4$mvuekth_lower <- GTS_throw4$largest5 + 4
GTS_throw4$mvuekth_upper <- GTS_throw4$largest5 * 20^(1 / ((GTS_throw4$case_in_division - 4) / (4 + 1)))
GTS_throw4$largest <- GTS_throw4$largest5

GTS_throw3$mvuekth <- GTS_throw3$largest4 * (GTS_throw3$case_in_division + 1) / (GTS_throw3$case_in_division - 3) - 1
GTS_throw3$mvuekth_lower <- GTS_throw3$largest4 + 3
GTS_throw3$mvuekth_upper <- GTS_throw3$largest4 * 20^(1 / ((GTS_throw3$case_in_division - 3) / (3 + 1)))
GTS_throw3$largest <- GTS_throw3$largest4

GTS_throw2$mvuekth <- GTS_throw2$largest3 * (GTS_throw2$case_in_division + 1) / (GTS_throw2$case_in_division - 2) - 1
GTS_throw2$mvuekth_lower <- GTS_throw2$largest3 + 2
GTS_throw2$mvuekth_upper <- GTS_throw2$largest3 * 20^(1 / ((GTS_throw2$case_in_division - 2) / (2 + 1)))
GTS_throw2$largest <- GTS_throw2$largest3

GTS_throw1$mvuekth <- GTS_throw1$largest2 * (GTS_throw1$case_in_division + 1) / (GTS_throw1$case_in_division - 1) - 1
GTS_throw1$mvuekth_lower <- GTS_throw1$largest2 + 1
GTS_throw1$mvuekth_upper <- GTS_throw1$largest2 * 20^(1 / ((GTS_throw1$case_in_division - 1) / (1 + 1)))
GTS_throw1$largest <- GTS_throw1$largest2

GTS_throw <- rbind(GTS_throw1, GTS_throw2, GTS_throw3, GTS_throw4)

# uniformalty test for kth largest
complete_division_list <- GTS_throw[!duplicated(GTS_throw$division), ]$division
GTS_throw$KSpercentkth <- GTS_throw$clean_case_number / GTS_throw$largest
kstest <- data.frame(division = c(), ks_p_value_kth = c())

for (i in 1:length(complete_division_list)) {
  if (i %% 100 == 0) { print(i) }
  target_batch <- subset(GTS_throw, division == complete_division_list[i])
  target_batch <- subset(target_batch, KSpercentkth != 1)
  if (length(target_batch$division) == 0) {
    temp <- data.frame(division = complete_division_list[i], ks_p_value_kth = 0)
  } else {
    ks <- ks.test(target_batch$KSpercentkth, "punif", 0, 1)
    temp <- data.frame(division = complete_division_list[i], ks_p_value_kth = ks[["p.value"]])
  }
  kstest <- rbind(kstest, temp)
}

GTS_throw <- merge(GTS_throw, kstest, by = 'division')

GTS_throw <- subset(GTS_throw, select = c(division, throw, mvuekth, mvuekth_lower, mvuekth_upper, ks_p_value_kth))

GTS_throw <- GTS_throw[!duplicated(GTS$division), ]
transparency <- GTS[!duplicated(GTS$division), ]

transparency <- merge(transparency, GTS_throw, all.x = TRUE, on = 'divisions')
transparency <- subset(transparency, select = c(division, year, case_in_division, state_code, dist_code, court_no, max_case_number, mvue, bayesian, mvue_upper, coarsened_clean_case_number, max_case_number_coarsen, case_in_division_coarsen, mvue_coarsen, mvue_upper_coarsen, ks_p_value_mvue, ks_p_value_coarsen, throw, mvuekth, mvuekth_lower, mvuekth_upper, ks_p_value_kth))

write.csv(transparency, 'Indian2018_transparency.csv')
