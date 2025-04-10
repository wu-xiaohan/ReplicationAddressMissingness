# This file contains replication code for estimating transparency in administrative
# first instance cases from 2018 to 2022 in Chinese Courts
# using MUVE, Coarsened German tank estimator, and
# k-th largest German tank estimator.

# Dataset that can be processed using this file:
# data/court18-22/fulltext/admin_first.csv
# data/court18-22/allcases/admin_first.csv


# This code uses cases from the year 2018 as an example.

filetype <- "fulltext" # "fulltext" or "allcases"
filename <- paste0(
  "data/court18-22/", filetype, "/admin_first.csv"
)
output_filename <- paste0(
  "data/court18-22/", filetype, "/final_estimates/admin_first_transparency.csv"
)
ksfilename_mvue <- paste0(
  "data/court18-22/", filetype, "/final_estimates/admin_first_ks_test_mvue.csv"
)
ksfilename_coarsen <- paste0(
  "data/court18-22/", filetype, "/final_estimates/admin_first_ks_test_coarsen.csv"
)
# Load data
dataset <- read.csv(
  file = filename,
  header = TRUE,
  sep = ","
)

# Functions for transparency estimation
data_clean <- function(dataset) {
  #clean court subdivision
  dataset$court_division <- ifelse(
    "court_division" %in% colnames(dataset),
    dataset$court_division,
    NA
  )
  dataset$court_division <- "行初"
  
  dataset$court_id <- gsub(" ", "", dataset$court_id)
  dataset$court_id <- gsub("Ｏ", "0", dataset$court_id)
  dataset$court_id <- gsub("０", "0", dataset$court_id)
  dataset$court_id <- gsub("ｌ", "1", dataset$court_id)
  dataset$court_id <- gsub("１", "1", dataset$court_id)
  dataset$court_id <- gsub("２", "2", dataset$court_id)
  dataset$court_id <- gsub("３", "3", dataset$court_id)
  dataset$court_id <- gsub("４", "4", dataset$court_id)
  dataset$court_id <- gsub("５", "5", dataset$court_id)
  dataset$court_id <- gsub("６", "6", dataset$court_id)
  dataset$court_id <- gsub("７", "7", dataset$court_id)
  dataset$court_id <- gsub("８", "8", dataset$court_id)
  dataset$court_id <- gsub("９", "9", dataset$court_id)
  
  dataset$year_id <- gsub(' ','',dataset$year_id)
  dataset$year_id <- gsub('０','0',dataset$year_id)
  dataset$year_id <- gsub('１','1',dataset$year_id)
  dataset$year_id <- gsub('２','2',dataset$year_id)
  dataset$year_id <- gsub('３','3',dataset$year_id)
  dataset$year_id <- gsub('４','4',dataset$year_id)
  dataset$year_id <- gsub('５','5',dataset$year_id)
  dataset$year_id <- gsub('６','6',dataset$year_id)
  dataset$year_id <- gsub('７','7',dataset$year_id)
  dataset$year_id <- gsub('８','8',dataset$year_id)
  dataset$year_id <- gsub('９','9',dataset$year_id)
  dataset$year_id <- as.integer(dataset$year_id)
  
  # Clean case number
  dataset$case_number_id <- gsub(" ", "", dataset$case_number_id)
  dataset$case_number_id <- gsub("０", "0", dataset$case_number_id)
  dataset$case_number_id <- gsub("１", "1", dataset$case_number_id)
  dataset$case_number_id <- gsub("２", "2", dataset$case_number_id)
  dataset$case_number_id <- gsub("３", "3", dataset$case_number_id)
  dataset$case_number_id <- gsub("４", "4", dataset$case_number_id)
  dataset$case_number_id <- gsub("５", "5", dataset$case_number_id)
  dataset$case_number_id <- gsub("６", "6", dataset$case_number_id)
  dataset$case_number_id <- gsub("７", "7", dataset$case_number_id)
  dataset$case_number_id <- gsub("８", "8", dataset$case_number_id)
  dataset$case_number_id <- gsub("９", "9", dataset$case_number_id)
  # Remove cases with missing metadata
  dataset <- dataset[dataset$court_id != "", ] 
  dataset <- dataset[dataset$court_division != "", ]
  dataset <- dataset[dataset$case_number_id != "", ]
  dataset <- dataset[dataset$year_id != "", ]
  dataset= subset(dataset,year_id == 2018|year_id == 2019|year_id == 2020|year_id == 2021|year_id == 2022)
  #clean case serial number
  pattern = "\\d*"
  dataset$clean_case_number=regmatches(dataset$case_number_id, regexpr(pattern, dataset$case_number_id))
  pattern='0*'
  dataset$clean_case_number=sub(pattern,"",dataset$clean_case_number)
  dataset$clean_case_number = as.numeric(dataset$clean_case_number)
  dataset = dataset[!is.na(dataset$clean_case_number),]
  # Create division(batch) = year-court-court_division
  dataset$division <- paste(
    dataset$year_id, dataset$court_id, dataset$court_division, sep = " "
  )
  dataset$num_id <- as.numeric(as.factor(dataset$division))
  dataset <- dataset[!duplicated(dataset[, c("clean_case_number", "division")]), ] 
  # Further remove duplicates with the same case_id
  dataset$division <- as.factor(dataset$division)
  # Count how many cases in one division - save as case_in_division
  bydivision <- tapply(dataset$clean_case_number, dataset$division, length)
  iddata <- data.frame(
    division = names(bydivision), case_in_division = bydivision
  )
  dataset <- merge(dataset, iddata, all.x = TRUE, by = "division")
  return(dataset)
}

get_german_tank_solution <- function(dataset) {
  # found out the maximum case number for each batch and save as max_case_number
  bycase_number <- tapply(dataset$clean_case_number, dataset$division, max)
  iddata <- data.frame(
    division = names(bycase_number),
    max_case_number = bycase_number
  )
  dataset <- merge(dataset, iddata, by = "division")
  # Calculate mvue and bayesian german-tank estimation and confidence interval
  # for mvue method.
  dataset$mvue <- dataset$max_case_number + 
    dataset$max_case_number / dataset$case_in_division - 1
  dataset$bayesian <- ((dataset$case_in_division - 1) /
                         (dataset$case_in_division - 2) *
                         (dataset$max_case_number - 1))
  dataset$mvue_upper <- dataset$max_case_number *
    20^(1 / dataset$case_in_division)
  return(dataset)
}

get_coarsen_gts <- function(dataset, binsize) {
  dataset$coarsened_clean_case_number <- ceiling(dataset$clean_case_number / binsize)
  bycase_number <- tapply(dataset$coarsened_clean_case_number, dataset$division, max)
  iddata <- data.frame(division = names(bycase_number), max_case_number_coarsen = bycase_number)
  dataset <- merge(dataset, iddata, by = "division")
  bycase_number <- tapply(
    dataset$coarsened_clean_case_number,
    dataset$division,
    function(x) length(unique(x))
  )
  iddata <- data.frame(division = names(bycase_number), case_in_division_coarsen = bycase_number)
  dataset <- merge(dataset, iddata, by = "division")
  dataset$mvue_coarsen <- binsize * (
    dataset$max_case_number_coarsen +
      dataset$max_case_number_coarsen / dataset$case_in_division_coarsen - 1
  )
  dataset$mvue_upper_coarsen <- dataset$max_case_number_coarsen *
    20^(1 / dataset$case_in_division_coarsen) * binsize
  return(dataset)
}

clean_data <- data_clean(dataset)

# Load court information
court <- read.csv('data/courts.csv')

clean_data <- clean_data[clean_data$court_id %in% court$court_id, ]
court_number <- clean_data$court_id[!duplicated(clean_data$court_id)]
batch_number <- clean_data$division[!duplicated(clean_data$division)]
print(paste0("Number of distinct courts in civil dataset: ", length(court_number)))
print(paste0(
  "Number of distinct batches in civil dataset: ",
  length(batch_number)
))

# get german tank solutions
GTS <- get_german_tank_solution(clean_data)
GTS <- get_coarsen_gts(GTS, 10)

# uniformality test for MVUE
complete_division_list <- GTS[!duplicated(GTS$division), ]$division
GTS$KSpercentMVUE <- GTS$clean_case_number / GTS$max_case_number
kstest <- data.frame(division = c(), ks_p_value_mvue = c())

for (i in 1:length(complete_division_list)){
  if (i %% 100 == 0) {
    cat("Uniformity test Progress:", (i / length(complete_division_list)) * 100, "percent done.\n")
  }
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

write.csv(kstest, ksfilename_mvue)

GTS <- merge(GTS, kstest, by = "division")

# uniformity test for CoarsenedGTS
complete_division_list <- GTS[!duplicated(GTS$division), ]$division
GTS$KSpercentCoarsen <- GTS$coarsened_clean_case_number/GTS$max_case_number_coarsen
GTS_drop <- GTS[!duplicated(GTS[c("division", "coarsened_clean_case_number")]), ]
kstest <- data.frame(division = c(), ks_p_value_coarsen = c())

for (i in 1:length(complete_division_list)){
  if (i %% 100 == 0) {
    cat("Uniformity test Progress:", (i / length(complete_division_list)) * 100, "percent done.\n")
  }
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

write.csv(kstest, ksfilename_coarsen)

GTS <- merge(GTS, kstest, by = "division")

# Estimate K-th largest MVUE in eligible batches, up to 5th largest.
gts_temp <- subset(GTS, (case_in_division > 10) & (ks_p_value_mvue < 0.05))
gts_temp$average_gap <- (gts_temp$max_case_number - gts_temp$case_in_division) / gts_temp$case_in_division
gts_temp <- subset(gts_temp, average_gap > 3)

# Compute largest k-th case numbers
f <- function(x) {
  sort(x, TRUE)[2]
}
bycase_number <- tapply(gts_temp$clean_case_number, gts_temp$division, f)
iddata <- data.frame(division = names(bycase_number), largest2 = bycase_number)
gts_temp <- merge(gts_temp, iddata, by = "division")

f <- function(x) {
  sort(x, TRUE)[3]
}
bycase_number <- tapply(gts_temp$clean_case_number, gts_temp$division, f)
iddata <- data.frame(division = names(bycase_number), largest3 = bycase_number)
gts_temp <- merge(gts_temp, iddata, by = "division")

f <- function(x) {
  sort(x, TRUE)[4]
}
bycase_number <- tapply(gts_temp$clean_case_number, gts_temp$division, f)
iddata <- data.frame(division = names(bycase_number), largest4 = bycase_number)
gts_temp <- merge(gts_temp, iddata, by = "division")

f <- function(x) {
  sort(x, TRUE)[5]
}
bycase_number <- tapply(gts_temp$clean_case_number, gts_temp$division, f)
iddata <- data.frame(division = names(bycase_number), largest5 = bycase_number)
gts_temp <- merge(gts_temp, iddata, by = "division")

# Compute gaps
gts_temp$gap_1 <- gts_temp$max_case_number - gts_temp$largest2
gts_temp$gap_2 <- gts_temp$largest2 - gts_temp$largest3
gts_temp$gap_3 <- gts_temp$largest3 - gts_temp$largest4
gts_temp$gap_4 <- gts_temp$largest4 - gts_temp$largest5

# Decide how many observations to drop
gts_throw_4 <- subset(gts_temp, gap_4 > 10 * average_gap)
gts_throw_3 <- subset(
  gts_temp, 
  (gap_3 > 10 * average_gap) & (gap_4 < 10 * average_gap)
)
gts_throw_2 <- subset(
  gts_temp, 
  (gap_2 > 10 * average_gap) & 
    (gap_3 < 10 * average_gap) & 
    (gap_4 < 10 * average_gap)
)
gts_throw_1 <- subset(
  gts_temp, 
  (gap_1 > 10 * average_gap) & 
    (gap_2 < 10 * average_gap) & 
    (gap_3 < 10 * average_gap) & 
    (gap_4 < 10 * average_gap)
)

# Identify throw-away cases
gts_throw_4$throw <- 4
gts_throw_3$throw <- 3
gts_throw_2$throw <- 2
gts_throw_1$throw <- 1

gts_throw_4 <- subset(gts_throw_4, clean_case_number < largest5)
gts_throw_3 <- subset(gts_throw_3, clean_case_number < largest4)
gts_throw_2 <- subset(gts_throw_2, clean_case_number < largest3)
gts_throw_1 <- subset(gts_throw_1, clean_case_number < largest2)

# Estimate k-th largest GTS and its confidence interval
gts_throw_4$mvuekth <- gts_throw_4$largest5 * (gts_throw_4$case_in_division + 1) / 
  (gts_throw_4$case_in_division - 4) - 1
gts_throw_4$mvuekth_lower <- gts_throw_4$largest5 + 4
gts_throw_4$mvuekth_upper <- gts_throw_4$largest5 * 20 ^ 
  (1 / ((gts_throw_4$case_in_division - 4) / (4 + 1)))
gts_throw_4$largest <- gts_throw_4$largest5

gts_throw_3$mvuekth <- gts_throw_3$largest4 * (gts_throw_3$case_in_division + 1) / 
  (gts_throw_3$case_in_division - 3) - 1
gts_throw_3$mvuekth_lower <- gts_throw_3$largest4 + 3
gts_throw_3$mvuekth_upper <- gts_throw_3$largest4 * 20 ^ 
  (1 / ((gts_throw_3$case_in_division - 3) / (3 + 1)))
gts_throw_3$largest <- gts_throw_3$largest4

gts_throw_2$mvuekth <- gts_throw_2$largest3 * (gts_throw_2$case_in_division + 1) / 
  (gts_throw_2$case_in_division - 2) - 1
gts_throw_2$mvuekth_lower <- gts_throw_2$largest3 + 2
gts_throw_2$mvuekth_upper <- gts_throw_2$largest3 * 20 ^ 
  (1 / ((gts_throw_2$case_in_division - 2) / (2 + 1)))
gts_throw_2$largest <- gts_throw_2$largest3

gts_throw_1$mvuekth <- gts_throw_1$largest2 * (gts_throw_1$case_in_division + 1) / 
  (gts_throw_1$case_in_division - 1) - 1
gts_throw_1$mvuekth_lower <- gts_throw_1$largest2 + 1
gts_throw_1$mvuekth_upper <- gts_throw_1$largest2 * 20 ^ 
  (1 / ((gts_throw_1$case_in_division - 1) / (1 + 1)))
gts_throw_1$largest <- gts_throw_1$largest2

gts_throw <- rbind(gts_throw_1, gts_throw_2, gts_throw_3, gts_throw_4)

# uniformalty test for kth largest
complete_division_list <- gts_throw[!duplicated(gts_throw$division), ]$division
gts_throw$KSpercentkth <- gts_throw$clean_case_number / gts_throw$largest
kstest <- data.frame(division = c(), ks_p_value_kth = c())

for (i in 1:length(complete_division_list)){
  if (i %% 100 == 0) {
    cat("Uniformity test Progress:", (i / length(complete_division_list)) * 100, "percent done.\n")
  }
  target_batch <- subset(gts_throw, division == complete_division_list[i])
  target_batch <- subset(target_batch, KSpercentkth != 1)
  if (length(target_batch$division) == 0) {
    temp <- data.frame(division = complete_division_list[i], ks_p_value_kth = 0)
  } else {
    ks <- ks.test(target_batch$KSpercentkth, "punif", 0, 1)
    temp <- data.frame(division = complete_division_list[i], ks_p_value_kth = ks[["p.value"]])
  }
  kstest <- rbind(kstest, temp)
}

gts_throw <- merge(gts_throw, kstest, by = "division")

# Merge kth largest estimation
gts_throw <- subset(gts_throw, select = c(division, throw, mvuekth, mvuekth_lower, mvuekth_upper, ks_p_value_kth))
transparency <- GTS[!duplicated(GTS$division), ]
gts_throw <- gts_throw[!duplicated(gts_throw$division), ]
transparency <- merge(transparency, gts_throw, all.x = TRUE, on = "divisions")
transparency <- subset(
  transparency,
  select = c(
    division, year_id, court_division, case_in_division, court_id,
    max_case_number, mvue, bayesian, mvue_upper, coarsened_clean_case_number,
    max_case_number_coarsen, case_in_division_coarsen, mvue_coarsen,
    mvue_upper_coarsen, ks_p_value_mvue, ks_p_value_coarsen, throw,
    mvuekth, mvuekth_lower, mvuekth_upper, ks_p_value_kth
  )
)

write.csv(transparency, output_filename)

