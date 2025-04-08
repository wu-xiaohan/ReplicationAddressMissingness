#functions for transparency estimation: 
# dataClean();getGermanTankSolution();getCoarsenGTS();

dataClean <- function(dataset) {
  dataset$clean_case_number <- substr(dataset$cino, start = 7, stop = 12)
  dataset$clean_case_number <- as.numeric(dataset$clean_case_number)
  
  # Remove cases with missing metadata
  dataset <- dataset[dataset$state_code != "", ] 
  dataset <- dataset[dataset$dist_code != "", ]
  dataset <- dataset[dataset$court_no != "", ]
  dataset <- dataset[dataset$clean_case_number != "", ]
  dataset <- dataset[!is.na(dataset$clean_case_number), ]
  dataset <- dataset[!is.na(dataset$state_code), ]
  dataset <- dataset[!is.na(dataset$dist_code), ]
  dataset <- dataset[!is.na(dataset$court_no), ]
  
  # Create batch (division) = year-court-court_division
  dataset$division <- paste(dataset$year, dataset$state_code, dataset$dist_code, dataset$court_no, sep = "-")
  dataset$num_id <- as.numeric(as.factor(dataset$division))
  dataset$division <- as.factor(dataset$division)
  
  # Count how many cases in one batch - save as case_in_division
  bydivision <- tapply(dataset$clean_case_number, dataset$division, length)
  iddata <- data.frame(division = names(bydivision), case_in_division = bydivision)
  dataset <- merge(dataset, iddata, all.x = TRUE, by = "division")
  
  return(dataset)
}

getGermanTankSolution <- function(dataset){
  # found out the maximum case number for each batch and save as max_case_number
  bycase_number <- tapply(dataset$clean_case_number, dataset$division, max)
  iddata <- data.frame(division = names(bycase_number), max_case_number = bycase_number)
  dataset <- merge(dataset, iddata, by = "division")
  
  # Calculate mvue and bayesian german-tank estimation and confidence interval for mvue method.
  dataset$mvue <- dataset$max_case_number + dataset$max_case_number / dataset$case_in_division - 1
  dataset$bayesian <- ((dataset$case_in_division - 1) / (dataset$case_in_division - 2) * (dataset$max_case_number - 1))
  dataset$mvue_upper <- dataset$max_case_number * 20^(1 / dataset$case_in_division)
  return(dataset)
}

getCoarsenGTS <- function(dataset, binsize){
  dataset$coarsened_clean_case_number <- ceiling(dataset$clean_case_number / binsize)
  bycase_number <- tapply(dataset$coarsened_clean_case_number, dataset$division, max)
  iddata <- data.frame(division = names(bycase_number), max_case_number_coarsen = bycase_number)
  dataset <- merge(dataset, iddata, by = "division")
  
  bycase_number <- tapply(dataset$coarsened_clean_case_number, dataset$division, function(x) length(unique(x)))
  iddata <- data.frame(division = names(bycase_number), case_in_division_coarsen = bycase_number)
  dataset <- merge(dataset, iddata, by = "division")
  
  dataset$mvue_coarsen <- binsize * (dataset$max_case_number_coarsen + dataset$max_case_number_coarsen / dataset$case_in_division_coarsen - 1)
  dataset$mvue_upper_coarsen <- dataset$max_case_number_coarsen * 20^(1 / dataset$case_in_division_coarsen) * binsize
  return(dataset)
}
