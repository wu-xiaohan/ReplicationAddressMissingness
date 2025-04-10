# This file contains code for obtain simulated aggregate prediction interval 
# for Chinese court transparency.
# This code use data from year 2018 as an example.

# Dataset that can be processed using this file:
# data/court18-22/allcases/final_estimates/admin_first_transparency.csv
# data/court18-22/allcases/final_estimates/input_data_first_transparency.csv
# data/court18-22/allcases/final_estimates/civil_first_2018_transparency.csv
# data/court18-22/allcases/final_estimates/civil_first_2019_transparency.csv
# data/court18-22/allcases/final_estimates/civil_first_2020_transparency.csv
# data/court18-22/allcases/final_estimates/civil_first_2021_transparency.csv
# data/court18-22/allcases/final_estimates/civil_first_2022_transparency.csv
# data/court18-22/fulltext/final_estimates/admin_first_transparency.csv
# data/court18-22/fulltext/final_estimates/input_data_first_transparency.csv
# data/court18-22/fulltext/final_estimates/civil_first_2018_transparency.csv
# data/court18-22/fulltext/final_estimates/civil_first_2019_transparency.csv
# data/court18-22/fulltext/final_estimates/civil_first_2020_transparency.csv
# data/court18-22/fulltext/final_estimates/civil_first_2021_transparency.csv
# data/court18-22/fulltext/final_estimates/civil_first_2022_transparency.csv



library(MASS)


# loading data
filetype <- 'fulltext' # 'allcases' or 'fulltext'
casetype <- 'admin' # 'admin' or 'civil' or 'criminal'
remaining_filename <- 'first_transparency.csv'
input_filename <- paste0("data/court18-22/", filetype, "/final_estimates/", casetype, "_",remaining_filename)
print(input_filename)
input_data <- read.csv(file = input_filename) 
remaining_filename <- 'SIM.Rdata'
output_filename <- paste0("data/court18-22/", filetype, "/simulation/PI_", casetype,"_",remaining_filename )
bin_size <- 10 # 10 for admin and criminal cases, 100 for civil cases

# function for sampling from the posterior distribution of the total number of cases 
# given largest serial number m and number of available cases k. (Assume k cases were draw with replacement)  
simu_mvue1 <- function(m,k){
  inverse_cdf <- function(x) m*(x)^(-1/k)
  X <- inverse_cdf(runif(5000))
  return(X)
} 
# function for sampling from the posterior distribution of the total number of cases 
# given largest serial number m and number of available cases k. (Assume k cases were draw without replacement)  
simu_mvue2 <- function(m,k){
  N_cdf <- function(x) ifelse(x >= m,1-choose(m-1,k-1)/choose(x,k-1),0)
  N_cdf <- Vectorize(N_cdf)
  N_cdf_inv <- function(p){
    sapply(p, function(.x) uniroot(
      function(p) N_cdf(p) - .x, 
      interval = c(0, 1e16) 
    )[["root"]])
  }
  X <- try(N_cdf_inv(runif(5000)), silent = TRUE)
  # when choose function returns inf, use the following function for an approximation.
  if (inherits(X,"try-error")){
    inverse_cdf <- function(x) m*(x)^(-1/k)
    X <- inverse_cdf(runif(5000))
  }
  return(X)
}




# input_data -----
input_data$est_total <- NA
input_data$est_total <- ifelse((input_data$ks_p_value_mvue >= 0.05), input_data$mvue, input_data$est_total)
input_data$est_total <- ifelse((is.na(input_data$est_total) & (input_data$ks_p_value_kth >= 0.05)), input_data$mvuekth, input_data$est_total)
input_data$est_total <- ifelse((is.na(input_data$est_total) & (input_data$ks_p_value_coarsen >= 0.05)), input_data$mvue_coarsen, input_data$est_total)
input_data$reliable <- ifelse(is.na(input_data$est_total), 0, 1)
input_data$est_total <- ifelse((is.na(input_data$est_total) & (!is.na(input_data$mvuekth))), input_data$mvuekth, input_data$est_total)
input_data$est_total <- ifelse((is.na(input_data$est_total)), input_data$mvue, input_data$est_total)
input_data <- subset(input_data, est_total < 30000) # thrown out impossible cases
input_data$availability <- input_data$case_in_division / input_data$est_total

input_data$method <- NA
input_data$method <- ifelse((input_data$ks_p_value_mvue >= 0.05), "mvue", input_data$method)
input_data$method <- ifelse((is.na(input_data$method) & (input_data$ks_p_value_kth >= 0.05)), "mvuekth", input_data$method)
input_data$method <- ifelse((is.na(input_data$method) & (input_data$ks_p_value_coarsen >= 0.05)), "mvue_coarsen", input_data$method)
input_data$method <- ifelse((is.na(input_data$method) & (!is.na(input_data$mvuekth))), "mvuekth", input_data$method)
input_data$method <- ifelse((is.na(input_data$method)), "mvue", input_data$method)

data <- input_data
input_data_res <- matrix(0, length(data$division), 5000)
## obtain 5000 simulation results for german tank methods. ----
for (i in 1:length(data$division)){
  print(i)
  if (i %% 100 == 0) {
    cat("Uniformity test Progress:", (i / length(data$division)) * 100, "percent done.\n")
  }
  if ((data$method[i] == "mvue")&(data$case_in_division[i] == 1)){
    input_data_res[i,1:5000] <- data$case_in_division[i]/simu_mvue1(data$max_case_number[i],data$case_in_division[i])
  } else if ((data$method[i] == "mvue")&(data$case_in_division[i] > 1)){
    input_data_res[i,1:5000] <- data$case_in_division[i]/simu_mvue2(data$max_case_number[i],data$case_in_division[i])
  } else if ((data$method[i] == "mvuekth")&(data$case_in_division[i]-data$throw[i] == 1)){ # Assmue draw with replacement when there is only one case.
    input_data_res[i,1:5000] <- data$case_in_division[i]/simu_mvue1(data$mvuekth_lower[i]-data$throw[i],(data$case_in_division[i]-data$throw[i])/(data$throw[i]+1))
  } else if ((data$method[i] == "mvuekth")&(data$case_in_division[i]-data$throw[i] > 1)){
    input_data_res[i,1:5000] <- data$case_in_division[i]/simu_mvue2(data$mvuekth_lower[i]-data$throw[i],(data$case_in_division[i]-data$throw[i])/(data$throw[i]+1))
  } else if ((data$method[i] == "mvue_coarsen")&(data$case_in_division_coarsen[i] == 1)){
    input_data_res[i,1:5000] <- data$case_in_division[i]/(simu_mvue1(data$max_case_number_coarsen[i],data$case_in_division_coarsen[i])*bin_size)
  } else if ((data$method[i] == "mvue_coarsen")&(data$case_in_division_coarsen[i] > 1)){
    input_data_res[i,1:5000] <- data$case_in_division[i]/(simu_mvue2(data$max_case_number_coarsen[i],data$case_in_division_coarsen[i])*bin_size)
  }
}

save(input_data_res, file = output_filename)



