# This file contains code for obtain simulated aggregate prediction interval for Indian court transparency.
# This code use data from year 2018 as an example.

library(MASS)

# loading data
indian <- read.csv(file = 'Indian2018_transparency.csv') 

# Function for sampling from the posterior distribution of the total number of cases 
# Given largest serial number m and number of available cases k. (Assume k cases were draw with replacement)  
simu_mvue1 <- function(m,k){
  inverse_cdf <- function(x) m*(x)^(-1/k)
  X <- inverse_cdf(runif(5000))
  return(X)
} 
# Function for sampling from the posterior distribution of the total number of cases 
# Given largest serial number m and number of available cases k. (Assume k cases were draw without replacement)  
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

# obtain estimated total, availability and adopted method (methods are adopted using the decision tree in Figure 21.)
indian$est_total <- NA
indian$est_total <- ifelse((indian$ks_p_value_mvue>=0.05),indian$mvue,indian$est_total)
indian$est_total <- ifelse((is.na(indian$est_total)&(indian$ks_p_value_kth>=0.05)),indian$mvuekth,indian$est_total)
indian$est_total <- ifelse((is.na(indian$est_total)&(indian$ks_p_value_coarsen>=0.05)),indian$mvue_coarsen,indian$est_total)
indian$reliable <- ifelse(is.na(indian$est_total),0,1)
indian$est_total <- ifelse((is.na(indian$est_total)&(!is.na(indian$mvuekth))),indian$mvuekth,indian$est_total)
indian$est_total <- ifelse((is.na(indian$est_total)),indian$mvue,indian$est_total)
indian$availability <- indian$case_in_division/indian$est_total

indian$method <- NA
indian$method <- ifelse((indian$ks_p_value_mvue>=0.05),"mvue",indian$method)
indian$method <- ifelse((is.na(indian$method)&(indian$ks_p_value_kth>=0.05)),"mvuekth",indian$method)
indian$method <- ifelse((is.na(indian$method)&(indian$ks_p_value_coarsen>=0.05)),"mvue_coarsen",indian$method)
indian$method <- ifelse((is.na(indian$method)&(!is.na(indian$mvuekth))),"mvuekth",indian$method)
indian$method <- ifelse((is.na(indian$method)),"mvue",indian$method)


# simulation
data <- indian
indian_res <- matrix(0,length(data$division),5000)
## obtain 5000 simulation results for german tank methods. ----
for (i in 1:length(data$division)){
  print(i)
  if ((data$method[i] == "mvue")&(data$case_in_division[i] == 1)){
    indian_res[i,1:5000] <- data$case_in_division[i]/simu_mvue1(data$max_case_number[i],data$case_in_division[i])
  } else if ((data$method[i] == "mvue")&(data$case_in_division[i] > 1)){
    indian_res[i,1:5000] <- data$case_in_division[i]/simu_mvue2(data$max_case_number[i],data$case_in_division[i])
  } else if ((data$method[i] == "mvuekth")&(data$case_in_division[i]-data$throw[i] == 1)){ # Assmue draw with replacement when there is only one case.
    indian_res[i,1:5000] <- data$case_in_division[i]/simu_mvue1(data$mvuekth_lower[i]-data$throw[i],(data$case_in_division[i]-data$throw[i])/(data$throw[i]+1))
  } else if ((data$method[i] == "mvuekth")&(data$case_in_division[i]-data$throw[i] > 1)){
    indian_res[i,1:5000] <- data$case_in_division[i]/simu_mvue2(data$mvuekth_lower[i]-data$throw[i],(data$case_in_division[i]-data$throw[i])/(data$throw[i]+1))
  } else if ((data$method[i] == "mvue_coarsen")&(data$case_in_division_coarsen[i] == 1)){
    indian_res[i,1:5000] <- data$case_in_division[i]/(simu_mvue1(data$max_case_number_coarsen[i],data$case_in_division_coarsen[I])*50)
  } else if ((data$method[i] == "mvue_coarsen")&(data$case_in_division_coarsen[i] > 1)){
    indian_res[i,1:5000] <- data$case_in_division[i]/(simu_mvue2(data$max_case_number_coarsen[i],data$case_in_division_coarsen[I])*50)
  }
}

# Save simulation results.

save(indian_res,file = 'Indian_Sim_18.Rdata')
