# This file organizes the indian court transparency and produce Figure 29.
# Run Indian_transparency_estimation.R and Indian_PI_simulation.R to obtain relevant files before running this document.
# Load transparency estimates and prediction interval simulation.
# This code use year 2018 as an example. 

india_data <- read.csv(file = "Indian2018_transparency.csv") 
load("Indian_Sim_18.Rdata")

india_data$est_total <- NA
india_data$est_total <- ifelse((india_data$ks_p_value_mvue >= 0.05), india_data$mvue, india_data$est_total)
india_data$est_total <- ifelse((is.na(india_data$est_total) & (india_data$ks_p_value_kth >= 0.05)), india_data$mvuekth, india_data$est_total)
india_data$est_total <- ifelse((is.na(india_data$est_total) & (india_data$ks_p_value_coarsen >= 0.05)), india_data$mvue_coarsen, india_data$est_total)
india_data$reliable <- ifelse(is.na(india_data$est_total), 0, 1)
india_data$est_total <- ifelse((is.na(india_data$est_total) & (!is.na(india_data$mvuekth))), india_data$mvuekth, india_data$est_total)
india_data$est_total <- ifelse((is.na(india_data$est_total)), india_data$mvue, india_data$est_total)
india_data$availability <- india_data$case_in_division / india_data$est_total

india_data$method <- NA
india_data$method <- ifelse((india_data$ks_p_value_mvue >= 0.05), "mvue", india_data$method)
india_data$method <- ifelse((is.na(india_data$method) & (india_data$ks_p_value_kth >= 0.05)), "mvuekth", india_data$method)
india_data$method <- ifelse((is.na(india_data$method) & (india_data$ks_p_value_coarsen >= 0.05)), "mvue_coarsen", india_data$method)
india_data$method <- ifelse((is.na(india_data$method) & (!is.na(india_data$mvuekth))), "mvuekth", india_data$method)
india_data$method <- ifelse((is.na(india_data$method)), "mvue", india_data$method)

india_data$conservative_maximum <- ifelse(india_data$method == 'mvuekth', india_data$mvuekth_lower, india_data$max_case_number) # when using kth method, the conservative maximum is defined as k-th largest.
india_data$conservative_upper_bound <- india_data$case_in_division / india_data$conservative_maximum # for each batch, calculate the upper bound of availability.
india_data <- cbind(indian_res, india_data)

# calculating transparency estimates and aggregated 95% prediction intervals. 
india_calc <- subset(india_data, reliable == 1)
est <- colSums(admin_calc[, 1:5000]) / length(admin_calc$division)
print(round(quantile(est, c(0.05, 1))[1], 4)) # Lower bound 
print(round(mean(admin_calc$conservative_upper_bound), 4)) # Upper bound
print(round(mean(admin_calc$availability), 4)) # availability estimation

# Save the aggregated yearly transparency and save it to csv file called Indian_transparency_over_time.csv
# Create Figure 29
library(ggplot2)
Indian_overtime <- read_csv('Data/Indian_transparency_over_time.csv')
geom_point(size = 4) + geom_line() +
  geom_errorbar(aes(ymin = trans_overtime$Lower, ymax = trans_overtime$Upper), alpha = 1, linetype = 1, width = .2, position = position_dodge(0)) +
  labs(#title = "Indian Court Transparency from 2010 to 2018 with Prediction Intervals",
    x = "Year",
    y = "Transparency"
  ) + theme_bw() + theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.ticks.length = unit(.2, "cm"),
    axis.text.x = element_text(size = 18, angle = 0),
    axis.text.y = element_text(size = 18, angle = 0)) + geom_abline(intercept = 0, slope = 1, color = "black", size = 0.5) + theme(
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 18))
p
ggsave("Figure29_Indian_Transparency.png", plot = p, width = 10, height = 6, dpi = 300)
