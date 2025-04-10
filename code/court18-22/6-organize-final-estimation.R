# This script calculate aggregated transparency and prediction intervals

# Load court data
court = read.csv('data/courts.csv')

# administrative ----

admin = read.csv(file = 'data/court18-22/fulltext/final_estimates/admin_transparency_final.csv') 
load("data/court18-22/fulltext/simulation/PI_admin_SIM.Rdata")

admin$conservative_maximum = ifelse(admin$method ==	'mvuekth',admin$mvuekth_lower, admin$max_case_number) # when using kth method, the conservative maximum is defined as k-th largest.
admin$conservative_upper_bound = admin$case_in_division/admin$conservative_maximum # for each batch, calculate the upper bound of availability.
admin = cbind(input_data_res,admin)
admin = merge(admin,court, all.x = TRUE, by = 'court_id')

admin_calc = subset(admin,(reliable == 1)&(level == 4)&(year_id == 2018))
est = colSums(admin_calc[,2:5001])/length(admin_calc$division)
print(round(quantile(est, c(0.05,1))[1],4)) 
print(round(mean(admin_calc$conservative_upper_bound),4)) 
print(round(mean(admin_calc$availability),4))

admin_calc = subset(admin,(reliable == 1)&(level == 4)&(year_id == 2019)) 
est = colSums(admin_calc[,2:5001])/length(admin_calc$division)
print(round(quantile(est, c(0.05,1))[1],4)) 
print(round(mean(admin_calc$conservative_upper_bound),4))
print(round(mean(admin_calc$availability),4))

admin_calc = subset(admin,(reliable == 1)&(level == 4)&(year_id == 2020)) 
est = colSums(admin_calc[,2:5001])/length(admin_calc$division)
print(round(quantile(est, c(0.05,1))[1],4)) 
print(round(mean(admin_calc$conservative_upper_bound),4)) 
print(round(mean(admin_calc$availability),4))

admin_calc = subset(admin,(reliable == 1)&(level == 4)&(year_id == 2021)) 
est = colSums(admin_calc[,2:5001])/length(admin_calc$division)
print(round(quantile(est, c(0.05,1))[1],4)) 
print(round(mean(admin_calc$conservative_upper_bound),4)) 
print(round(mean(admin_calc$availability),4)) 

admin_calc = subset(admin,(reliable == 1)&(level == 4)&(year_id == 2022)) 
est = colSums(admin_calc[,2:5001])/length(admin_calc$division)
print(round(quantile(est, c(0.05,1))[1],4)) 
print(round(mean(admin_calc$conservative_upper_bound),4)) 
print(round(mean(admin_calc$availability),4))

# criminal -----
criminal = read.csv(file = 'data/court18-22/fulltext/final_estimates/criminal_transparency_final.csv') 
load("data/court18-22/fulltext/simulation/PI_criminal_SIM.Rdata")

criminal$conservative_maximum = ifelse(criminal$method ==	'mvuekth',criminal$mvuekth_lower, criminal$max_case_number) # when using kth method, the conservative maximum is defined as k-th largest.
criminal$conservative_upper_bound = criminal$case_in_division/criminal$conservative_maximum # for each batch, calculate the upper bound of availability.
criminal = cbind(input_data_res,criminal)
criminal = merge(criminal,court, all.x = TRUE, by = 'court_id')

criminal_calc = subset(criminal,(reliable == 1)&(level == 4)&(year_id == 2018)) 
est = colSums(criminal_calc[,2:5001])/length(criminal_calc$division)
print(round(quantile(est, c(0.05,1))[1],4)) 
print(round(mean(criminal_calc$conservative_upper_bound),4)) 
print(round(mean(criminal_calc$availability),4))


criminal_calc = subset(criminal,(reliable == 1)&(level == 4)&(year_id == 2019)) 
est = colSums(criminal_calc[,2:5001])/length(criminal_calc$division)
print(round(quantile(est, c(0.05,1))[1],4)) 
print(round(mean(criminal_calc$conservative_upper_bound),4)) 
print(round(mean(criminal_calc$availability),4))

criminal_calc = subset(criminal,(reliable == 1)&(level == 4)&(year_id == 2020)) 
est = colSums(criminal_calc[,2:5001])/length(criminal_calc$division)
print(round(quantile(est, c(0.05,1))[1],4)) 
print(round(mean(criminal_calc$conservative_upper_bound),4)) 
print(round(mean(criminal_calc$availability),4))

criminal_calc = subset(criminal,(reliable == 1)&(level == 4)&(year_id == 2021)) 
est = colSums(criminal_calc[,2:5001])/length(criminal_calc$division)
print(round(quantile(est, c(0.05,1))[1],4)) # 
print(round(mean(criminal_calc$conservative_upper_bound),4)) # 
print(round(mean(criminal_calc$availability),4))# 

criminal_calc = subset(criminal,(reliable == 1)&(level == 4)&(year_id == 2022)) 
est = colSums(criminal_calc[,2:5001])/length(criminal_calc$division)
print(round(quantile(est, c(0.05,1))[1],4)) #
print(round(mean(criminal_calc$conservative_upper_bound),4)) # 
print(round(mean(criminal_calc$availability),4))# 


# civil -----
civil = read.csv(file = 'data/court18-22/fulltext/final_estimates/civil_transparency_final.csv') 
load("data/court18-22/fulltext/simulation/PI_civil_SIM.Rdata")

civil$conservative_maximum = ifelse(civil$method ==	'mvuekth',civil$mvuekth_lower, civil$max_case_number) # when using kth method, the conservative maximum is defined as k-th largest.
civil$conservative_upper_bound = civil$case_in_division/civil$conservative_maximum # for each batch, calculate the upper bound of availability.
civil = cbind(civil_res,civil)
civil = merge(civil,court, all.x = TRUE, by = 'court_id')

civil_calc = subset(civil,(reliable == 1)&(level == 4)&(year_id == 2018)) 
est = colSums(civil_calc[,2:5001])/length(civil_calc$division)
print(round(quantile(est, c(0.05,1))[1],4)) 
print(round(mean(civil_calc$conservative_upper_bound),4)) 
print(round(mean(civil_calc$availability),4))

civil_calc = subset(civil,(reliable == 1)&(level == 4)&(year_id == 2019)) 
est = colSums(civil_calc[,2:5001])/length(civil_calc$division)
print(round(quantile(est, c(0.05,1))[1],4)) 
print(round(mean(civil_calc$conservative_upper_bound),4)) 
print(round(mean(civil_calc$availability),4))

civil_calc = subset(civil,(reliable == 1)&(level == 4)&(year_id == 2020)) 
est = colSums(civil_calc[,2:5001])/length(civil_calc$division)
print(round(quantile(est, c(0.05,1))[1],4)) 
print(round(mean(civil_calc$conservative_upper_bound),4)) 
print(round(mean(civil_calc$availability),4))

civil_calc = subset(civil,(reliable == 1)&(level == 4)&(year_id == 2021)) 
est = colSums(civil_calc[,2:5001])/length(civil_calc$division)
print(round(quantile(est, c(0.05,1))[1],4)) 
print(round(mean(civil_calc$conservative_upper_bound),4)) 
print(round(mean(civil_calc$availability),4))

civil_calc = subset(civil,(reliable == 1)&(level == 4)&(year_id == 2022)) 
est = colSums(civil_calc[,2:5001])/length(civil_calc$division)
print(round(quantile(est, c(0.05,1))[1],4)) 
print(round(mean(civil_calc$conservative_upper_bound),4)) 
print(round(mean(civil_calc$availability),4))
