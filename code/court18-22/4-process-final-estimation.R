# This file process the transparency estimates for year 2018-2022

# Load data
court = read.csv('data/courts.csv')
# criminal analysis ----
criminal = read.csv(file = 'data/court18-22/fulltext/final_estimates/criminal_first_transparency.csv')
criminal = merge(criminal,court, all.x = TRUE, by = 'court_id')
criminal$est_total = NA
criminal$est_total = ifelse((criminal$ks_p_value_mvue>=0.05),criminal$mvue,criminal$est_total)
criminal$est_total = ifelse((is.na(criminal$est_total)&(criminal$ks_p_value_kth>=0.05)),criminal$mvuekth,criminal$est_total)
criminal$est_total = ifelse((is.na(criminal$est_total)&(criminal$ks_p_value_coarsen>=0.05)),criminal$mvue_coarsen,criminal$est_total)
#criminal$est_total = ifelse((is.na(criminal$est_total)&(!is.na(criminal$linearEstimation))&(criminal$white_test_p_value>=0.05)&(criminal$r_squared>0.8)&(criminal$linearEstimation>criminal$case_in_division)), criminal$linearEstimation,criminal$est_total)
criminal$reliable = ifelse(is.na(criminal$est_total),0,1)
#criminal$est_total = ifelse((is.na(criminal$est_total))&(!is.na(criminal$linearEstimation))&(criminal$availStart_in_batch>25)&(criminal$linearEstimation>criminal$case_in_division),criminal$linearEstimation,criminal$est_total)
criminal$est_total = ifelse((is.na(criminal$est_total)&(!is.na(criminal$mvuekth))),criminal$mvuekth,criminal$est_total)
criminal$est_total = ifelse((is.na(criminal$est_total)),criminal$mvue,criminal$est_total)
criminal = subset(criminal,est_total<30000) 
criminal$availability = criminal$case_in_division/criminal$est_total

criminal$method = NA
criminal$method = ifelse((criminal$ks_p_value_mvue>=0.05),"mvue",criminal$method)
criminal$method = ifelse((is.na(criminal$method)&(criminal$ks_p_value_kth>=0.05)),"mvuekth",criminal$method)
criminal$method = ifelse((is.na(criminal$method)&(criminal$ks_p_value_coarsen>=0.05)),"mvue_coarsen",criminal$method)
#criminal$method = ifelse((is.na(criminal$method)&(!is.na(criminal$linearEstimation))&(criminal$white_test_p_value>=0.05)&(criminal$r_squared>0.8)&(criminal$linearEstimation>criminal$case_in_division)), "linearEstimation",criminal$method)
#criminal$method = ifelse((is.na(criminal$method))&(!is.na(criminal$linearEstimation))&(criminal$availStart_in_batch>25)&(criminal$linearEstimation>criminal$case_in_division),"linearEstimation",criminal$method)
criminal$method = ifelse((is.na(criminal$method)&(!is.na(criminal$mvuekth))),"mvuekth",criminal$method)
criminal$method = ifelse((is.na(criminal$method)),"mvue",criminal$method)

# admin analysis ----

admin = read.csv(file = 'data/court18-22/fulltext/final_estimates/admin_first_transparency.csv')
admin = merge(admin,court, all.x = TRUE, by = 'court_id')

admin$est_total = NA

admin$est_total = ifelse((admin$ks_p_value_mvue>=0.05),admin$mvue,admin$est_total)
admin$est_total = ifelse((is.na(admin$est_total)&(admin$ks_p_value_kth>=0.05)),admin$mvuekth,admin$est_total)
admin$est_total = ifelse((is.na(admin$est_total)&(admin$ks_p_value_coarsen>=0.05)),admin$mvue_coarsen,admin$est_total)
#admin$est_total = ifelse((is.na(admin$est_total)&(!is.na(admin$linearEstimation))&(admin$white_test_p_value>=0.05)&(admin$r_squared>0.8)&(admin$linearEstimation>admin$case_in_division)), admin$linearEstimation,admin$est_total)
admin$reliable = ifelse(is.na(admin$est_total),0,1)
#admin$est_total = ifelse((is.na(admin$est_total))&(!is.na(admin$linearEstimation))&(admin$availStart_in_batch>25)&(admin$linearEstimation>admin$case_in_division),admin$linearEstimation,admin$est_total)
admin$est_total = ifelse((is.na(admin$est_total)&(!is.na(admin$mvuekth))),admin$mvuekth,admin$est_total)
admin$est_total = ifelse((is.na(admin$est_total)),admin$mvue,admin$est_total)
admin = subset(admin,est_total<30000) # thrown out unrealistic estimations, can use 15000
admin$availability = admin$case_in_division/admin$est_total

admin$method = NA
admin$method = ifelse((admin$ks_p_value_mvue>=0.05),'MVUE',admin$method)
admin$method = ifelse((is.na(admin$method)&(admin$ks_p_value_kth>=0.05)),'mvuekth',admin$method)
admin$method = ifelse((is.na(admin$method)&(admin$ks_p_value_coarsen>=0.05)),'mvue_coarsen',admin$method)
admin$method = ifelse((is.na(admin$method)&(!is.na(admin$mvuekth))),'mvuekth',admin$method)
admin$method = ifelse((is.na(admin$method)),'mvue',admin$method)


civil2018 = read.csv(file = 'data/court18-22/fulltext/final_estimates/civil_first_2018_transparency.csv')
civil2019 = read.csv(file = 'data/court18-22/fulltext/final_estimates/civil_first_2019_transparency.csv')
civil2020 = read.csv(file = 'data/court18-22/fulltext/final_estimates/civil_first_2020_transparency.csv')
civil2021 = read.csv(file = 'data/court18-22/fulltext/final_estimates/civil_first_2021_transparency.csv')
civil2022 = read.csv(file = 'data/court18-22/fulltext/final_estimates/civil_first_2022_transparency.csv')
civil = rbind(civil2018,civil2019,civil2020,civil2021,civil2022)
rm(civil2018,civil2019,civil2020,civil2021,civil2022)

civil = merge(civil,court, all.x = TRUE, by = 'court_id')
civil$est_total = NA
civil$est_total = ifelse((civil$ks_p_value_mvue>=0.05),civil$mvue,civil$est_total)
civil$est_total = ifelse((is.na(civil$est_total)&(civil$ks_p_value_kth>=0.05)),civil$mvuekth,civil$est_total)
civil$est_total = ifelse((is.na(civil$est_total)&(civil$ks_p_value_coarsen>=0.05)),civil$mvue_coarsen,civil$est_total)
#civil$est_total = ifelse((is.na(civil$est_total)&(!is.na(civil$linearEstimation))&(civil$white_test_p_value>=0.05)&(civil$r_squared>0.8)&(civil$linearEstimation>civil$case_in_division)), civil$linearEstimation,civil$est_total)
civil$reliable = ifelse(is.na(civil$est_total),0,1)
#civil$est_total = ifelse((is.na(civil$est_total))&(!is.na(civil$linearEstimation))&(civil$availStart_in_batch>25)&(civil$linearEstimation>civil$case_in_division),civil$linearEstimation,civil$est_total)
civil$est_total = ifelse((is.na(civil$est_total)&(!is.na(civil$mvuekth))),civil$mvuekth,civil$est_total)
civil$est_total = ifelse((is.na(civil$est_total)),civil$mvue,civil$est_total)
civil = subset(civil,est_total<30000) # thrown out unrealistic estimations, can use 150000 as well
civil$availability = civil$case_in_division/civil$est_total

civil$method = NA
civil$method = ifelse((civil$ks_p_value_mvue>=0.05),"mvue",civil$method)
civil$method = ifelse((is.na(civil$method)&(civil$ks_p_value_kth>=0.05)),"mvuekth",civil$method)
civil$method = ifelse((is.na(civil$method)&(civil$ks_p_value_coarsen>=0.05)),"mvue_coarsen",civil$method)
#civil$method = ifelse((is.na(civil$method)&(!is.na(civil$linearEstimation))&(civil$white_test_p_value>=0.05)&(civil$r_squared>0.8)&(civil$linearEstimation>civil$case_in_division)), "linearEstimation",civil$method)
#civil$method = ifelse((is.na(civil$method))&(!is.na(civil$linearEstimation))&(civil$availStart_in_batch>25)&(civil$linearEstimation>civil$case_in_division),"linearEstimation",civil$method)
civil$method = ifelse((is.na(civil$method)&(!is.na(civil$mvuekth))),"mvuekth",civil$method)
civil$method = ifelse((is.na(civil$method)),"mvue",civil$method)

write.csv(admin,'data/court18-22/fulltext/final_estimates/admin_transparency_final.csv')
write.csv(criminal,'data/court18-22/fulltext/final_estimates/criminal_transparency_final.csv')
write.csv(civil,'data/court18-22/fulltext/final_estimates/civil_transparency_final.csv')
