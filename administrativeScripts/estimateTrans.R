library(lmtest)

# import data
data2013 = read.csv(file = '2013admin/2013data.csv',header = TRUE, sep = ',')
data2014 = read.csv(file = '2014admin/2014data.csv',header = TRUE, sep = ',')
data2015 = read.csv(file = '2015admin/2015data.csv',header = TRUE, sep = ',')
data2016 = read.csv(file = '2016admin/2016data.csv',header = TRUE, sep = ',')
data2017 = read.csv(file = '2017admin/2017data.csv',header = TRUE, sep = ',')
court = read.csv(file = 'courts.csv',header = TRUE, sep = ',')
court=court[!duplicated(court[,c('std_court')]),]
rawdata = rbind(data2013,data2014,data2015,data2016,data2017)


# remove dups with same filename
rawdata=rawdata[!duplicated(rawdata[,c('filename')]),]
rawdata$std_court=as.character(rawdata$std_court)  # should be redundant
rawdata$case_number_id=as.character(rawdata$case_number_id)
rawdata$court_division=as.character(rawdata$court_division)  # should be redundant
rawdata$year_id = as.numeric(rawdata$year_id)

# clean dataset
cleanData = dataClean(rawdata)

courtNumber = cleanData$std_court[!duplicated(cleanData$std_court)]
batchNumber = cleanData$division[!duplicated(cleanData$division)]

print(paste0("Number of distinct courts from 2013 to 2017 in administrative dataset:",length(courtNumber)))
print(paste0("Number of distinct batches from 2013 to 2017 in administrative dataset:",length(batchNumber)))

# get german tank solution
GTS = getGermanTankSolution(cleanData)

# uniformality test
complete_division_list = GTS[!duplicated(GTS$division),]$division
GTS$KSpercent = GTS$clean_case_number/GTS$max_case_number
kstest = data.frame(division = c(),p_value = c())

for (i in 1:length(complete_division_list)){
  if (i%%100 ==0){print(i)}
  target_batch = subset(GTS,division == complete_division_list[i])
  ks = ks.test(target_batch$KSpercent,"punif",0,1)
  temp = data.frame(division = complete_division_list[i],ks_p_value = ks[["p.value"]])
  kstest = rbind(kstest,temp)
}

GTS = merge(GTS,kstest,by='division')

# get key dates
GTS = getKeydates(GTS)

# remove cases with decision days < -60 --- a case cannot be solved before filing
processedData = subset(GTS,decisiondays >= -60)

workday = read.csv(file = 'workday.csv',header = TRUE,sep = ',')

# adjust dates for 2013
data2013 = subset(processedData,year_id == 2013)
data2013 = adjustStartDate(data2013,workday,2013)

# remove outlier and get all cases filed on Workdays
data2013 = removeOutlier(data2013)

#Estimate total # of cases using linear regression
division_list = data2013[!duplicated(data2013$division),]$division
linearEstimator = data.frame(division = c(),availStart_in_batch = c(),linearEstimation = c(),lower_bound = c(), upper_bound = c(),r_squared = c(), white_test = c())

for (i in 1:length(division_list)){
  if (i%%100 ==0){print(i)}
  target_batch = subset(data2013,division == division_list[i])
  temp_model = try(lm(adjust_startdate ~ clean_case_number + 0, data = target_batch), silent = TRUE)
  if (!inherits(temp_model,"try-error")){
    confint.default(temp_model)[2]
    wt = bptest(temp_model, ~ clean_case_number + I(clean_case_number^2), data = target_batch)
    temp = data.frame(division = division_list[i],availStart_in_batch = length(target_batch$adjust_startdate),linearEstimation = round(250/as.numeric(coefficients(temp_model))),lower_bound = round(250/confint.default(temp_model)[2]), upper_bound = round(250/confint.default(temp_model)[1]),r_squared = summary(temp_model)[["r.squared"]],white_test_p_value = wt[["p.value"]])
    linearEstimator = rbind(linearEstimator,temp)
  }
}

write.csv(data2013,'2013admin/temp2013.csv')  
write.csv(linearEstimator,'2013admin/linearEstimator2013.csv')    

# repeat the same process for 2014-2017.

linearEstimation13 = read.csv('2013admin/linearEstimator2013.csv')
linearEstimation14 = read.csv('2014admin/linearEstimator2014.csv')
linearEstimation15 = read.csv('2015admin/linearEstimator2015.csv')
linearEstimation16 = read.csv('2016admin/linearEstimator2016.csv')
linearEstimation17 = read.csv('2017admin/linearEstimator2017.csv')
linearEstimation = rbind(linearEstimation13,linearEstimation14,linearEstimation15,linearEstimation16,linearEstimation17)
rm(linearEstimation13,linearEstimation14,linearEstimation15,linearEstimation16,linearEstimation17)
linearEstimation = subset(linearEstimation,select = -c(X))
GTS = merge(GTS,linearEstimation,all.x = TRUE,by = 'division')

GTS$province = GTS$province.x
GTS$gtp_transparency = GTS$availability
GTS$regression_transparency = GTS$case_in_division/GTS$linearEstimation
GTS$diffinestimation = abs(GTS$mvue - GTS$linearEstimation)
GTS = subset(GTS,select = -c(province.x,province.y,availability))
transparency = GTS[!duplicated(GTS$division),]
transparency = subset(transparency,select = -c(id,filename,converted_date,converted_start,converted_filing,case_number_id,case_id,clean_case_number,num_id,KSpercent,decisiondays,initial_startdate,filingdate,startdays))

write.csv(transparency,'admin_transparency.csv')



















