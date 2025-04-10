# This file contains replication code for estimating transparency in criminal
# litigations from 2013 to 2018 in Chinese Courts
# using MUVE, Coarsened German tank estimator,
# k-th largest German tank estimator, and linear regression.

# Dataset that can be processed using this file:
# data/court13-17/raw_data/criminal_litigation.csv


# load data
initial_data = read.csv('data/court13-17/raw_data/criminal_litigation.csv')
court = read.csv(file = 'data/courts.csv',header = TRUE, sep = ',')
court=court[!duplicated(court[,c('std_court')]),]

data_clean = function(dataset){
  
  #clean court subdivision 
  dataset$court_division=gsub('之一','',dataset$court_division)
  dataset$court_division=gsub('之二','',dataset$court_division)
  dataset$court_division=gsub('之三','',dataset$court_division)
  dataset$court_division=gsub('之四','',dataset$court_division)
  dataset$court_division=gsub('之五','',dataset$court_division)
  dataset$court_division=gsub('之六','',dataset$court_division)
  dataset$court_division=gsub('之七','',dataset$court_division)
  dataset$court_division=gsub('之八','',dataset$court_division)
  dataset$court_division=gsub('之九','',dataset$court_division)
  
  #clean case number
  dataset$case_number_id=gsub(' ','',dataset$case_number_id)
  dataset$case_number_id=gsub('０','0',dataset$case_number_id)
  dataset$case_number_id=gsub('１','1',dataset$case_number_id)
  dataset$case_number_id=gsub('２','2',dataset$case_number_id)
  dataset$case_number_id=gsub('３','3',dataset$case_number_id)
  dataset$case_number_id=gsub('４','4',dataset$case_number_id)
  dataset$case_number_id=gsub('５','5',dataset$case_number_id)
  dataset$case_number_id=gsub('６','6',dataset$case_number_id)
  dataset$case_number_id=gsub('７','7',dataset$case_number_id)
  dataset$case_number_id=gsub('８','8',dataset$case_number_id)
  dataset$case_number_id=gsub('９','9',dataset$case_number_id)
  
  #remove cases with missing metadata
  dataset = dataset[dataset$std_court!="",] 
  dataset = dataset[dataset$case_number_id!="",]
  dataset = dataset[dataset$court_division!="",]
  
  #clean case serial number
  pattern = "\\d*"
  dataset$clean_case_number=regmatches(dataset$case_number_id, regexpr(pattern, dataset$case_number_id))
  pattern='0*'
  dataset$clean_case_number=sub(pattern,"",dataset$clean_case_number)
  dataset$clean_case_number = as.numeric(dataset$clean_case_number)
  dataset = dataset[!is.na(dataset$clean_case_number),]
  dataset = subset(dataset,clean_case_number>=0)
  
  # create division = year-court-court_division
  dataset$division <- paste(dataset$year_id, dataset$std_court, dataset$court_division, sep=" ")
  dataset$num_id=as.numeric(as.factor(dataset$division))
  dataset=dataset[rev(order(dataset$division, dataset$clean_case_number, dataset$converted_date)),]
  dataset=dataset[!duplicated(dataset[,c('clean_case_number','division')]),] # further remove dups with same case_id
  dataset$division=as.factor(dataset$division)
  
  # Merge additional court information
  dataset= merge(dataset,court,all.x = TRUE,by="std_court")
  
  # count how many cases in one division - save as case_in_division
  bydivision <- tapply(dataset$clean_case_number, dataset$division, length)
  iddata = data.frame(division=names(bydivision),case_in_division=bydivision)
  dataset= merge(dataset,iddata,all.x = TRUE,by="division")
  
  dataset = subset(dataset,court_division == '刑初'|court_division == '刑终'|court_division == '刑再')
  # remove cases with unrealistic case_number
  # dataset = subset(dataset,clean_case_number<100000)
  
  return(dataset)
}

get_german_tank_solution = function(dataset){
  # found out the maximum case number for each batch and save as max_case_number
  bycase_number=tapply(dataset$clean_case_number, dataset$division, max)
  iddata = data.frame(division=names(bycase_number),max_case_number=bycase_number)
  dataset= merge(dataset,iddata,by="division")
  
  # Calculate mvue and bayesian german-tank estimation and confidence interval for mvue method.
  dataset$mvue = dataset$max_case_number+dataset$max_case_number/dataset$case_in_division-1
  dataset$bayesian = ((dataset$case_in_division-1)/(dataset$case_in_division-2)*(dataset$max_case_number-1))
  dataset$mvue_upper = dataset$max_case_number*20^(1/dataset$case_in_division)
  return(dataset)
}

get_coarsen_GTS = function(dataset,binsize){
  dataset$coarsened_clean_case_number = ceiling(dataset$clean_case_number/binsize)
  bycase_number=tapply(dataset$coarsened_clean_case_number, dataset$division, max)
  iddata = data.frame(division=names(bycase_number),max_case_number_coarsen=bycase_number)
  dataset= merge(dataset,iddata,by="division")
  
  bycase_number=tapply(dataset$coarsened_clean_case_number, dataset$division, function(x) length(unique(x)))
  iddata = data.frame(division=names(bycase_number),case_in_division_coarsen=bycase_number)
  dataset = merge(dataset,iddata,by="division")
  
  dataset$mvue_coarsen = binsize*(dataset$max_case_number_coarsen+dataset$max_case_number_coarsen/dataset$case_in_division_coarsen-1)
  dataset$mvue_upper_coarsen = dataset$max_case_number_coarsen*20^(1/dataset$case_in_division_coarsen)*binsize
  return(dataset)
}



get_key_dates = function(dataset){
  
  #get decision days for each cases (from Jan.1st) 
  yearnum = as.Date(paste(as.character(dataset$year_id),"-01-01",sep = ""),"%Y-%m-%d")
  dataset$converted_date = as.Date(dataset$converted_date)
  dataset$decisiondays = as.numeric(dataset$converted_date-yearnum)
  
  #get start days for each cases (from Jan.1st) 立案时间
  tempdate = data.frame(startdate=dataset$converted_start,year_id=dataset$year_id,id=dataset$id)
  tempdate = tempdate[as.character(tempdate$startdate)!="",]
  tempdate$startdate=as.Date(tempdate$startdate)
  tempyear = as.Date(paste(as.character(tempdate$year_id),"-01-01",sep = ""),"%Y-%m-%d")
  tempdate$startdays = as.numeric(tempdate$startdate-tempyear)
  tempdate = data.frame(startdays=tempdate$startdays,id=tempdate$id)
  dataset= merge(dataset,tempdate,by="id",all.x = TRUE)
  
  # Change startdays which are 60 days behind the year (because some courts start using new sequence at the end of the year) 
  # and 365 days after Jan. 1st to NA
  dataset$startdays[dataset$startdays <= -60] = NA
  dataset$startdays[dataset$startdays > 365] = NA
  
  return(dataset)
}

# function remove_outlier() and remove_outlier2() find outlier using loess.smooth()
remove_outlier <- function (dataset,tribunal){
  uncleanedtemp = subset(dataset,(division == tribunal)&!is.na(adjust_startdate))
  #uncomment to plot 
  #jpeg(paste("data/images/", as.character(unique(uncleanedtemp$division)),".jpg",sep=""))
  #plot(adjust_startdate ~ clean_case_number, data = uncleanedtemp,ylim = c(0,250),xlim = c(0,(max(uncleanedtemp$max_case_number)+30)))
  lo <- try(loess.smooth(uncleanedtemp$clean_case_number, uncleanedtemp$adjust_startdate,span = 0.5), silent = TRUE)
  if (inherits(lo,"try-error")){
    return(uncleanedtemp)
  }
  #lines(lo$x, lo$y, lwd = 3,col = 'red')
  #lines(lo$x, lo$y + 37.5, lwd = 3, col = 'blue')
  #lines(lo$x, lo$y - 37.5, lwd = 3, col = 'blue')
  f1 <- approxfun(lo$x, lo$y + 37.5)
  wh1 <- which(uncleanedtemp$adjust_startdate > f1(uncleanedtemp$clean_case_number))
  f2 <- approxfun(lo$x, lo$y - 37.5)
  wh2 <- which(uncleanedtemp$adjust_startdate < f2(uncleanedtemp$clean_case_number))
  ## identify points to exclude
  exclude <- uncleanedtemp[c(wh1, wh2), ]
  #points(exclude$clean_case_number, exclude$adjust_startdate, pch = 4, col = 2, cex = 2)
  #dev.off()
  if (length(exclude$startdays)>0){
    temp <- uncleanedtemp[-c(wh1, wh2), ]
  }else{
    temp = uncleanedtemp
  }
  return(temp)
}

remove_outlier2 <- function (dataset,tribunal){
  uncleanedtemp = subset(dataset,division == tribunal&!is.na(adjust_startdate))
  t=quantile(uncleanedtemp$clean_case_number)
  gap = 0.15*(t['25%']+t['50%']+t['75%'])/1.5
  #uncomment to plot 
  #jpeg(paste("data/images/", as.character(unique(uncleanedtemp$division)),".jpg",sep=""))
  #plot( clean_case_number ~ adjust_startdate, data = uncleanedtemp,xlim = c(0,250),ylim = c(0,(max(uncleanedtemp$max_case_number)+30)))
  lo <- try(loess.smooth(uncleanedtemp$adjust_startdate,uncleanedtemp$clean_case_number,span = 0.5), silent = TRUE)
  if (inherits(lo,"try-error")){
    return(uncleanedtemp)
  }
  #lines(lo$x, lo$y, lwd = 3,col = 'red')
  #lines(lo$x, lo$y + gap, lwd = 3, col = 'blue')
  #lines(lo$x, lo$y - gap, lwd = 3, col = 'blue')
  f1 <- try(approxfun(lo$x, lo$y + gap))
  if (inherits(f1,"try-error")){
    return(uncleanedtemp)
  }
  wh1 <- which(uncleanedtemp$clean_case_number > f1(uncleanedtemp$adjust_startdate))
  f2 <- approxfun(lo$x, lo$y - gap)
  wh2 <- which(uncleanedtemp$clean_case_number < f2(uncleanedtemp$adjust_startdate))
  ## identify points to exclude
  exclude <- uncleanedtemp[c(wh1, wh2), ]
  #points(exclude$adjust_startdate,exclude$clean_case_number, pch = 4, col = 2, cex = 2)
  #dev.off()
  if (length(exclude$startdays)>0){
    temp <- uncleanedtemp[-c(wh1, wh2), ]
  }else{
    temp = uncleanedtemp
  }
  return(temp)
}

remove_outlier_all_batch = function(dataset){
  Workday = subset(dataset,adjust_startdate != 'Holiday')
  Workday$adjust_startdate = as.numeric(as.character(Workday$adjust_startdate))
  #Workday$adjust_decisiondate = as.numeric(as.character(Workday$adjust_decisiondate))
  
  # count how many cases have start dates for each division, store as start_number
  bystart_number <- tapply(Workday$clean_case_number, Workday$division, length)
  iddata = data.frame(division=names(bystart_number),start_number=bystart_number)
  Workday= merge(Workday,iddata,by="division",all.x = TRUE)
  #Workday$adjust_decisiondate = as.numeric(as.character(Workday$adjust_decisiondate)) # generate NA for cases decided on holidays
  
  division_list = Workday[!duplicated(Workday$division),]$division
  
  flag = 1
  for (i in 1:length(division_list)){
    dat = subset(Workday,division == division_list[i])
    print(i)
    if (length(dat$startdays) >= 10){
      if (flag ==1){
        ground_truth1 = remove_outlier(Workday,division_list[i])
        flag = flag +1
      }else{
        ground_truth1 = rbind(ground_truth1,remove_outlier(Workday,division_list[i]))
      }
    }else{
      if (flag ==1){
        ground_truth1 = dat
        flag = flag +1
      }else{ground_truth1 = rbind(ground_truth1,dat)}
    }}
  
  
  flag = 1
  for (i in 1:length(division_list)){
    dat = subset(ground_truth1,division == division_list[i])
    if (i%%100 ==0){print(i)}
    if (length(dat$startdays) >= 10){
      if (flag ==1){
        ground_truth_filter2 = remove_outlier2(ground_truth1,division_list[i])
        flag = flag +1
      }else{
        ground_truth_filter2 = rbind(ground_truth_filter2,remove_outlier2(ground_truth1,division_list[i]))
      }
    }else{
      if (flag ==1){
        ground_truth_filter2 = dat
        flag = flag +1
      }else{ground_truth_filter2 = rbind(ground_truth_filter2,dat)}
    }}
  
  return(ground_truth_filter2)
  
}


adjust_start_date = function(dataset,workday,year){
  # convert startdate from 365 scale to 250 scale by removing the weekends and holidays.
  workday_starttrans = workday
  workday_starttrans$startdays = workday_starttrans$days
  workday_starttrans = subset(workday_starttrans,select = -c(days))
  
  if (year == 2013){
    workday_starttrans$adjust_startdate = workday_starttrans$X2013Workdays
    workday_starttrans = subset(workday_starttrans,select = -c(X2013Workdays,X2014Workdays,X2015Workdays,X2016Workdays,X2017Workdays))
    dataset = merge(dataset,workday_starttrans,by = "startdays", all.x = TRUE)
    return(dataset)
  }else if (year == 2014){
    workday_starttrans$adjust_startdate = workday_starttrans$X2014Workdays
    workday_starttrans = subset(workday_starttrans,select = -c(X2013Workdays,X2014Workdays,X2015Workdays,X2016Workdays,X2017Workdays))
    dataset = merge(dataset,workday_starttrans,by = "startdays", all.x = TRUE)
    return(dataset)
  }else if (year == 2015){
    workday_starttrans$adjust_startdate = workday_starttrans$X2015Workdays
    workday_starttrans = subset(workday_starttrans,select = -c(X2013Workdays,X2014Workdays,X2015Workdays,X2016Workdays,X2017Workdays))
    dataset = merge(dataset,workday_starttrans,by = "startdays", all.x = TRUE)
    return(dataset)
  }else if (year == 2016){
    workday_starttrans$adjust_startdate = workday_starttrans$X2016Workdays
    workday_starttrans = subset(workday_starttrans,select = -c(X2013Workdays,X2014Workdays,X2015Workdays,X2016Workdays,X2017Workdays))
    dataset = merge(dataset,workday_starttrans,by = "startdays", all.x = TRUE)
    return(dataset)
  }else if (year == 2017){
    workday_starttrans$adjust_startdate = workday_starttrans$X2017Workdays
    workday_starttrans = subset(workday_starttrans,select = -c(X2013Workdays,X2014Workdays,X2015Workdays,X2016Workdays,X2017Workdays))
    dataset = merge(dataset,workday_starttrans,by = "startdays", all.x = TRUE)
    return(dataset)
  }
}

linear_estimator = function(dataset){
  #Estimate total # of cases using linear regression
  division_list = dataset[!duplicated(dataset$division),]$division
  linearEstimator = data.frame(division = c(),availStart_in_batch = c(),linearEstimation = c(),lower_bound_confidence = c(), upper_bound_confidence = c(),lower_bound_prediction = c(), upper_bound_prediction = c(),r_squared = c(), white_test_p_value = c())
  
  for (i in 1:length(division_list)){
    if (i%%100 ==0){print(i)}
    target_batch = subset(dataset,division == division_list[i])
    temp_model = try(lm(clean_case_number ~ adjust_startdate + 0, data = target_batch), silent = TRUE)
    if (!inherits(temp_model,"try-error")){
      new.dat <- data.frame(adjust_startdate = 250) # change to 251 for 2016
      pred_c = predict(temp_model, newdata = new.dat, interval = 'confidence')
      pred_p = predict(temp_model, newdata = new.dat, interval = 'prediction')
      wt = bptest(temp_model, ~ adjust_startdate + I(adjust_startdate^2), data = target_batch)
      temp = data.frame(division = division_list[i],availStart_in_batch = length(target_batch$adjust_startdate),linearEstimation = pred_c[1],lower_bound_confidence = pred_c[2], upper_bound_confidence = pred_c[3],lower_bound_prediction = pred_p[2], upper_bound_prediction = pred_p[3],r_squared = summary(temp_model)[["r.squared"]],white_test_p_value = wt[["p.value"]])
      linearEstimator = rbind(linearEstimator,temp)
    }
  }
  return(linearEstimation)
}

# clean dataset
initial_data$std_court=as.character(initial_data$std_court)
initial_data$case_number_id=as.character(initial_data$case_number_id)
initial_data$court_division=as.character(initial_data$court_division)
initial_data$year_id = as.numeric(initial_data$year_id)
cleanData = data_clean(initial_data)

rm(initial_data)

courtNumber = cleanData$std_court[!duplicated(cleanData$std_court)]
batchNumber = cleanData$division[!duplicated(cleanData$division)]

print(paste0("Number of distinct courts from 2013 to 2017 in criminal dataset:",length(courtNumber)))
print(paste0("Number of distinct batches from 2013 to 2017 in criminal dataset:",length(batchNumber)))

GTS = get_german_tank_solution(cleanData)
GTS = get_coarsen_GTS(GTS,10)

# uniformality test for MVUE
complete_division_list = GTS[!duplicated(GTS$division),]$division
GTS$KSpercentMVUE = GTS$clean_case_number/GTS$max_case_number
kstest = data.frame(division = c(),ks_p_value_mvue = c())

for (i in 1:length(complete_division_list)){
  if (i%%100 ==0){print(i)}
  target_batch = subset(GTS,division == complete_division_list[i])
  target_batch = subset(target_batch,KSpercentMVUE != 1)
  if (length(target_batch$division) == 0){
    temp = data.frame(division = complete_division_list[i],ks_p_value_mvue = 0)
  }else{
    ks = ks.test(target_batch$KSpercentMVUE,"punif",0,1)
    temp = data.frame(division = complete_division_list[i],ks_p_value_mvue = ks[["p.value"]])
  }
  kstest = rbind(kstest,temp)
}


GTS = merge(GTS,kstest,by='division')


# uniformalty test for CoarsenedGTS
complete_division_list = GTS[!duplicated(GTS$division),]$division
GTS$KSpercentCoarsen = GTS$coarsened_clean_case_number/GTS$max_case_number_coarsen
GTS_drop = GTS[!duplicated(GTS[c("division","coarsened_clean_case_number")]),]
kstest = data.frame(division = c(),ks_p_value_coarsen = c())

for (i in 1:length(complete_division_list)){
  if (i%%100 ==0){print(i)}
  target_batch = subset(GTS_drop,division == complete_division_list[i])
  target_batch = subset(target_batch,KSpercentCoarsen != 1)
  if (length(target_batch$division) == 0){
    temp = data.frame(division = complete_division_list[i],ks_p_value_coarsen = 0)
  }else{
    ks = ks.test(target_batch$KSpercentCoarsen,"punif",0,1)
    temp = data.frame(division = complete_division_list[i],ks_p_value_coarsen = ks[["p.value"]])
  }
  kstest = rbind(kstest,temp)
}

GTS = merge(GTS,kstest,by='division')

# Kth removed MVUE
GTS_temp = subset(GTS,(case_in_division > 10)&(ks_p_value_mvue<0.05))
GTS_temp$average_gap = (GTS_temp$max_case_number - GTS_temp$case_in_division)/GTS_temp$case_in_division
GTS_temp = subset(GTS_temp,average_gap>3)

f <- function(x){sort(x, TRUE)[2]}
bycase_number=tapply(GTS_temp$clean_case_number, GTS_temp$division, f)
iddata = data.frame(division=names(bycase_number),largest2=bycase_number)
GTS_temp= merge(GTS_temp,iddata,by="division")

f <- function(x){sort(x, TRUE)[3]}
bycase_number=tapply(GTS_temp$clean_case_number, GTS_temp$division, f)
iddata = data.frame(division=names(bycase_number),largest3=bycase_number)
GTS_temp= merge(GTS_temp,iddata,by="division")

f <- function(x){sort(x, TRUE)[4]}
bycase_number=tapply(GTS_temp$clean_case_number, GTS_temp$division, f)
iddata = data.frame(division=names(bycase_number),largest4=bycase_number)
GTS_temp= merge(GTS_temp,iddata,by="division")

f <- function(x){sort(x, TRUE)[5]}
bycase_number=tapply(GTS_temp$clean_case_number, GTS_temp$division, f)
iddata = data.frame(division=names(bycase_number),largest5=bycase_number)
GTS_temp= merge(GTS_temp,iddata,by="division")

GTS_temp$gap1 = GTS_temp$max_case_number - GTS_temp$largest2
GTS_temp$gap2 = GTS_temp$largest2 - GTS_temp$largest3
GTS_temp$gap3 = GTS_temp$largest3 - GTS_temp$largest4
GTS_temp$gap4 = GTS_temp$largest4 - GTS_temp$largest5

GTS_throw4 = subset(GTS_temp,gap4>10*average_gap)
GTS_throw3 = subset(GTS_temp,(gap3>10*average_gap)&(gap4<10*average_gap))
GTS_throw2 = subset(GTS_temp,(gap2>10*average_gap)&(gap3<10*average_gap)&(gap4<10*average_gap))
GTS_throw1 = subset(GTS_temp,(gap1>10*average_gap)&(gap2<10*average_gap)&(gap3<10*average_gap)&(gap4<10*average_gap))

GTS_throw4$throw = 4
GTS_throw3$throw = 3
GTS_throw2$throw = 2
GTS_throw1$throw = 1

GTS_throw4 = subset(GTS_throw4,clean_case_number<largest5)
GTS_throw3 = subset(GTS_throw3,clean_case_number<largest4)
GTS_throw2 = subset(GTS_throw2,clean_case_number<largest3)
GTS_throw1 = subset(GTS_throw1,clean_case_number<largest2)

GTS_throw4$mvuekth = GTS_throw4$largest5*(GTS_throw4$case_in_division+1)/(GTS_throw4$case_in_division-4)-1
GTS_throw4$mvuekth_lower = GTS_throw4$largest5 + 4
GTS_throw4$mvuekth_upper = GTS_throw4$largest5*20^(1/((GTS_throw4$case_in_division - 4)/(4+1)))
GTS_throw4$largest = GTS_throw4$largest5

GTS_throw3$mvuekth = GTS_throw3$largest4*(GTS_throw3$case_in_division+1)/(GTS_throw3$case_in_division-3)-1
GTS_throw3$mvuekth_lower = GTS_throw3$largest4 + 3
GTS_throw3$mvuekth_upper = GTS_throw3$largest4*20^(1/((GTS_throw3$case_in_division - 3)/(3+1)))
GTS_throw3$largest = GTS_throw3$largest4

GTS_throw2$mvuekth = GTS_throw2$largest3*(GTS_throw2$case_in_division+1)/(GTS_throw2$case_in_division-2)-1
GTS_throw2$mvuekth_lower = GTS_throw2$largest3 + 2
GTS_throw2$mvuekth_upper = GTS_throw2$largest3*20^(1/((GTS_throw2$case_in_division - 2)/(2+1)))
GTS_throw2$largest = GTS_throw2$largest3

GTS_throw1$mvuekth = GTS_throw1$largest2*(GTS_throw1$case_in_division+1)/(GTS_throw1$case_in_division-1)-1
GTS_throw1$mvuekth_lower = GTS_throw1$largest2 + 1
GTS_throw1$mvuekth_upper = GTS_throw1$largest2*20^(1/((GTS_throw1$case_in_division - 1)/(1+1)))
GTS_throw1$largest = GTS_throw1$largest2

GTS_throw = rbind(GTS_throw1,GTS_throw2,GTS_throw3,GTS_throw4)

# uniformalty test for kth largest
complete_division_list = GTS_throw[!duplicated(GTS_throw$division),]$division
GTS_throw$KSpercentkth = GTS_throw$clean_case_number/GTS_throw$largest
kstest = data.frame(division = c(),ks_p_value_kth = c())

for (i in 1:length(complete_division_list)){
  if (i%%100 ==0){print(i)}
  target_batch = subset(GTS_throw,division == complete_division_list[i])
  target_batch = subset(target_batch,KSpercentkth != 1)
  if (length(target_batch$division) == 0){
    temp = data.frame(division = complete_division_list[i],ks_p_value_kth = 0)
  }else{
    ks = ks.test(target_batch$KSpercentkth,"punif",0,1)
    temp = data.frame(division = complete_division_list[i],ks_p_value_kth = ks[["p.value"]])
  }
  kstest = rbind(kstest,temp)
}

GTS_throw = merge(GTS_throw,kstest,by='division')

GTS_throw = subset(GTS_throw, select = c(division,throw,mvuekth,mvuekth_lower,mvuekth_upper,ks_p_value_kth))

GTS_throw = GTS_throw[!duplicated(GTS_throw$division),]

GTS = merge(GTS,GTS_throw,all.x = TRUE, on = 'divisions')

# get key dates
GTS = get_key_dates(GTS)

processedData = subset(GTS,decisiondays >= -60)

workday = read.csv(file = 'data/workday.csv',header = TRUE,sep = ',')

data2013 = subset(processedData,year_id == 2013)
data2013 = adjust_start_date(data2013,workday,2013)
data2014 = subset(processedData,year_id == 2014)
data2014 = adjust_start_date(data2014,workday,2014)
data2015 = subset(processedData,year_id == 2015)
data2015 = adjust_start_date(data2015,workday,2015)
data2016 = subset(processedData,year_id == 2016)
data2016 = adjust_start_date(data2016,workday,2016)
data2017 = subset(processedData,year_id == 2017)
data2017 = adjust_start_date(data2017,workday,2017)

processedData = rbind(data2013,data2014,data2015,data2016,data2017)

processedDataSave = subset(processedData,court_division == '刑初')
processedDataSave = subset(processedDataSave,select = c(filename,division,decisiondays,adjust_startdate,startdays,clean_case_number))
write.csv(processedDataSave,'data/court13-17/temp_file_for_ttd/criminal13-17-all.csv') 

# remove outlier and get all cases filed on Workdays
processedData = remove_outlier_all_batch(processedData)

processedDataSave = subset(processedData,select = c(division,startdays,id,std_court,province,converted_date,converted_start,year_id,case_number_id,court_division, std_court_id,case_id, filename,clean_case_number,num_id,court_id,district,level,city,case_in_division,max_case_number,mvue,bayesian, decisiondays,adjust_startdate))
write.csv(processedDataSave,'data/court13-17/temp_file_for_ttd/criminal13-17temp.csv') 


#Estimate total # of cases using linear regression
linearEstimator = linear_estimator(processedData)
GTS = merge(GTS,linearEstimation,all.x = TRUE,by = 'division')
transparency = GTS[!duplicated(GTS$division),]
criminal = subset(transparency,select = c(division,std_court,year_id,court_division,case_in_division,court_id,district,level,city,max_case_number,mvue,bayesian,mvue_upper,coarsened_clean_case_number,max_case_number_coarsen,case_in_division_coarsen,mvue_coarsen,mvue_upper_coarsen,ks_p_value_mvue,ks_p_value_coarsen,throw,mvuekth,mvuekth_lower,mvuekth_upper,ks_p_value_kth,linearEstimation,availStart_in_batch,lower_bound_confidence,upper_bound_confidence,lower_bound_prediction,upper_bound_prediction,r_squared,white_test_p_value,province))

criminal$est_total = NA
criminal$est_total = ifelse((criminal$ks_p_value_mvue>=0.05),criminal$mvue,criminal$est_total)
criminal$est_total = ifelse((is.na(criminal$est_total)&(criminal$ks_p_value_kth>=0.05)),criminal$mvuekth,criminal$est_total)
criminal$est_total = ifelse((is.na(criminal$est_total)&(criminal$ks_p_value_coarsen>=0.05)),criminal$mvue_coarsen,criminal$est_total)
criminal$est_total = ifelse((is.na(criminal$est_total)&(!is.na(criminal$linearEstimation))&(criminal$white_test_p_value>=0.05)&(criminal$r_squared>0.8)&(criminal$linearEstimation>criminal$case_in_division)), criminal$linearEstimation,criminal$est_total)
criminal$reliable = ifelse(is.na(criminal$est_total),0,1)
criminal$est_total = ifelse((is.na(criminal$est_total))&(!is.na(criminal$linearEstimation))&(criminal$availStart_in_batch>25)&(criminal$linearEstimation>criminal$case_in_division),criminal$linearEstimation,criminal$est_total)
criminal$est_total = ifelse((is.na(criminal$est_total)&(!is.na(criminal$mvuekth))),criminal$mvuekth,criminal$est_total)
criminal$est_total = ifelse((is.na(criminal$est_total)),criminal$mvue,criminal$est_total)
criminal = subset(criminal,est_total<30000) # six thrown out six unrealistic batches
criminal$availability = criminal$case_in_division/criminal$est_total

write.csv(criminal,'data/court13-17/final_estimates/criminal_transparency_final.csv')

