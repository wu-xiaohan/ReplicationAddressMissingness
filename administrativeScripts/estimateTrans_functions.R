#functions for transparency estimation: dataClean();getGermanTankSolution();
#getKeydates();getting_groundTruth();getting_groundTruth2();removeOutlier()

dataClean = function(dataset){
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
  
  # filter out year from 2013 to 2017, court division in ("行初","行终","行再")
  dataset= subset(dataset,year_id == '2017'|year_id == '2016'|year_id == '2015'|year_id == '2014'|year_id == '2013')
  dataset = subset(dataset,court_division == '行初'|court_division == '行终'|court_division == '行再')
  
  # count how many cases in one division - save as case_in_division
  bydivision <- tapply(dataset$clean_case_number, dataset$division, length)
  iddata = data.frame(division=names(bydivision),case_in_division=bydivision)
  dataset= merge(dataset,iddata,all.x = TRUE,by="division")
  
  # remove cases with unrealistic case_number
  dataset = subset(dataset,clean_case_number<10000)
  
  return(dataset)
}

getGermanTankSolution = function(dataset){
  # found out the maximum case number for each batch and save as max_case_number
  bynum_id=tapply(dataset$num_id, dataset$division, max)
  bycase_number=tapply(dataset$clean_case_number, dataset$division, max)
  iddata = data.frame(division=names(bynum_id),max_case_number=bycase_number)
  dataset= merge(dataset,iddata,by="division")
  
  # Calculate mvue and bayesian german-tank estimation and confidence interval for mvue method.
  dataset$mvue = dataset$max_case_number+dataset$max_case_number/dataset$case_in_division-1
  dataset$bayesian = ((dataset$case_in_division-1)/(dataset$case_in_division-2)*(dataset$max_case_number-1))
  dataset$availability = dataset$case_in_division/dataset$mvue
  dataset$ci_upper = dataset$max_case_number*20^(1/dataset$case_in_division) - dataset$max_case_number
  return(dataset)
}

getKeydates = function(dataset){
  
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
  tempdate = data.frame(initial_startdate=tempdate$startdays,id=tempdate$id)
  dataset= merge(dataset,tempdate,by="id",all.x = TRUE)
  
  #get filing days for each cases (from Jan.1st) & remove cases 起诉时间
  
  tempdate = data.frame(startdate=dataset$converted_filing,year_id=dataset$year_id,id=dataset$id)
  tempdate = tempdate[as.character(tempdate$startdate)!="",]
  tempdate$startdate=as.Date(tempdate$startdate)
  tempyear = as.Date(paste(as.character(tempdate$year_id),"-01-01",sep = ""),"%Y-%m-%d")
  tempdate$startdays = as.numeric(tempdate$startdate-tempyear)
  tempdate = data.frame(filingdate=tempdate$startdays,id=tempdate$id)
  dataset= merge(dataset,tempdate,by="id",all.x = TRUE)
  
  # if there is no 立案时间，use 起诉时间
  dataset$startdays = ifelse(is.na(dataset$initial_startdate), dataset$filingdate, dataset$initial_startdate)
  
  # Change startdays which are 60 days behind the year (because some courts start using new sequence at the end of the year) 
  # and 365 days after Jan. 1st to NA
  dataset$startdays[dataset$startdays <= -60] = NA
  dataset$startdays[dataset$startdays > 365] = NA
  
  return(dataset)
}

getting_groundTruth <- function (dataset,tribunal){
  uncleanedtemp = subset(dataset,(division == tribunal)&!is.na(adjust_startdate))
  #removing outliners
  jpeg(paste("2013admin/plot/loess1/", as.character(unique(uncleanedtemp$division)),".jpg",sep=""))
  plot(adjust_startdate ~ clean_case_number, data = uncleanedtemp,ylim = c(0,250),xlim = c(0,(max(uncleanedtemp$max_case_number)+30)))
  lo <- try(loess.smooth(uncleanedtemp$clean_case_number, uncleanedtemp$adjust_startdate,span = 0.5), silent = TRUE)
  if (inherits(lo,"try-error")){
    return(uncleanedtemp)
  }
  lines(lo$x, lo$y, lwd = 3,col = 'red')
  lines(lo$x, lo$y + 37.5, lwd = 3, col = 'blue')
  lines(lo$x, lo$y - 37.5, lwd = 3, col = 'blue')
  f1 <- approxfun(lo$x, lo$y + 37.5)
  wh1 <- which(uncleanedtemp$adjust_startdate > f1(uncleanedtemp$clean_case_number))
  f2 <- approxfun(lo$x, lo$y - 37.5)
  wh2 <- which(uncleanedtemp$adjust_startdate < f2(uncleanedtemp$clean_case_number))
  ## identify points to exclude
  exclude <- uncleanedtemp[c(wh1, wh2), ]
  points(exclude$clean_case_number, exclude$adjust_startdate, pch = 4, col = 2, cex = 2)
  dev.off()
  if (length(exclude$startdays)>0){
    temp <- uncleanedtemp[-c(wh1, wh2), ]
  }else{
    temp = uncleanedtemp
  }
  return(temp)
}

getting_groundTruth2 <- function (dataset,tribunal){
  uncleanedtemp = subset(dataset,division == tribunal&!is.na(adjust_startdate))
  t=quantile(uncleanedtemp$clean_case_number)
  gap = 0.15*(t['25%']+t['50%']+t['75%'])/1.5
  #removing outliners
  jpeg(paste("2013admin/plot/loess2/", as.character(unique(uncleanedtemp$division)),".jpg",sep=""))
  plot( clean_case_number ~ adjust_startdate, data = uncleanedtemp,xlim = c(0,250),ylim = c(0,(max(uncleanedtemp$max_case_number)+30)))
  lo <- try(loess.smooth(uncleanedtemp$adjust_startdate,uncleanedtemp$clean_case_number,span = 0.5), silent = TRUE)
  if (inherits(lo,"try-error")){
    return(uncleanedtemp)
  }
  lines(lo$x, lo$y, lwd = 3,col = 'red')
  lines(lo$x, lo$y + gap, lwd = 3, col = 'blue')
  lines(lo$x, lo$y - gap, lwd = 3, col = 'blue')
  f1 <- try(approxfun(lo$x, lo$y + gap))
  if (inherits(f1,"try-error")){
    return(uncleanedtemp)
  }
  wh1 <- which(uncleanedtemp$clean_case_number > f1(uncleanedtemp$adjust_startdate))
  f2 <- approxfun(lo$x, lo$y - gap)
  wh2 <- which(uncleanedtemp$clean_case_number < f2(uncleanedtemp$adjust_startdate))
  ## identify points to exclude
  exclude <- uncleanedtemp[c(wh1, wh2), ]
  points(exclude$adjust_startdate,exclude$clean_case_number, pch = 4, col = 2, cex = 2)
  dev.off()
  if (length(exclude$startdays)>0){
    temp <- uncleanedtemp[-c(wh1, wh2), ]
  }else{
    temp = uncleanedtemp
  }
  return(temp)
}

removeOutlier = function(dataset){
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
        ground_truth1 = getting_groundTruth(Workday,division_list[i])
        flag = flag +1
      }else{
        ground_truth1 = rbind(ground_truth1,getting_groundTruth(Workday,division_list[i]))
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
        ground_truth_filter2 = getting_groundTruth2(ground_truth1,division_list[i])
        flag = flag +1
      }else{
        ground_truth_filter2 = rbind(ground_truth_filter2,getting_groundTruth2(ground_truth1,division_list[i]))
      }
    }else{
      if (flag ==1){
        ground_truth_filter2 = dat
        flag = flag +1
      }else{ground_truth_filter2 = rbind(ground_truth_filter2,dat)}
    }}
  
  return(ground_truth_filter2)
  
}


adjustStartDate = function(dataset,workday,year){
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

linearEstimator = function(dataset){
  #Estimate total # of cases using linear regression
  division_list = dataset[!duplicated(dataset$division),]$division
  linearEstimation = data.frame(division = c(),availStart_in_batch = c(),linearEstimation = c(),lower_bound = c(), upper_bound = c(),r_squared = c(), white_test = c())
  
  
  for (i in 1:length(division_list)){
    if (i%%100 ==0){print(i)}
    target_batch = subset(dataset,division == division_list[i])
    temp_model = try(lm(adjust_startdate ~ clean_case_number + 0, data = target_batch), silent = TRUE)
    if (!inherits(temp_model,"try-error")){
      confint.default(temp_model)[2]
      wt = bptest(temp_model, ~ clean_case_number + I(clean_case_number^2), data = target_batch)
      temp = data.frame(division = division_list[i],availStart_in_batch = length(target_batch$adjust_startdate),linearEstimation = round(250/as.numeric(coefficients(temp_model))),lower_bound = round(250/confint.default(temp_model)[2]), upper_bound = round(250/confint.default(temp_model)[1]),r_squared = summary(temp_model)[["r.squared"]],white_test_p_value = wt[["p.value"]])
      linearEstimation = rbind(linearEstimation,temp)
    }
  }
  
  return(linearEstimation)
}




  
 
  

  






