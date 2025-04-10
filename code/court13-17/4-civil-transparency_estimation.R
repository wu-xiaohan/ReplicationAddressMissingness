# This script calculate the transparency for civil litigations



dataClean = function(dataset){
  
  #### 民初 clean ######
  dataset = subset(dataset,court_division != '刑初')
  dataset = subset(dataset,court_division != '行初')
  dataset = subset(dataset,court_division != '刑环保初')
  dataset = subset(dataset,court_division != '行赔初')
  dataset = subset(dataset,court_division != '民再初')
  dataset = subset(dataset,court_division != '申(预)初')
  dataset = subset(dataset,court_division != '刑环保初')
  dataset = subset(dataset,court_division != '行赔初')
  dataset = subset(dataset,court_division != '民再初')
  dataset = subset(dataset,court_division != '邢初')
  dataset = subset(dataset,court_division != '民一再初')
  dataset = subset(dataset,court_division != '审民初再')
  dataset = subset(dataset,court_division != '审民再初')
  dataset = subset(dataset,court_division != '审行初再')
  dataset = subset(dataset,court_division != '特初')
  dataset = subset(dataset,court_division != '执初')
  dataset = subset(dataset,court_division != '审刑初再')
  dataset = subset(dataset,court_division != '知刑初')
  dataset = subset(dataset,court_division != '民一特初')
  dataset = subset(dataset,court_division != '少刑初')
  dataset = subset(dataset,court_division != '再重初')
  dataset = subset(dataset,court_division != '经重初')
  dataset = subset(dataset,court_division != '再刑初')
  dataset = subset(dataset,court_division != '民监初')
  dataset = subset(dataset,court_division != '刑重初')
  dataset = subset(dataset,court_division != '经开民再初')
  dataset = subset(dataset,court_division != '特清算初')
  dataset = subset(dataset,court_division != '再初')
  dataset = subset(dataset,court_division != '行立初')
  dataset = subset(dataset,court_division != '行赔初')
  dataset = subset(dataset,court_division != '刑初(重)')
  dataset = subset(dataset,court_division != '民民再初')
  dataset = subset(dataset,court_division != '刑再初')
  dataset = subset(dataset,court_division != '经开行初')
  dataset = subset(dataset,court_division != '经开民再初')
  dataset = subset(dataset,court_division != '执异初')
  dataset = subset(dataset,court_division != '执分初')
  dataset = subset(dataset,court_division != '刑民一初')
  dataset = subset(dataset,court_division != '刑一初')
  dataset = subset(dataset,court_division != '刑重初')
  dataset = subset(dataset,court_division != '行初审')
  dataset = subset(dataset,court_division != '经开刑初')
  dataset = subset(dataset,court_division != '再民初')
  dataset = subset(dataset,court_division != '刑监初')
  dataset = subset(dataset,court_division != '行重初')
  dataset = subset(dataset,court_division != '民再抗初')
  dataset = subset(dataset,court_division != '知刑初')
  dataset = subset(dataset,court_division != '民二民再初')
  dataset = subset(dataset,court_division != '监商再初')
  dataset = subset(dataset,court_division != '刑二初')
  dataset = subset(dataset,court_division != '民重再初')
  dataset = subset(dataset,court_division != '建始刑初')
  dataset = subset(dataset,court_division != '邢立行初')
  dataset = subset(dataset,court_division != '刑诉初')
  dataset = subset(dataset,court_division != '邢刑初')
  dataset = subset(dataset,court_division != '立刑初')
  dataset = subset(dataset,court_division != '审刑初再')
  dataset = subset(dataset,court_division != '审二民初再')
  dataset = subset(dataset,court_division != '民二终初')
  dataset = subset(dataset,court_division != '民二初终')
  dataset = subset(dataset,court_division != '行再初')
  dataset = subset(dataset,court_division != '行受初')
  dataset = subset(dataset,court_division != '刑受初')
  dataset = subset(dataset,court_division != '刑立初')
  dataset = subset(dataset,court_division != '民再初重')
  dataset = subset(dataset,court_division != '民一终初')
  dataset = subset(dataset,court_division != '刑少初')
  dataset = subset(dataset,court_division != '刑自初')
  dataset = subset(dataset,court_division != '民初再')
  dataset = subset(dataset,court_division != '监民再初')
  dataset = subset(dataset,court_division != '民初终')
  dataset = subset(dataset,court_division != '民再初')
  dataset = subset(dataset,court_division != '行赔初')
  dataset = subset(dataset,court_division != '刑民初')
  dataset = subset(dataset,court_division != '民再初')
  dataset = subset(dataset,court_division != '行赔初')
  
  dataset$clean_division = '民初'
  
  #remove cases with missing metadata
  dataset = dataset[dataset$std_court!="",] 
  dataset = dataset[dataset$case_number_id!="",]
  
  # create division(batch) = year-court-court_division
  dataset$division <- paste(dataset$year_id, dataset$std_court, dataset$clean_division, sep=" ")
  dataset$num_id=as.numeric(as.factor(dataset$division))
  dataset=dataset[rev(order(dataset$division, dataset$clean_case_number, dataset$converted_date)),]
  dataset=dataset[!duplicated(dataset[,c('clean_case_number','division')]),] # further remove dups with same case_id
  dataset$division=as.factor(dataset$division)
  
  # count how many cases in one division - save as case_in_division
  bydivision <- tapply(dataset$clean_case_number, dataset$division, length)
  iddata = data.frame(division=names(bydivision),case_in_division=bydivision)
  dataset= merge(dataset,iddata,all.x = TRUE,by="division")
  
  # Merge additional court information
  dataset= merge(dataset,court,all.x = TRUE,by="std_court")
  
  
  # remove cases with unrealistic case_number
  # dataset = subset(dataset,clean_case_number<70000)
  # dataset = subset(dataset,clean_case_number>=0)
  
  return(dataset)
}

getGermanTankSolution = function(dataset){
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

getCoarsenGTS = function(dataset,binsize){
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

# clean dataset
cleanData = dataClean(rawdata)


courtNumber = cleanData$std_court[!duplicated(cleanData$std_court)]
batchNumber = cleanData$division[!duplicated(cleanData$division)]

print(paste0("Number of distinct courts from 2013 to 2017 in civil dataset:",length(courtNumber)))
print(paste0("Number of distinct batches from 2013 to 2017 in civil dataset:",length(batchNumber)))

# get german tank solution
GTS = getGermanTankSolution(cleanData)
GTS = getCoarsenGTS(GTS,100)

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


write.csv(kstest,'kstestmvuecivil13.csv')
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


write.csv(kstest,'kstestCoarsencivil13.csv')
GTS = merge(GTS,kstest,by='division')


# Kth removed MVUE
GTS_temp = subset(GTS,(case_in_division > 10)&(ks_p_value_mvue<0.05))
GTS_temp$average_gap = (GTS_temp$max_case_number - GTS_temp$case_in_division)/GTS_temp$case_in_division
GTS_temp = subset(GTS_temp,average_gap>30)

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
GTS = GTS[!duplicated(GTS$division),]
GTS = merge(GTS,GTS_throw,all.x = TRUE, on = 'divisions')
