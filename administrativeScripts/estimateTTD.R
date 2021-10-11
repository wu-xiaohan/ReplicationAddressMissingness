## loess cross-validation
library(bootstrap)
library(forecast)
data2013 = read.csv('2013admin/temp2013.csv')

input2013 = subset(GTS,(year_id == 2013)&(court_division == '行初'))
input2013 = subset(input2013,decisiondays >= -60)
input2013 = adjustStartDate(input2013,workday,2013)
input2013$adjust_startdate = as.numeric(as.character(input2013$adjust_startdate))
data2013 = subset(data2013,court_division == '行初')

division_list = as.character(data2013[!duplicated(data2013$division),]$division)
failed_divisions = c()
flag = 1
for (i in 1:length(division_list)){
  dat = subset(data2013,division == as.character(division_list[i]))
  print(i)
  input = subset(input2013,division == as.character(division_list[i])&!is.na(input2013$decisiondays))
  if (length(dat$decisiondays) < 10){
    failed_divisions = append(failed_divisions,division_list[i])
    jpeg(paste("2013admin/plot/failed_batch/", as.character(unique(dat$division)),".jpg",sep=""))
    plot(dat$clean_case_number,dat$startdays,xlab = "Cleaned Case Number",
         ylab = paste("Days since start of",as.character(unique(dat$year_id)),sep = " "),
         main = paste(as.character(unique(dat$division)),division_list[i],'  ',as.character(length(dat$days)),sep = ""),family = "STKaiti")
    points(as.numeric(input$clean_case_number),as.numeric(input$decisiondays),col="red",pch=13)
    dev.off()
  }else{
    best_fit = loess_wrapper(data2013,division_list[i],0.05)
    if (best_fit[[1]] != "failed"){
      test_clean_case_number = subset(input2013,division == as.character(division_list[i]))
      pred = predict(best_fit[[1]],data.frame(clean_case_number = test_clean_case_number$clean_case_number))
      temp_output = data.frame(filename = test_clean_case_number$filename,truth = test_clean_case_number$adjust_startdate, pred_filing = pred, best_span = best_fit[[2]], min_error = best_fit[[3]])
      if (flag == 1){
        final_pred = temp_output
        flag = flag + 1
      }else{
        final_pred = rbind(final_pred,temp_output)
      }
    }else{
      failed_divisions = append(failed_divisions,division_list[i])
      jpeg(paste("2013admin/plot/failed_batch/", as.character(unique(dat$division)),".jpg",sep=""))
      plot(dat$clean_case_number,dat$startdays,xlab = "Cleaned Case Number",
           ylab = paste("days since start of",as.character(unique(dat$year_id)),sep = " "),
           main = paste(as.character(unique(dat$division)),division_list[i],'  ',as.character(length(dat$days)),sep = ""),family = "STKaiti")
      points(as.numeric(input$clean_case_number),as.numeric(input$decisiondays),col="red",pch=13)
      dev.off()
    }
  }
}

write.csv(final_pred,'adminTTD2013pred.csv')

# repeat the process for year 2014-2017.


