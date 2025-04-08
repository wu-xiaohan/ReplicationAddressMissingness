# This file contains replication code for estimating time to decision
# in criminal and administrative litigations from 2013 to 2018 in Chinese Courts.

# The code can also run on data/court13-18/temp_file_for_ttd/admin13-18-all.csv

# Load data
GTS = read.csv('data/court13-18/temp_file_for_ttd/criminal13-18-all.csv')

GTS$adjust_startdate = as.numeric(as.character(GTS$adjust_startdate))

input_data = read.csv('data/court13-18/temp_file_for_ttd/criminal13-18temp.csv')

input_data = subset(input_data,court_division == '刑初') # use 行初 for administrative cases


# Functions
loess_fitting = function(dat,s){
  temp = dat
  fit = loess(adjust_startdate ~ clean_case_number, temp,span = s,control=loess.control(surface="direct"))
  clean_case_numberlims<-range(temp$clean_case_number)
  # Generating Test Data
  clean_case_number.grid<-seq(from=clean_case_numberlims[1], to = clean_case_numberlims[2])
  # Uncomment to plot
  # Plotting the Regression Line to the scatterplot   
  # jpeg(paste("data/image/", as.character(unique(dat$division)),".jpg",sep=""))
  # plot(temp$clean_case_number,temp$adjust_startdate,col="grey",ylab="Work Day Number",xlab="Case Number")
  pred = predict(fit,data.frame(clean_case_number = clean_case_number.grid))
  # points(clean_case_number.grid,pred,col="darkgreen",lwd=2,type="l")
  #dev.off()
  return(fit)
}

loess_predict <- function(fit, newdata) {
  predict(fit, newdata = newdata)
}

loess_model <- function(x, y, span){
  loess(y ~ x, span = span, control=loess.control(surface="direct"))
}

loess_wrapper <- function (dataset,tribunal, starting){
  temp = subset(dataset,division==tribunal&!is.na(adjust_startdate))
  if (length(temp$converted_date)>50){
    folds = 50
  }else{
    folds = length(temp$converted_date)
  }
  # Choosing span
  y = temp$adjust_startdate
  x = temp$clean_case_number
  span.index = 0
  starting_span = starting
  span.vals = seq(starting_span, 1, by = 0.05)
  # Mean absolute error for span selection
  # Quantify error for each span, using CV
  mean.abs.error = length(span.vals)
  for (each.span in span.vals) {
    span.index = span.index + 1
    if (starting_span < 0.7){
      y.hat.cv = try(crossval(x, y, theta.fit = loess_model, theta.predict = loess_predict, span = each.span, ngroup = folds)$cv.fit, silent=TRUE)
      if (inherits(y.hat.cv,"try-error")){
        print(starting_span)
        starting_span = starting_span + 0.05
        return(loess_wrapper(dataset,tribunal,starting_span))
      }
      non.empty.indices = !is.na(y.hat.cv)
      mean.abs.error[span.index] <- mean(abs(y[non.empty.indices] - y.hat.cv[non.empty.indices]))
    }else{
      return(list("failed","failed","failed"))
    }
  }
  # jpeg(paste("data/image/", as.character(unique(dat$division)),".jpg",sep=""))
  best.span = span.vals[which.min(mean.abs.error)]
  min_error = min(mean.abs.error)
  # plot(span.vals,mean.abs.error,type="l",xlim = c(0,0.8),col="red",main = str(best.span))
  # dev.off()
  # find the span which minimizes error
  # fit the best model
  best.model <- loess_fitting(temp,best.span)
  return(list(best.model,best.span,min_error))
}

division_list = as.character(input_data[!duplicated(input_data$division),]$division)
failed_divisions = c()
flag = 1
for (i in 1:length(division_list)){
  dat = subset(input_data,division == as.character(division_list[i]))
  print(i)
  # input = subset(input2013,division == as.character(division_list[i])&!is.na(input2013$decisiondays))
  if (length(dat$decisiondays) < 5){
    failed_divisions = append(failed_divisions,division_list[i])
    # jpeg(paste("2013criminal/plot/failed_batch/", as.character(unique(dat$division)),".jpg",sep=""))
    # plot(dat$clean_case_number,dat$startdays,xlab = "Cleaned Case Number",
    #     ylab = paste("Days since start of",as.character(unique(dat$year_id)),sep = " "),
    #     main = paste(as.character(unique(dat$division)),division_list[i],'  ',as.character(length(dat$days)),sep = ""),family = "STKaiti")
    #points(as.numeric(input$clean_case_number),as.numeric(input$decisiondays),col="red",pch=13)
    #dev.off()
  }else{
    best_fit = loess_wrapper(input_data,division_list[i],0.05)
    if (best_fit[[1]] != "failed"){
      test_clean_case_number = subset(GTS,division == as.character(division_list[i]))
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
      # Uncomment to plot
      # jpeg(paste("2013criminal/plot/failed_batch/", as.character(unique(dat$division)),".jpg",sep=""))
      # plot(dat$clean_case_number,dat$startdays,xlab = "Cleaned Case Number",
      #     ylab = paste("days since start of",as.character(unique(dat$year_id)),sep = " "),
      #     main = paste(as.character(unique(dat$division)),division_list[i],'  ',as.character(length(dat$days)),sep = ""),family = "STKaiti")
      # points(as.numeric(input$clean_case_number),as.numeric(input$decisiondays),col="red",pch=13)
      # dev.off()
    }
  }
}


write.csv(final_pred,'/Users/wuxiaohan/Dropbox/Research/TTD-replication_final/data/court13-18/final_estimates/criminalTTDpred.csv')





