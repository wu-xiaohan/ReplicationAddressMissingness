# functions for estimating time to decision. loessfitting()

loessfitting = function(dat,s){
  temp = dat
  fit = loess(adjust_startdate ~ clean_case_number, temp,span = s,control=loess.control(surface="direct"))
  #summary(fit)
  clean_case_numberlims<-range(temp$clean_case_number)
  #Generating Test Data
  clean_case_number.grid<-seq(from=clean_case_numberlims[1], to = clean_case_numberlims[2])
  #Plotting the Regression Line to the scatterplot   
  jpeg(paste("2015admin/plot/final_fit/", as.character(unique(dat$division)),".jpg",sep=""))
  plot(temp$clean_case_number,temp$adjust_startdate,col="grey",ylab="Work Day Number",xlab="Case Number")
  pred = predict(fit,data.frame(clean_case_number = clean_case_number.grid))
  points(clean_case_number.grid,pred,col="darkgreen",lwd=2,type="l")
  dev.off()
  return(fit)
}

loess.predict <- function(fit, newdata) {
  predict(fit, newdata = newdata)
}

loess.model = function(x, y, span){
  loess(y ~ x, span = span, control=loess.control(surface="direct"))
}

loess_wrapper <- function (dataset,tribunal, starting){
  temp = subset(dataset,division==tribunal&!is.na(adjust_startdate))
  folds = length(temp$converted_date)
  #print(folds)
  #choosing span
  y = temp$adjust_startdate
  x = temp$clean_case_number
  span.index = 0
  starting_span = starting
  span.vals = seq(starting_span, 1, by = 0.05)
  #mean absolute error for span selection
  # Quantify error for each span, using CV
  mean.abs.error = length(span.vals)
  for (each.span in span.vals) {
    span.index = span.index + 1
    if (starting_span<0.8){
      y.hat.cv = try(crossval(x, y, theta.fit = loess.model, theta.predict = loess.predict, span = each.span, ngroup = folds)$cv.fit, silent=TRUE)
      if (inherits(y.hat.cv,"try-error")){
        starting_span = starting_span + 0.05
        return(loess_wrapper(dataset,tribunal,starting_span))
      }
      non.empty.indices = !is.na(y.hat.cv)
      mean.abs.error[span.index] <- mean(abs(y[non.empty.indices] - y.hat.cv[non.empty.indices]))
    }else{
      return(list("failed","failed","failed"))
    }
  }
  jpeg(paste("2015admin/plot/choosing_span/", as.character(unique(dat$division)),".jpg",sep=""))
  best.span = span.vals[which.min(mean.abs.error)]
  min_error = min(mean.abs.error)
  plot(span.vals,mean.abs.error,type="l",xlim = c(0,0.8),col="red",main = str(best.span))
  dev.off()
  # find the span which minimizes error
  # fit the best model
  best.model <- loessfitting(temp,best.span)
  return(list(best.model,best.span,min_error))
}

