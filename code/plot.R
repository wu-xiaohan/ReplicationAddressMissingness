# This file generates Figures in paper.

library(ggplot2)

######################################## Figure 1 ########################################

liuan <- read.csv(file = 'data/dataforplots/六安市金安区人民法院行政一审2016.csv',header = TRUE, sep = ',')
liuan <- subset(liuan,!is.na(adjust_startdate))
liuan$case_number_id=as.numeric(liuan$case_number_id)
liuan$adjust_startdate=as.numeric(liuan$adjust_startdate)
linear_model <- lm(case_number_id ~ 0+adjust_startdate,data = liuan)
#predict(linear_model, newdata = data.frame(adjust_startdate = 251), interval = "none") #62.95531 
# Open PNG device
png("Figure1", width = 800, height = 600, res = 120)

# Your plotting code
plot(liuan$adjust_startdate, liuan$case_number_id, 
     xlim = c(0, 260), ylim = c(0, 70), 
     xlab = "Working Day", ylab = "Case Number", 
     pch = 19)

abline(linear_model, col = "red")
abline(v = 251, lty = 2)
abline(h = 63, lty = 2)
points(251, 63, pch = 19, col = "red")
text(40, 50, "Average number of cases filed per day  = 0.25", pos = 4, col = "red") 
text(205, 65, "Est. total: 63", pos = 4, col = "red")

# Close the device to write the file
dev.off()

######################################## Figure 2 ########################################

### import data
criminal = read.csv(file = 'data/court13-17/final_estimates/criminal_transparency_final.csv')
criminal_validation = read.csv(file = 'data/dataforplots/criminal_validation_plt_new.csv')
criminal_validation$division  = paste(criminal_validation$Year,criminal_validation$法院名称,'刑初')
criminal_validation = merge(criminal_validation,criminal,by = 'division', all.x = TRUE)
criminal_validation$gtp_percent_error = (abs(criminal_validation$mvue - criminal_validation$受理刑事诉讼)/criminal_validation$受理刑事诉讼)
criminal_validation$regre_percent_error = (abs(criminal_validation$linearEstimation - criminal_validation$受理刑事诉讼)/criminal_validation$受理刑事诉讼)

criminal_v_plot = criminal_validation
criminal_v_plot$esti = criminal_v_plot$mvue
criminal_v_plot$Method = 'MVUE'
criminal_v_plot$ci_low = criminal_v_plot$max_case_number
criminal_v_plot$ci_upper = criminal_v_plot$mvue_upper
criminal_v_plot1 = criminal_validation
criminal_v_plot1$esti = criminal_v_plot1$linearEstimation
criminal_v_plot1$Method = 'Linear'
criminal_v_plot1$ci_low = criminal_v_plot1$lower_bound_prediction
criminal_v_plot1$ci_upper = criminal_v_plot1$upper_bound_prediction
criminal_v_plot = rbind(criminal_v_plot,criminal_v_plot1)

p = ggplot(criminal_v_plot, aes(x = 受理刑事诉讼, y = esti, group = Method)) +
  geom_point(size = 2,aes(shape=Method,color = Method),fill="white") +
  geom_errorbar(aes(ymax = ci_upper, ymin = ci_low))  + ggtitle("Criminal Courts Validation") + ylim(0, 3500) + xlim(0, 3500) +
  xlab("Number of Cases from Work Reports") + ylab("Estimation") + scale_color_manual(values = c("red", "blue", "green")) + theme_bw() + theme(
    plot.title = element_text(size=20,hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.ticks.length=unit(.2, "cm"),
    axis.text.x = element_text(size=18, angle=0),
    axis.text.y = element_text(size=18, angle=0)
  ) + geom_abline(intercept = 0, slope = 1, color="black", size=0.5) + theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18))
p
ggsave("Figure2-1.png", plot = p, width = 8, height = 6, dpi = 300)
criminal_v_plot = criminal_validation
criminal_v_plot$error_percent = criminal_v_plot$gtp_percent_error*100
criminal_v_plot$Method = 'MVUE'
criminal_v_plot1 = criminal_validation
criminal_v_plot1$Method = 'Linear'
criminal_v_plot1$error_percent = criminal_v_plot1$regre_percent_error*100
criminal_v_plot = rbind(criminal_v_plot,criminal_v_plot1)

p = ggplot(criminal_v_plot, aes(x = ks_p_value_mvue, y = error_percent, group = Method)) +
  geom_point(size = 2,aes(shape=Method,color = Method),fill="white") + ggtitle("Uniformity Test of Criminal Courts") +
  xlab("P-value of Uniformity Test") + ylab("Percent Error") + scale_color_manual(values = c("red", "blue", "green")) + theme_bw() + theme(
    plot.title = element_text(size=20,hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.ticks.length=unit(.2, "cm"),
    axis.text.x = element_text(size=18, angle=0),
    axis.text.y = element_text(size=18, angle=0)
  ) + geom_vline(xintercept = 0.05) + theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18))

p + annotate("text", x = 0.27, y = 200, label = "0.05 p-value threshold", size = 6)
ggsave("Figure2-2.png", plot = p, width = 8, height = 6, dpi = 300)
# median error percentage for MVUE
median(criminal_v_plot1$gtp_percent_error,na.rm = TRUE)
# median error percentage for linear regression
median(criminal_v_plot1$regre_percent_error,na.rm = TRUE)
#95% coverage
test = subset(criminal_validation,(受理刑事诉讼>=max_case_number)&(受理刑事诉讼<=mvue_upper))
length(test$division)/52 #mvue
test = subset(criminal_validation,(受理刑事诉讼>=lower_bound_prediction)&(受理刑事诉讼<=upper_bound_prediction))
length(test$division)/50 #linear


######################################## Figure 3 ########################################
# import data
admin = read.csv(file = 'data/court13-17/final_estimates/admin_transparency_final.csv')
admin_validation = read.csv(file = 'data/dataforplots/adm_validation_plt_new.csv')
admin_validation$division  = paste(admin_validation$Year,admin_validation$法院名称,'行初')
admin_validation = merge(admin_validation,admin,by = 'division', all.x = TRUE)
admin_validation$gtp_percent_error = (abs(admin_validation$mvue - admin_validation$受理行政诉讼)/admin_validation$受理行政诉讼)
admin_validation$regre_percent_error = (abs(admin_validation$linearEstimation - admin_validation$受理行政诉讼)/admin_validation$受理行政诉讼)

## Zoomed Validation Plot
admin_v_plot = admin_validation
admin_v_plot$esti = admin_v_plot$mvue
admin_v_plot$Method = 'MVUE'
admin_v_plot$ci_low = admin_v_plot$max_case_number
admin_v_plot$ci_upper = admin_v_plot$mvue_upper
admin_v_plot1 = admin_validation
admin_v_plot1$esti = admin_v_plot1$linearEstimation
admin_v_plot1$Method = 'Linear'
admin_v_plot1$ci_low = admin_v_plot1$lower_bound_prediction
admin_v_plot1$ci_upper = admin_v_plot1$upper_bound_prediction
admin_v_plot = rbind(admin_v_plot,admin_v_plot1)

p = ggplot(admin_v_plot, aes(x = 受理行政诉讼, y = esti, group = Method)) +
  geom_point(size = 2,aes(shape=Method,color = Method),fill="white") +
  geom_errorbar(aes(ymax = ci_upper, ymin = ci_low)) + ggtitle("Administrative Courts Validation") + ylim(0, 500) + xlim(0, 500) +
  xlab("Number of Cases from Work Reports") + ylab("Estimation") + scale_color_manual(values = c("red", "blue", "green")) + theme_bw() + theme(
    plot.title = element_text(size=20,hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.ticks.length=unit(.2, "cm"),
    axis.text.x = element_text(size=18, angle=0),
    axis.text.y = element_text(size=18, angle=0)) + geom_abline(intercept = 0, slope = 1, color="black", size=0.5) + theme(
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 18))
p
ggsave("Figure3-1.png", plot = p, width = 8, height = 6, dpi = 300)

## Uniformity test of Administrative Court
admin_v_plot = admin_validation
admin_v_plot$error_percent = admin_v_plot$gtp_percent_error*100
admin_v_plot$Method = 'MVUE'
admin_v_plot1 = admin_validation
admin_v_plot1$Method = 'Linear'
admin_v_plot1$error_percent = admin_v_plot1$regre_percent_error*100
admin_v_plot = rbind(admin_v_plot,admin_v_plot1)

p = ggplot(admin_v_plot, aes(x = ks_p_value_mvue, y = error_percent, group = Method)) +
  geom_point(size = 2,aes(shape=Method,color = Method),fill="white") + ggtitle("Uniformity Test of Administrative Courts") +
  xlab("P-value of Uniformity Test") + ylab("Percent Error") + scale_color_manual(values = c("red", "blue", "green")) + theme_bw() + theme(
    plot.title = element_text(size=20,hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.ticks.length=unit(.2, "cm"),
    axis.text.x = element_text(size=18, angle=0),
    axis.text.y = element_text(size=18, angle=0)
  ) + geom_vline(xintercept = 0.05) + theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18))
p + annotate("text", x = 0.27, y = 120, label = "0.05 p-value threshold", size = 6)
ggsave("Figure3-2.png", plot = p, width = 8, height = 6, dpi = 300)

# median error percentage for MVUE
median(admin_v_plot1$gtp_percent_error,na.rm = TRUE)
# median error percentage for linear regression
median(admin_v_plot1$regre_percent_error,na.rm = TRUE)
#95% coverage
test = subset(admin_validation,(受理行政诉讼>=max_case_number)&(受理行政诉讼<=mvue_upper))
length(test$division)/32 #mvue
test = subset(admin_validation,(受理行政诉讼>=lower_bound_prediction)&(受理行政诉讼<=upper_bound_prediction))
length(test$division)/27 #linear

######################################## Figure 4 ########################################

### import data
civil = read.csv(file = 'data/court13-17/final_estimates/civil_transparency_final.csv')
civil_validation = read.csv(file = 'data/dataforplots/civil_validation_plt_new.csv')
civil_validation$division  = paste(civil_validation$Year,civil_validation$法院名称,'民初')
civil_validation = merge(civil_validation,civil,by = 'division', all.x = TRUE)
civil_validation$gtp_percent_error = (abs(civil_validation$mvue - civil_validation$民事案件受理总数)/civil_validation$民事案件受理总数)
civil_validation$regre_percent_error = (abs(civil_validation$linearEstimation - civil_validation$民事案件受理总数)/civil_validation$民事案件受理总数)

civil_v_plot = civil_validation
civil_v_plot$esti = civil_v_plot$mvue
civil_v_plot$Method = 'MVUE'
civil_v_plot$ci_low = civil_v_plot$max_case_number
civil_v_plot$ci_upper = civil_v_plot$mvue_upper
civil_v_plot1 = civil_validation
civil_v_plot1$esti = civil_v_plot1$linearEstimation
civil_v_plot1$Method = 'Linear'
civil_v_plot1$ci_low = civil_v_plot1$lower_bound_prediction
civil_v_plot1$ci_upper = civil_v_plot1$upper_bound_prediction
civil_v_plot = rbind(civil_v_plot,civil_v_plot1)

p = ggplot(civil_v_plot, aes(x = 民事案件受理总数, y = esti, group = Method)) +
  geom_point(size = 2,aes(shape=Method,color = Method),fill="white") +
  geom_errorbar(aes(ymax = ci_upper, ymin = ci_low))  + ggtitle("Civil Courts Validation") + ylim(-1050, 32000) + xlim(0, 32000) +
  xlab("Number of Cases from Work Reports") + ylab("Estimation") + scale_color_manual(values = c("red", "blue", "green")) + theme_bw() + theme(
    plot.title = element_text(size=20,hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.ticks.length=unit(.2, "cm"),
    axis.text.x = element_text(size=18, angle=0),
    axis.text.y = element_text(size=18, angle=0)
  ) + geom_abline(intercept = 0, slope = 1, color="black", size=0.5) + theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18))
p
ggsave("Figure4-1.png", plot = p, width = 8, height = 6, dpi = 300)
civil_v_plot = civil_validation
civil_v_plot$error_percent = civil_v_plot$gtp_percent_error*100
civil_v_plot$Method = 'MVUE'
civil_v_plot1 = civil_validation
civil_v_plot1$Method = 'Linear'
civil_v_plot1$error_percent = civil_v_plot1$regre_percent_error*100
civil_v_plot = rbind(civil_v_plot,civil_v_plot1)

p = ggplot(civil_v_plot, aes(x = ks_p_value_mvue, y = error_percent, group = Method)) +
  geom_point(size = 2,aes(shape=Method,color = Method),fill="white") +  ggtitle("Uniformity Test of Civil Courts") +
  xlab("P-value of Uniformity Test") + ylab("Percent Error") + scale_color_manual(values = c("red", "blue", "green")) + theme_bw() + theme(
    plot.title = element_text(size=20,hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.ticks.length=unit(.2, "cm"),
    axis.text.x = element_text(size=18, angle=0),
    axis.text.y = element_text(size=18, angle=0)
  ) + geom_vline(xintercept = 0.05) + theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18))


p + annotate("text", x = 0.25, y = 200, label = "0.05 p-value threshold", size = 6)
ggsave("Figure4-2.png", plot = p, width = 8, height = 6, dpi = 300)

# median error percentage for MVUE
median(civil_v_plot1$gtp_percent_error,na.rm = TRUE)
# median error percentage for linear regression
median(civil_v_plot1$regre_percent_error,na.rm = TRUE)
#95% coverage
test = subset(civil_validation,(民事案件受理总数>=max_case_number)&(民事案件受理总数<=mvue_upper))
length(test$division)/53 #mvue
test = subset(civil_validation,(民事案件受理总数>=lower_bound_prediction)&(民事案件受理总数<=upper_bound_prediction))
length(test$division)/53 #linear


######################################## Figure 5 ########################################
admin = read.csv(file = 'data/dataforplots/admin_validation_transparency_coarsen.csv')
criminal = read.csv(file = 'data/dataforplots/criminal_validation_transparency_coarsen.csv')
civil = read.csv(file = 'data/dataforplots/civil_validation_transparency_coarsen.csv')

# admin validation
admin_validation = read.csv(file = 'data/dataforplots/adm_validation_plt_new.csv')
admin_validation$division  = paste(admin_validation$Year,admin_validation$法院名称,'行初')
admin_validation = merge(admin_validation,admin,by = 'division', all.x = TRUE)
admin_validation$gtp_percent_error = (abs(admin_validation$mvue - admin_validation$受理行政诉讼)/admin_validation$受理行政诉讼)
admin_validation$coarsed_percent_error = (abs(admin_validation$mvue_coarsen - admin_validation$受理行政诉讼)/admin_validation$受理行政诉讼)


admin_v_plot = admin_validation
admin_v_plot$esti = admin_v_plot$mvue
admin_v_plot$Method = 'MVUE'
admin_v_plot$ci_low = admin_v_plot$max_case_number
admin_v_plot$ci_upper = admin_v_plot$mvue_upper

p <- ggplot(admin_v_plot, aes(x = 受理行政诉讼, y = esti, group = Method)) +
  geom_point(size = 2,aes(shape=Method,color = Method),fill="white") +
  geom_errorbar(aes(ymax = ci_upper, ymin = ci_low)) + ylim(0, 500) + xlim(0, 500) + ggtitle("Administrative Courts Validation") +
  xlab("Number of Cases from Work Reports") + ylab("Estimation") + scale_color_manual(values = c("red", "blue", "green")) + theme_bw() + theme(
    plot.title = element_text(size=20,hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.ticks.length=unit(.2, "cm"),
    axis.text.x = element_text(size=18, angle=0),
    axis.text.y = element_text(size=18, angle=0)
  ) + geom_abline(intercept = 0, slope = 1, color="black", size=0.5) + theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18))
p
ggsave("Figure5-1.png", plot = p, width = 8, height = 6, dpi = 300)

admin_v_plot = admin_validation
admin_v_plot$esti = admin_v_plot$mvue_coarsen
admin_v_plot$Method = 'CoarsenGTS'
admin_v_plot$ci_low = admin_v_plot$max_case_number_coarsen*10
admin_v_plot$ci_upper = admin_v_plot$mvue_upper_coarsen

p <- ggplot(admin_v_plot, aes(x = 受理行政诉讼, y = esti, group = Method)) +
  geom_point(size = 2,aes(shape=Method,color = Method),fill="white") +
  geom_errorbar(aes(ymax = ci_upper, ymin = ci_low)) + ylim(0, 500) + xlim(0, 500) + ggtitle("Administrative Courts Validation") +
  xlab("Number of Cases from Work Reports") + ylab("Estimation") + scale_color_manual(values = c("red", "blue", "green")) + theme_bw() + theme(
    plot.title = element_text(size=20,hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.ticks.length=unit(.2, "cm"),
    axis.text.x = element_text(size=18, angle=0),
    axis.text.y = element_text(size=18, angle=0)
  ) + geom_abline(intercept = 0, slope = 1, color="black", size=0.5) + theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18))
p
ggsave("Figure5-2.png", plot = p, width = 8, height = 6, dpi = 300)

######################################## Figure 6 ########################################

admin_v_plot = admin_validation
admin_v_plot$error_percent = admin_v_plot$gtp_percent_error*100
admin_v_plot$ks_p_value_t = admin_v_plot$ks_p_value
admin_v_plot$Method = 'MVUE'

admin_v_plot1 = admin_validation
admin_v_plot1$error_percent = admin_v_plot1$coarsed_percent_error*100
admin_v_plot1$Method = 'CoarsenGTS'
admin_v_plot1$ks_p_value_t = admin_v_plot1$ks_p_value_coarsen

admin_v_plot = rbind(admin_v_plot,admin_v_plot1)
#admin_v_plot = subset(admin_v_plot,受理行政诉讼<1000)
p = ggplot(admin_v_plot, aes(x = ks_p_value_t, y = error_percent, group = Method)) +
  geom_point(size = 2,aes(shape=Method,color = Method),fill="white") + ggtitle("Uniformity Test of Administrative Courts") +
  xlab("P-value of Uniformity Test") + ylab("Percent Error") + scale_color_manual(values = c("red", "blue", "green")) + theme_bw() + theme(
    plot.title = element_text(size=20,hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.ticks.length=unit(.2, "cm"),
    axis.text.x = element_text(size=18, angle=0),
    axis.text.y = element_text(size=18, angle=0)
  ) + geom_vline(xintercept = 0.05) + theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18))
p + annotate("text", x = 0.3, y = 100, label = "0.05 p-value threshold", size = 6)
ggsave("Figure6.png", plot = p, width = 8, height = 6, dpi = 300)

######################################## Figure 7 ########################################
library(readr)
TransparencyOverTime <- read_csv("data/dataforplots/TransparencyOverTime.csv")

p<-ggplot(data=TransparencyOverTime, aes(x=Year, y=Transparency, group = Type)) + 
  geom_point(size = 4,aes(shape=Type,color = Type)) + geom_line() +
  geom_errorbar(aes(ymin=TransparencyOverTime$Lower, ymax=TransparencyOverTime$Upper), alpha = 1, linetype = 1,width=.2,position=position_dodge(0)) +
  labs(#title = "First Instance Transparency from 2013 to 2017 with Confidence Intervals",
    x = "Year",
    y = "Transparency",
    group = "Case Type") + scale_color_manual(values = c("red", "blue", "green")) + theme_bw() + theme(
      plot.title = element_text(size=20,hjust = 0.5),
      axis.title.x = element_text(size=20),
      axis.title.y = element_text(size=20),
      axis.ticks.length=unit(.2, "cm"),
      axis.text.x = element_text(size=18, angle=0),
      axis.text.y = element_text(size=18, angle=0)) + geom_abline(intercept = 0, slope = 1, color="black", size=0.5) + theme(
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))
p
ggsave("Figure7.png", plot = p, width = 8, height = 6, dpi = 300)


######################################## Figure 8 ########################################
loessfitting = function(dat,s){
  temp = dat
  fit = loess(adjust_startdate ~ clean_case_number, temp,span = s,control=loess.control(surface="direct"))
  #summary(fit)
  clean_case_numberlims<-range(temp$clean_case_number)
  #Generating Test Data
  clean_case_number.grid<-seq(from=clean_case_numberlims[1], to = clean_case_numberlims[2])
  #Plotting the Regression Line to the scatterplot   
  #jpeg(paste("2017admin/plot/final_fit/", as.character(unique(dat$division)),".jpg",sep=""))
  #plot(temp$clean_case_number,temp$adjust_startdate,col="grey",ylab="Work Day Number",xlab="Case Number")
  pred = predict(fit,data.frame(clean_case_number = clean_case_number.grid))
  #points(clean_case_number.grid,pred,col="darkgreen",lwd=2,type="l")
  #dev.off()
  return(fit)
}

loess.predict <- function(fit, newdata) {
  predict(fit, newdata = newdata)
}

loess.model = function(x, y, span){
  loess(y ~ x, span = span, control=loess.control(surface="direct"))
}

loess_wrapper <- function (dataset, starting){
  temp = dataset
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
        return(loess_wrapper(dataset,starting_span))
      }
      non.empty.indices = !is.na(y.hat.cv)
      mean.abs.error[span.index] <- mean(abs(y[non.empty.indices] - y.hat.cv[non.empty.indices]))
    }else{
      return(list("failed","failed","failed"))
    }
  }
  #jpeg(paste("2017admin/plot/choosing_span/", as.character(unique(dat$division)),".jpg",sep=""))
  best.span = span.vals[which.min(mean.abs.error)]
  min_error = min(mean.abs.error)
  #plot(span.vals,mean.abs.error,type="l",xlim = c(0,0.8),col="red",main = str(best.span))
  #dev.off()
  # find the span which minimizes error
  # fit the best model
  best.model <- loessfitting(temp,best.span)
  return(list(best.model,best.span,min_error))
}

input = read.csv(file = 'data/dataforplots/临沂市兰山区人民法院input.csv')
data = read.csv(file = 'data/dataforplots/临沂市兰山区人民法院temp.csv')
data = subset(data,!is.na(adjust_startdate))
library(bootstrap)
library(forecast)
library(ggplot2)
best_fit = loess_wrapper(data,0.05)
fit = loess(adjust_startdate ~ clean_case_number,data,span = best_fit[2][[1]],control=loess.control(surface="direct"))
clean_case_numberlims<-range(data$clean_case_number)
clean_case_number.grid<-seq(from=clean_case_numberlims[1], to = clean_case_numberlims[2])
predline = predict(fit,data.frame(clean_case_number = clean_case_number.grid))
predPoint = predict(fit,data.frame(clean_case_number = input$clean_case_number))


plotData1 = data.frame(clean_case_number = data$clean_case_number,adjust_startdate = data$adjust_startdate,DType = "Available")
plotData2 = data.frame(clean_case_number = clean_case_number.grid,adjust_startdate = predline,DType = "FittedLine")
plotData3 = data.frame(clean_case_number = input$clean_case_number,adjust_startdate = predPoint,DType = "Estimated")
plotData = rbind(plotData1,plotData2,plotData3)

p <- ggplot(data = plotData, aes(x = clean_case_number, y = adjust_startdate,colour = DType)) +
  geom_line(data = subset(plotData, DType == "FittedLine"),aes(shape=DType, color=DType)) +
  geom_point(data = subset(plotData, DType == "Estimated"),aes(shape=DType, color=DType)) +
  geom_point(data = subset(plotData, DType == "Available"),aes(shape=DType, color=DType)) +
  scale_color_manual(values = c("red", "blue", "black"))+ scale_shape_manual(values=c(3,16,27))+
  xlab("Case Serial Number") + ylab("Work Day Number for Registration Date") + theme_bw() + theme(
    plot.title = element_text(size=20,hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.ticks.length=unit(.2, "cm"),
    axis.text.x = element_text(size=18, angle=0),
    axis.text.y = element_text(size=18, angle=0)
  ) + theme( legend.title = element_blank(),
             #legend.title = element_text(size = 20),
             legend.text = element_text(size = 18))
p
ggsave("Figure8.png", plot = p, width = 8, height = 6, dpi = 300)











######################################## Figure 9 25 26 ###########################
workday = read.csv(file = 'workday.csv',header = TRUE,sep = ',')
convert_back = function(dataset,workday,year){
  # convert startdate from 365 scale to 250 scale by removing the weekends and holidays.
  convert_back = workday
  convert_back$preddays = workday$days
  if (year == 2013){
    convert_back$pred_filing_round = convert_back$X2013Workdays
    convert_back = subset(convert_back,select = -c(X2013Workdays,X2014Workdays,X2015Workdays,X2016Workdays,X2017Workdays,days))
    dataset = merge(dataset,convert_back,by = "pred_filing_round", all.x = TRUE)
    return(dataset)
  }else if (year == 2014){
    convert_back$pred_filing_round = convert_back$X2014Workdays
    convert_back = subset(convert_back,select = -c(X2013Workdays,X2014Workdays,X2015Workdays,X2016Workdays,X2017Workdays,days))
    dataset = merge(dataset,convert_back,by = "pred_filing_round", all.x = TRUE)
    return(dataset)
  }else if (year == 2015){
    convert_back$pred_filing_round = convert_back$X2015Workdays
    convert_back = subset(convert_back,select = -c(X2013Workdays,X2014Workdays,X2015Workdays,X2016Workdays,X2017Workdays,days))
    dataset = merge(dataset,convert_back,by = "pred_filing_round", all.x = TRUE)
    return(dataset)
  }else if (year == 2016){
    convert_back$pred_filing_round = convert_back$X2016Workdays
    convert_back = subset(convert_back,select = -c(X2013Workdays,X2014Workdays,X2015Workdays,X2016Workdays,X2017Workdays,days))
    dataset = merge(dataset,convert_back,by = "pred_filing_round", all.x = TRUE)
    return(dataset)
  }else if (year == 2017){
    convert_back$pred_filing_round = convert_back$X2017Workdays
    convert_back = subset(convert_back,select = -c(X2013Workdays,X2014Workdays,X2015Workdays,X2016Workdays,X2017Workdays,days))
    dataset = merge(dataset,convert_back,by = "pred_filing_round", all.x = TRUE)
    return(dataset)
  }
}

ttd2013 = read.csv(file = 'data/dataforplots/adminTTD2013pred.csv')
ttd2013$pred_filing_round = round(ttd2013$pred_filing)
ttd2013 = convert_back(ttd2013,workday,2013)
ttd2014 = read.csv(file = 'data/dataforplots/adminTTD2014pred.csv')
ttd2014$pred_filing_round = round(ttd2014$pred_filing)
ttd2014 = convert_back(ttd2014,workday,2014)
ttd2015 = read.csv(file = 'data/dataforplots/adminTTD2015pred.csv')
ttd2015$pred_filing_round = round(ttd2015$pred_filing)
ttd2015 = convert_back(ttd2015,workday,2015)
ttd2016 = read.csv(file = 'data/dataforplots/adminTTD2016pred.csv')
ttd2016$pred_filing_round = round(ttd2016$pred_filing)
ttd2016 = convert_back(ttd2016,workday,2016)
ttd2017 = read.csv(file = 'data/dataforplots/adminTTD2017pred.csv')
ttd2017$pred_filing_round = round(ttd2017$pred_filing)
ttd2017 = convert_back(ttd2017,workday,2017)
ttd = rbind(ttd2013,ttd2014,ttd2015,ttd2016,ttd2017)
rm(ttd2013,ttd2014,ttd2015,ttd2016,ttd2017)

ttd = subset(ttd,select = c(filename,best_span,min_error,preddays))
GTS = read.csv(file = 'data/dataforplots/adm_GTS.csv')
GTS = merge(GTS,ttd,all.x=TRUE,by = 'filename')
GTS$final_startdays = ifelse(is.na(GTS$startdays),GTS$preddays,GTS$startdays)

ttd = subset(GTS,!is.na(GTS$preddays))
ttd$error = abs(ttd$preddays - ttd$startdays)
division = ttd[!duplicated(ttd$division),]

p = ggplot(ttd, aes(final_startdays)) +
  geom_histogram(binwidth = 1) + labs(x="Registration Date of the Year", y="Number of cases") + scale_fill_brewer(palette = 'Greens') + theme_bw() + theme(
    plot.title = element_text(size=20,hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.text.x = element_text(size=18, angle=0),
    axis.text.y = element_text(size=18, angle=0))
ggsave("Figure25.png", plot = p, width = 8, height = 6, dpi = 300)
## average error for admin
mean(ttd$min_error) # 4.5

# For courts where we have at least 10 known registration dates,
temp = subset(ttd,availStart_in_batch>=10)
mean(temp$min_error) # 2.9


p = ggplot(division) +
  geom_histogram(aes(x = min_error),binwidth = 1,alpha=0.95,color="gray50", fill="#74C476") + labs(x="Mean Absolute Error (MAE)", y="Number of Batches") + theme_bw() + theme(
    plot.title = element_text(size=20,hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.ticks.length=unit(.2, "cm"),
    axis.text.x = element_text(size=18, angle=0),
    axis.text.y = element_text(size=18, angle=0)
  )

p
ggsave("Figure26-1.png", plot = p, width = 8, height = 6, dpi = 300)

p <- ggplot(division)
p = p + geom_point(aes(availStart_in_batch, min_error),alpha = 0.6,color="black", fill="#74C476")+ labs(x="Number of Cases for Training", y="Mean Absolute Error (MAE)") + theme_bw() + theme(
  plot.title = element_text(size=20,hjust = 0.5),
  axis.title.x = element_text(size=20),
  axis.title.y = element_text(size=20),
  axis.ticks.length=unit(.2, "cm"),
  axis.text.x = element_text(size=18, angle=0),
  axis.text.y = element_text(size=18, angle=0)
)
p

ggsave("Figure26-2.png", plot = p, width = 8, height = 6, dpi = 300)
ttd = subset(GTS,is.na(GTS$startdays))
ttd = subset(ttd,!is.na(ttd$preddays))
ttd$duration = ttd$decisiondays - ttd$preddays
temp = subset(ttd,duration<0)
t1 = subset(ttd,year_id< 2015|(year_id == 2015&final_startdays<=120))
t1 = t1$duration
df1=data.frame(year='Before May 1st 2015',time_to_decision = t1)
t1 = subset(ttd,year_id> 2015|(year_id == 2015&final_startdays>120))
t1 = t1$duration
df2=data.frame(year='After May 1st 2015',time_to_decision = t1)
df = rbind(df2,df1)
rm(t1,df1,df2)

df$time_to_decision = ifelse(df$time_to_decision<0,0,df$time_to_decision)
p <- ggplot(df) +
  geom_histogram(aes(x=time_to_decision, fill=year), 
                 colour="grey50", alpha=0.95,binwidth = 10, position="identity")+ labs(x="Days to Decision", y="Number of Cases") + 
  xlim(c(-150,600))+  geom_vline(xintercept = 180,linetype="dashed",size = 0.5,show.legend=TRUE) + geom_vline(xintercept = 90,linetype="dashed",size = 0.3,show.legend=TRUE) + geom_vline(xintercept = 0)+ scale_fill_brewer(palette = 'Greens') + annotate("text", x = c(110,210), y = c(-500,-500), label = c("90", "180") , color="black", size=6) + theme_bw() + theme(
    plot.title = element_text(size=20,hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.text.x = element_text(size=18, angle=0),
    axis.text.y = element_text(size=18, angle=0)) + theme(
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 18))
ggsave("Figure9-1.png", plot = p, width = 8, height = 6, dpi = 300)

ttd_true = subset(GTS,!is.na(GTS$startdays))
ttd_true$duration = ttd_true$decisiondays - ttd_true$startdays
t1 = subset(ttd_true,year_id< 2015|(year_id == 2015&final_startdays<=120))
t1 = t1$duration
df1=data.frame(year='Before May 1st 2015',time_to_decision = t1)
t1 = subset(ttd_true,year_id> 2015|(year_id == 2015&final_startdays>120))
t1 = t1$duration
df2=data.frame(year='After May 1st 2015',time_to_decision = t1)
df = rbind(df2,df1)
rm(t1,df1,df2)
p <- ggplot(df) +
  geom_histogram(aes(x=time_to_decision, fill=year), 
                 colour="grey50", alpha=0.95,binwidth = 10, position="identity")+ labs(x="Days to Decision", y="Number of Cases") + 
  xlim(c(-150,600))+geom_vline(xintercept = 180,linetype="dashed",size = 0.5,show.legend=TRUE) + geom_vline(xintercept = 90,linetype="dashed",size = 0.3,show.legend=TRUE) + geom_vline(xintercept = 0)+ scale_fill_brewer(palette = 'Greens') + annotate("text", x = c(110,210), y = c(-500,-500), label = c("90", "180") , color="black", size=6) + theme_bw() + theme(
    plot.title = element_text(size=20,hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.text.x = element_text(size=18, angle=0),
    axis.text.y = element_text(size=18, angle=0)) + theme(
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 18))
ggsave("Figure9-2.png", plot = p, width = 8, height = 6, dpi = 300)


######################################## Figure 10 and 27 ########################################

ttd2013 = read.csv(file = 'data/dataforplots/criminalTTD2013pred.csv')
ttd2013$pred_filing_round = round(ttd2013$pred_filing)
ttd2013 = convert_back(ttd2013,workday,2013)
ttd2014 = read.csv(file = 'data/dataforplots/criminalTTD2014pred.csv')
ttd2014$pred_filing_round = round(ttd2014$pred_filing)
ttd2014 = convert_back(ttd2014,workday,2014)
ttd2015 = read.csv(file = 'data/dataforplots/criminalTTD2015pred.csv')
ttd2015$pred_filing_round = round(ttd2015$pred_filing)
ttd2015 = convert_back(ttd2015,workday,2015)
ttd2016 = read.csv(file = 'data/dataforplots/criminalTTD2016pred.csv')
ttd2016$pred_filing_round = round(ttd2016$pred_filing)
ttd2016 = convert_back(ttd2016,workday,2016)
ttd2017 = read.csv(file = 'data/dataforplots/criminalTTD2017pred.csv')
ttd2017$pred_filing_round = round(ttd2017$pred_filing)
ttd2017 = convert_back(ttd2017,workday,2017)

GTS = read.csv(file = 'data/dataforplots/criminal_GTS.csv')
GTS1 = subset(GTS,year_id == '2013')
GTS1 = merge(GTS1,ttd2013,all.x=TRUE,by = 'filename')
GTS2 = subset(GTS,year_id == '2014')
GTS2 = merge(GTS2,ttd2014,all.x=TRUE,by = 'filename')
GTS3 = subset(GTS,year_id == '2015')
GTS3 = merge(GTS3,ttd2015,all.x=TRUE,by = 'filename')
GTS4 = subset(GTS,year_id == '2016')
GTS4 = merge(GTS4,ttd2016,all.x=TRUE,by = 'filename')
GTS5 = subset(GTS,year_id == '2017')
GTS5 = merge(GTS5,ttd2017,all.x=TRUE,by = 'filename')

GTS = rbind(GTS1,GTS2,GTS3,GTS4,GTS5)
rm(ttd2013,ttd2014,ttd2015,ttd2016,ttd2017,GTS1,GTS2,GTS3,GTS4,GTS5)
GTS$final_startdays = ifelse(is.na(GTS$startdays),GTS$preddays,GTS$startdays)

ttd = subset(GTS,!is.na(GTS$preddays))
ttd$error = abs(ttd$preddays - ttd$startdays)
division = ttd[!duplicated(ttd$division),]

## average error for criminal
mean(ttd$min_error) # 1.368682

# For courts where we have at least 10 known registration dates,
temp = subset(ttd,availStart_in_batch>=15)
mean(temp$min_error) # 1.278175

p = ggplot(division) +
  geom_histogram(aes(x = min_error),binwidth = 1,alpha=0.95,color="grey50", fill="#74C476") + labs(x="Cross-Validation Mean Absolute Error", y="Number of Batches") + theme_bw() + theme(
    plot.title = element_text(size=20,hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.ticks.length=unit(.2, "cm"),
    axis.text.x = element_text(size=18, angle=0),
    axis.text.y = element_text(size=18, angle=0)
  )
p
ggsave("Figure27-1.png", plot = p, width = 8, height = 6, dpi = 300)

p <- ggplot(division)
p = p + geom_point(aes(availStart_in_batch, min_error),alpha = 1,color="black", fill="black") + 
  labs(x="Number of Cases for Training", y="Cross-Validation Mean Absolute Error") +
  xlim(0,200) + theme_bw() + theme(
    plot.title = element_text(size=20,hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.ticks.length=unit(.2, "cm"),
    axis.text.x = element_text(size=18, angle=0),
    axis.text.y = element_text(size=18, angle=0)
  )
p
ggsave("Figure27-2.png", plot = p, width = 8, height = 6, dpi = 300)


ttd = subset(GTS,is.na(GTS$startdays))
ttd = subset(ttd,!is.na(ttd$preddays))
ttd$duration = ttd$decisiondays - ttd$preddays

t1 = subset(ttd,year_id< 2015|(year_id == 2015&final_startdays<=120))
t1 = t1$duration
df1=data.frame(year='Before May 1st 2015',time_to_decision = t1)
t1 = subset(ttd,year_id> 2015|(year_id == 2015&final_startdays>120))
t1 = t1$duration
df2=data.frame(year='After May 1st 2015',time_to_decision = t1)
df = rbind(df2,df1)
rm(t1,df1,df2)

df$time_to_decision = ifelse(df$time_to_decision<0,0,df$time_to_decision)
p = ggplot(df) +
  geom_histogram(aes(x=time_to_decision), 
                 colour="grey50", alpha=0.95,binwidth = 10, position="identity",fill="#A1D99B")+ labs(x="Days to Decision", y="Number of Cases") + 
  xlim(c(-10,300)) + geom_vline(xintercept = 90,linetype="dashed",size = 0.3) + geom_vline(xintercept = 0) + annotate("text", x = c(100,210), y = c(-2000,-1000), label = c("90", "") , color="black", size=5) + theme_bw() + theme(
    plot.title = element_text(size=20,hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.text.x = element_text(size=18, angle=0),
    axis.text.y = element_text(size=18, angle=0)) + theme(
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 18))

p
ggsave("Figure10-1.png", plot = p, width = 8, height = 6, dpi = 300)
ttd_true = subset(GTS,!is.na(GTS$startdays))
ttd_true$duration = ttd_true$decisiondays - ttd_true$startdays
p = ggplot(ttd_true) +
  geom_histogram(aes(x=duration), 
                 colour="grey50", alpha=0.95,binwidth = 10, position="identity",fill="#A1D99B")+ labs(x="Days to Decision", y="Number of Cases") + 
  xlim(c(-10,300)) + geom_vline(xintercept = 90,linetype="dashed",size = 0.3) + geom_vline(xintercept = 0) + annotate("text", x = c(100,210), y = c(-10000,-1000), label = c("90", "") , color="black", size=5) + theme_bw() + theme(
    plot.title = element_text(size=20,hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.text.x = element_text(size=18, angle=0),
    axis.text.y = element_text(size=18, angle=0)) + theme(
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 18))
p
ggsave("Figure10-2.png", plot = p, width = 8, height = 6, dpi = 300)



######################################## Figure 15 ########################################
admin = read.csv(file = 'data/dataforplots/admin_validation_transparency_coarsen.csv')
criminal = read.csv(file = 'data/dataforplots/criminal_validation_transparency_coarsen.csv')
civil = read.csv(file = 'data/dataforplots/civil_validation_transparency_coarsen.csv')

criminal_validation = read.csv(file = 'data/dataforplots/criminal_validation_plt_new.csv')
criminal_validation$division  = paste(criminal_validation$Year,criminal_validation$法院名称,'刑初')
criminal_validation = merge(criminal_validation,criminal,by = 'division', all.x = TRUE)
criminal_validation$gtp_percent_error = (abs(criminal_validation$mvue - criminal_validation$受理刑事诉讼)/criminal_validation$受理刑事诉讼)
criminal_validation$coarsed_percent_error = (abs(criminal_validation$mvue_coarsen - criminal_validation$受理刑事诉讼)/criminal_validation$受理刑事诉讼)

#plot(criminal_validation$ks_p_value,criminal_validation$gtp_percent_error)

#plot(criminal_validation$ks_p_value_coarsen,criminal_validation$coarsed_percent_error)

criminal_v_plot = criminal_validation
criminal_v_plot$esti = criminal_v_plot$mvue
criminal_v_plot$Method = 'MVUE'
criminal_v_plot$ci_low = criminal_v_plot$max_case_number
criminal_v_plot$ci_upper = criminal_v_plot$mvue_upper

p = ggplot(criminal_v_plot, aes(x = 受理刑事诉讼, y = esti, group = Method)) +
  geom_point(size = 2,aes(shape=Method,color = Method),fill="white") +
  geom_errorbar(aes(ymax = ci_upper, ymin = ci_low)) +  ylim(0, 3500) + xlim(0, 3500) +ggtitle("Criminal Courts Validation") +
  xlab("Number of Cases from Work Reports") + ylab("Estimation") + scale_color_manual(values = c("red", "blue", "green")) + theme_bw() + theme(
    plot.title = element_text(size=20,hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.ticks.length=unit(.2, "cm"),
    axis.text.x = element_text(size=18, angle=0),
    axis.text.y = element_text(size=18, angle=0)
  ) + geom_abline(intercept = 0, slope = 1, color="black", size=0.5) + theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18))
p
ggsave("Figure15-1.png", plot = p, width = 8, height = 6, dpi = 300)
criminal_v_plot = criminal_validation
criminal_v_plot$esti = criminal_v_plot$mvue_coarsen
criminal_v_plot$Method = 'CoarsenGTS'
criminal_v_plot$ci_low = criminal_v_plot$max_case_number_coarsen*10
criminal_v_plot$ci_upper = criminal_v_plot$mvue_upper_coarsen

p = ggplot(criminal_v_plot, aes(x = 受理刑事诉讼, y = esti, group = Method)) +
  geom_point(size = 2,aes(shape=Method,color = Method),fill="white") +
  geom_errorbar(aes(ymax = ci_upper, ymin = ci_low)) +  ylim(0, 3500) + xlim(0, 3500) +ggtitle("Criminal Courts Validation") +
  xlab("Number of Cases from Work Reports") + ylab("Estimation") + scale_color_manual(values = c("red", "blue", "green")) + theme_bw() + theme(
    plot.title = element_text(size=20,hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.ticks.length=unit(.2, "cm"),
    axis.text.x = element_text(size=18, angle=0),
    axis.text.y = element_text(size=18, angle=0)
  ) + geom_abline(intercept = 0, slope = 1, color="black", size=0.5) + theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18))
p
ggsave("Figure15-2.png", plot = p, width = 8, height = 6, dpi = 300)


######################################## Figure 16 ########################################
criminal_v_plot = criminal_validation
criminal_v_plot$error_percent = criminal_v_plot$gtp_percent_error*100
criminal_v_plot$ks_p_value_t = criminal_v_plot$ks_p_value
criminal_v_plot$Method = 'MVUE'

criminal_v_plot1 = criminal_validation
criminal_v_plot1$error_percent = criminal_v_plot1$coarsed_percent_error*100
criminal_v_plot1$Method = 'CoarsenGTS'
criminal_v_plot1$ks_p_value_t = criminal_v_plot1$ks_p_value_coarsen

criminal_v_plot = rbind(criminal_v_plot,criminal_v_plot1)
#criminal_v_plot = subset(criminal_v_plot,受理行政诉讼<1000)
p = ggplot(criminal_v_plot, aes(x = ks_p_value_t, y = error_percent, group = Method)) +
  geom_point(size = 2,aes(shape=Method,color = Method),fill="white") + ggtitle("Uniformity Test of Criminal Courts") +
  xlab("P-value of Uniformity Test") + ylab("Percent Error") + scale_color_manual(values = c("red", "blue", "green")) + theme_bw() + theme(
    plot.title = element_text(size=20,hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.ticks.length=unit(.2, "cm"),
    axis.text.x = element_text(size=18, angle=0),
    axis.text.y = element_text(size=18, angle=0)
  ) + geom_vline(xintercept = 0.05) + theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18))
p + annotate("text", x = 0.3, y = 200, label = "0.05 p-value threshold", size = 6)
ggsave("Figure16.png", plot = p, width = 8, height = 6, dpi = 300)

######################################## Figure 17 ########################################

civil_validation = read.csv(file = 'data/dataforplots/civil_validation_plt_new.csv')
civil_validation$division  = paste(civil_validation$Year,civil_validation$法院名称,'民初')
civil_validation = merge(civil_validation,civil,by = 'division', all.x = TRUE)
civil_validation$gtp_percent_error = (abs(civil_validation$mvue - civil_validation$民事案件受理总数)/civil_validation$民事案件受理总数)



civil_validation$coarsed_percent_error = (abs(civil_validation$mvue_coarsen - civil_validation$民事案件受理总数)/civil_validation$民事案件受理总数)

#plot(civil_validation$ks_p_value,civil_validation$gtp_percent_error)

#plot(civil_validation$ks_p_value_coarsen,civil_validation$coarsed_percent_error)

civil_v_plot = civil_validation
civil_v_plot$esti = civil_v_plot$mvue
civil_v_plot$Method = 'MVUE'
civil_v_plot$ci_low = civil_v_plot$max_case_number
civil_v_plot$ci_upper = civil_v_plot$mvue_upper

p = ggplot(civil_v_plot, aes(x = 民事案件受理总数, y = esti, group = Method)) +
  geom_point(size = 2,aes(shape=Method,color = Method),fill="white") +
  geom_errorbar(aes(ymax = ci_upper, ymin = ci_low))  + xlim(0,35000) + ylim(0, 35000) + ggtitle("Civil Courts Validation") +
  xlab("Number of Cases from Work Reports") + ylab("Estimation") + scale_color_manual(values = c("red", "blue", "green")) + theme_bw() + theme(
    plot.title = element_text(size=20,hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.ticks.length=unit(.2, "cm"),
    axis.text.x = element_text(size=18, angle=0),
    axis.text.y = element_text(size=18, angle=0)
  ) + geom_abline(intercept = 0, slope = 1, color="black", size=0.5) + theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18))
p
ggsave("Figure17-1.png", plot = p, width = 8, height = 6, dpi = 300)
civil_v_plot = civil_validation
civil_v_plot$esti = civil_v_plot$mvue_coarsen
civil_v_plot$Method = 'CoarsenGTS'
civil_v_plot$ci_low = civil_v_plot$max_case_number_coarsen*100
civil_v_plot$ci_upper = civil_v_plot$mvue_upper_coarsen

p = ggplot(civil_v_plot, aes(x = 民事案件受理总数, y = esti, group = Method)) +
  geom_point(size = 2,aes(shape=Method,color = Method),fill="white") +
  geom_errorbar(aes(ymax = ci_upper, ymin = ci_low))  + xlim(0,35000) + ylim(0, 35000) + ggtitle("Civil Courts Validation") +
  xlab("Number of Cases from Work Reports") + ylab("Estimation") + scale_color_manual(values = c("red", "blue", "green")) + theme_bw() + theme(
    plot.title = element_text(size=20,hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.ticks.length=unit(.2, "cm"),
    axis.text.x = element_text(size=18, angle=0),
    axis.text.y = element_text(size=18, angle=0)
  ) + geom_abline(intercept = 0, slope = 1, color="black", size=0.5) + theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18))
p
ggsave("Figure17-2.png", plot = p, width = 8, height = 6, dpi = 300)


######################################## Figure 18 ########################################



civil_v_plot = civil_validation
civil_v_plot$error_percent = civil_v_plot$gtp_percent_error*100
civil_v_plot$ks_p_value_t = civil_v_plot$ks_p_value
civil_v_plot$Method = 'MVUE'

civil_v_plot1 = civil_validation
civil_v_plot1$error_percent = civil_v_plot1$coarsed_percent_error*100
civil_v_plot1$Method = 'CoarsenGTS'
civil_v_plot1$ks_p_value_t = civil_v_plot1$ks_p_value_coarsen

civil_v_plot = rbind(civil_v_plot,civil_v_plot1)

p = ggplot(civil_v_plot, aes(x = ks_p_value_t, y = error_percent, group = Method)) +
  geom_point(size = 2,aes(shape=Method,color = Method),fill="white") + ggtitle("Uniformity Test of Civil Courts") +
  xlab("P-value of Uniformity Test") + ylab("Percent Error") + scale_color_manual(values = c("red", "blue", "green")) + theme_bw() + theme(
    plot.title = element_text(size=20,hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.ticks.length=unit(.2, "cm"),
    axis.text.x = element_text(size=18, angle=0),
    axis.text.y = element_text(size=18, angle=0)
  ) + geom_vline(xintercept = 0.05) + theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18))
p + annotate("text", x = 0.3, y = 200, label = "0.05 p-value threshold",size = 6)

ggsave("Figure18.png", plot = p, width = 8, height = 6, dpi = 300)


######################################## Figure 19 20 ################
# Simulation I
# Suppose uniformity assumption is met.  MVUE will outperform Coarsened German Tank solution. Question is by how much.
set.seed(1)
library(lmtest)

reps <- 10000
sample.size <- 1000
population <- seq(1,2000)
prob <- NULL
batch.size <- seq(1,20)
mean.est <- c()
sd.est <- c()

for (size in batch.size){
  print(size)
  sim.sample <- replicate(n = reps, 
                          expr = sample(population, sample.size, replace = FALSE, prob = prob), 
                          simplify = TRUE)
  
  sim.sample.coarse = sim.sample/size
  
  est <- apply(sim.sample.coarse, 2, function(x) (round(max(x)+0.5) + round(max(x)+0.5)/length(unique(round(x+0.5))) - 1)*size)
  mean.est = c(mean.est,mean(est))
  sd.est = c(sd.est,sd(est))
}
png("Figure19.png", width = 800, height = 600, res = 120)
plot(batch.size,mean.est,xlab="Bin Size", ylab="Mean of Estimation of 10000 simulations", main = "Simulation I for Coarsened German Tank Solution")
dev.off()

# Simulation II
bins <- seq(1,100)
max_binprop = max(round(dbeta((bins/100*99+0.9)/100,0.999,0.999),4))
min_binprop = min(round(dbeta((bins/100*99+0.9)/100,0.999,0.999),4))
transparency <- 0.7
batch.size <- seq(1,20)
mean.est <- c()
sd.est <- c()

for (size in batch.size){
  print(size)
  reps = seq(1,10000)
  cgtp.est = c()
  for (rep in reps){
    selected.bins <- sample(bins,length(bins)*transparency, replace = FALSE, prob = NULL)
    total.sample <- c()
    for (bin in selected.bins){
      temp.population = seq(20*(bin-1)+1,20*bin)
      sample.size = round((round(dbeta((bin/100*99+0.9)/100,0.999,0.999),4)-min_binprop)/(max_binprop-min_binprop)*20)
      if (sample.size == 0){sample.size = 1}
      temp.sample <- sample(temp.population, sample.size, replace = FALSE, prob = NULL)
      total.sample <- c(total.sample,temp.sample)
    }
    total.sample.coarse = total.sample/size
    cgtp.est.temp <- (round(max(total.sample.coarse)+0.49999999) + round(max(total.sample.coarse)+0.49999999)/length(unique(round(total.sample.coarse+0.49999999))) - 1)*size
    cgtp.est <- c(cgtp.est,cgtp.est.temp)
  }
  mean.est = c(mean.est,mean(cgtp.est))
  sd.est = c(sd.est,sd(cgtp.est))
}
png("Figure20.png", width = 800, height = 600, res = 120)
plot(batch.size,mean.est,xlab="Bin Size",
     ylab="Mean of Estimation of 10000 simulations", main = "Simulation II for Coarsened German Tank Solution")
dev.off()


######################################## Figure 30 31 ######################################## 
###### transparency over time appendix ####
library(readr)
library(ggplot2)
TransparencyOverTime <- read_csv("data/dataforplots/court_transparency_over_18-22_allcases.csv")

p<-ggplot(data=TransparencyOverTime, aes(x=Year, y=Transparency, group = Type)) + 
  geom_point(size = 4,aes(shape=Type,color = Type)) + geom_line() +
  geom_errorbar(aes(ymin=TransparencyOverTime$Lower, ymax=TransparencyOverTime$Upper), alpha = 1, linetype = 1,width=.2,position=position_dodge(0)) +
  labs(#title = "First Instance Transparency from 2013 to 2017 with Confidence Intervals",
    x = "Year",
    y = "Transparency",
    group = "Case Type") + scale_color_manual(values = c("red", "blue", "green")) + theme_bw() + theme(
      plot.title = element_text(size=20,hjust = 0.5),
      axis.title.x = element_text(size=20),
      axis.title.y = element_text(size=20),
      axis.ticks.length=unit(.2, "cm"),
      axis.text.x = element_text(size=18, angle=0),
      axis.text.y = element_text(size=18, angle=0)) + geom_abline(intercept = 0, slope = 1, color="black", size=0.5) + theme(
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))
p
ggsave("Figure30.png", plot = p, width = 8, height = 6, dpi = 300)

TransparencyOverTime <- read_csv("data/dataforplots/court_transparency_over_18-22_fulltext.csv")


p<-ggplot(data=TransparencyOverTime, aes(x=Year, y=Transparency, group = Type)) + 
  geom_point(size = 4,aes(shape=Type,color = Type)) + geom_line() +
  geom_errorbar(aes(ymin=TransparencyOverTime$Lower, ymax=TransparencyOverTime$Upper), alpha = 1, linetype = 1,width=.2,position=position_dodge(0)) +
  labs(#title = "First Instance Transparency from 2013 to 2017 with Confidence Intervals",
    x = "Year",
    y = "Transparency",
    group = "Case Type") + scale_color_manual(values = c("red", "blue", "green")) + theme_bw() + theme(
      plot.title = element_text(size=20,hjust = 0.5),
      axis.title.x = element_text(size=20),
      axis.title.y = element_text(size=20),
      axis.ticks.length=unit(.2, "cm"),
      axis.text.x = element_text(size=18, angle=0),
      axis.text.y = element_text(size=18, angle=0)) + geom_abline(intercept = 0, slope = 1, color="black", size=0.5) + theme(
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))
p
ggsave("Figure31.png", plot = p, width = 8, height = 6, dpi = 300)

## trans over time plot for Indian courts
Indian_overtime = read_csv('Indian_transparency_over_time.csv')

geom_point(size = 4) + geom_line() +
  geom_errorbar(aes(ymin=trans_overtime$Lower, ymax=trans_overtime$Upper), alpha = 1, linetype = 1,width=.2,position=position_dodge(0)) +
  labs(#title = "First Instance Transparency from 2013 to 2017 with Confidence Intervals",
    x = "Year",
    y = "Transparency"
  )+ theme_bw() + theme(
    plot.title = element_text(size=20,hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.ticks.length=unit(.2, "cm"),
    axis.text.x = element_text(size=18, angle=0),
    axis.text.y = element_text(size=18, angle=0)) + geom_abline(intercept = 0, slope = 1, color="black", size=0.5) + theme(
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 18))
p
ggsave("Figure29.png", plot = p, width = 8, height = 6, dpi = 300)