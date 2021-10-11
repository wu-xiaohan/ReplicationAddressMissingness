library(ggplot2)

admin = read.csv(file = 'admin_transparency.csv')


admin$EstimatesRatio = admin$mvue/admin$linearEstimation
ggplot(admin, aes(x=EstimatesRatio)) + geom_histogram(bins=300) + xlim(0, 1.5) + geom_vline(xintercept = median(admin$EstimatesRatio[!is.na(admin$EstimatesRatio)]), color = 'red')


# admin validation
admin_validation = read.csv(file = 'adm_validation_plt_new.csv')
admin_validation$division  = paste(admin_validation$Year,admin_validation$法院名称,'行初')
admin_validation = merge(admin_validation,admin,by = 'division', all.x = TRUE)
admin_validation$gtp_percent_error = abs(admin_validation$mvue - admin_validation$受理行政诉讼)/admin_validation$受理行政诉讼
admin_validation$regre_percent_error = abs(admin_validation$linearEstimation - admin_validation$受理行政诉讼)/admin_validation$受理行政诉讼

plot(admin_validation$ks_p_value,admin_validation$gtp_percent_error)
plot(admin_validation$ci,admin_validation$gtp_percent_error)
plot(admin_validation$white_test,admin_validation$regre_percent_error)
plot(admin_validation$r_squared,admin_validation$regre_percent_error)

admin_v_plot = admin_validation
admin_v_plot$esti = admin_v_plot$mvue
admin_v_plot$Method = 'MVUE'
admin_v_plot$ci_low = admin_v_plot$max_case_number
admin_v_plot$ci_upper = admin_v_plot$max_case_number + admin_v_plot$ci_upper
admin_v_plot1 = admin_validation
admin_v_plot1$esti = admin_v_plot1$linearEstimation
admin_v_plot1$Method = 'Linear'
admin_v_plot1$ci_low = admin_v_plot1$lower_bound
admin_v_plot1$ci_upper = admin_v_plot1$upper_bound
admin_v_plot = rbind(admin_v_plot,admin_v_plot1)

## Observation >1000 removed 
ggplot(admin_v_plot, aes(x = 受理行政诉讼, y = esti, group = Method)) +
  geom_point(size = 2,aes(shape=Method,color = Method),fill="white") +
  geom_errorbar(aes(ymax = ci_upper, ymin = ci_low)) + ylim(0, 500) + xlim(0, 500) + ggtitle("Administrative Courts Validation") +
  xlab("Number of Cases from Work Reports") + ylab("Estimation") + scale_color_manual(values = c("red", "blue", "green")) + theme_bw() + theme(
    plot.title = element_text(size=20,hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20)
  ) + geom_abline(intercept = 0, slope = 1, color="black", size=0.5) + theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18))

admin_v_plot = admin_validation
admin_v_plot$error_percent = admin_v_plot$gtp_percent_error
admin_v_plot$Method = 'MVUE'
admin_v_plot1 = admin_validation
admin_v_plot1$Method = 'Linear'
admin_v_plot1$error_percent = admin_v_plot1$regre_percent_error
admin_v_plot = rbind(admin_v_plot,admin_v_plot1)
#admin_v_plot = subset(admin_v_plot,受理行政诉讼<1000)
p = ggplot(admin_v_plot, aes(x = ks_p_value, y = error_percent, group = Method)) +
  geom_point(size = 2,aes(shape=Method,color = Method),fill="white") + ggtitle("Uniformity Test of Administrative Courts") +
  xlab("P-value of Uniformity Test") + ylab("Percent Error") + scale_color_manual(values = c("red", "blue", "green")) + theme_bw() + theme(
    plot.title = element_text(size=20,hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20)
  ) + geom_vline(xintercept = 0.05) + theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18))
p + annotate("text", x = 0.2, y = 2, label = "0.05 p-value threshold")

