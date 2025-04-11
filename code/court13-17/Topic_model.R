# Do not run. This script is used to demostrate the steps taken in our topic modeling exercise. 
# The data folder doesn't contain the full text of court decisions.
# Cases id for the data set are saved in "data/topic_model/case_in_TM.csv"
# The topic model outputs are saved in the "data/topic_model" folder.

# Scripts for topic modeling

library(dplyr)
library(stm)
library(stringr)
require('urltools')
require('jsonlite')

raw_data = read.csv("file that contains segmentated full text")

###
rg1 <- "[\\S]*[市|县|区|省|村|镇|乡|庄|屯|街|郡|院|局|室|社|处|厅|某|x|X|×|甲|乙|丙|丁|戊|一|二|三|四|五|六|七|八|九|十|百|千|万|年]"
rg2 <- "[\\S]*[:digit:][\\S]*"
rg3 <- "[。|,|，|(|)|（|）|“|”|[|]|；|？|！|、|：|《|》|【|】]"
rg4 <- "[:punct:]"
rg5 <- "[[A-Za-z]]"
Encoding(rg1) <- "UTF-8"; Encoding(rg2) <- "UTF-8"; Encoding(rg3) <- "UTF-8"; Encoding(rg4) <- "UTF-8"
raw_data$segmentedtext <- as.factor(str_replace_all(as.character(raw_data$segmentedtext), rg1, ""))
raw_data$segmentedtext <- as.factor(str_replace_all(as.character(raw_data$segmentedtext), rg2, ""))
raw_data$segmentedtext <- as.factor(str_replace_all(as.character(raw_data$segmentedtext), rg3, ""))
raw_data$segmentedtext <- as.factor(str_replace_all(as.character(raw_data$segmentedtext), rg4, ""))
raw_data$segmentedtext <- str_replace_all(raw_data$segmentedtext, rg5, "")
raw_data$segmentedtext = as.character(raw_data$segmentedtext)

stopword = c('上海','云南','内蒙古','北京','吉林','四川','天津','宁夏','安徽','山东','山西','广东','广西','新疆','新疆兵团','最高人民法院','江苏','江西','河北','河南','浙江','海南','湖北','湖南','甘肃','福建','西藏','贵州','辽宁','重庆','陕西',
             '青海','黑龙江','不属于','下列','部分','文件','通知','有关','知道','审判员','书记员','审判长','简称','朝阳','仿宋','翁牛特旗','身份证','号码','复印件','上诉人','原告','被告','申请人','根据','该案','予以','诉讼','为由','或者','上述','本案','每日','代理人','被申请人',
             '该中心','京房','法规','日内','代理','案件','代理人')

processed <- textProcessor(documents=raw_data$segmentedtext,
                           metadata=raw_data,
                           wordLengths = c(2, Inf), 
                           lowercase=TRUE,
                           removenumbers=TRUE, 
                           removepunctuation=TRUE,
                           removestopwords=FALSE,
                           customstopwords = stopword,
                           stem=FALSE)

out <- prepDocuments(processed$documents, 
                     processed$vocab, 
                     meta = processed$meta,
                     lower.thresh = 50,
                     upper.thresh = 41380*0.5)

ttd_stm <- stm(out$documents, out$vocab, K = 0, prevalence = ~ exceeds, data=out$meta, seed=1993)

labelTopics(ttd_stm, n=10)
prep <- estimateEffect(1:68 ~ difficult, ttd_stm, meta = out$meta, uncertainty = "Global")

plot(prep, covariate = "exceeds",model = ttd_stm, method = "exceeds",main="",
     labeltype ="custom",cov.value1 = 0, cov.value2 = 1,custom.labels=c(1:68))

probwords <- labelTopics(ttd_stm, n=10)[[1]]
frexwords <- labelTopics(ttd_stm, n=10)[[2]]

write.csv(ttd_stm$theta, 'ttd_stm_theta.csv', row.names=FALSE)
write.csv(frexwords, 'ttd_stm-frexwords.csv', row.names=FALSE)
write.csv(probwords, 'ttd_stm-probwords.csv', row.names=FALSE)

