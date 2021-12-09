library(forecast)
library(dplyr)

###
data2=read.csv("C://Users//pmk8828//Documents//practicum//_SQL_query_Combined_Jan2019_Jan2021.csv")
enc.sub=substr(data2$encounter_code,1,3)
N=length(unique(data2$ï..patient_id))#638027

#obesity
length(which(enc.sub=="E66"|enc.sub=="278"|#obesity dx codes, icd 10/9
               data2$encounter_code=="Z6854"|data2$encounter_code=="Z68.54"|#obese BMI
               data2$encounter_code=="Z6853"|data2$encounter_code=="Z68.53"))#overweight BMI
obese=data2[which(enc.sub=="E66"|enc.sub=="278"|
                    data2$encounter_code=="Z6854"|data2$encounter_code=="Z68.54"|
                    data2$encounter_code=="Z6853"|data2$encounter_code=="Z68.53"),]
obese.sort=obese[order(obese$admit_date),]
obese.sort.uniq=obese.sort[!duplicated(obese.sort$ï..patient_id),]
dim(obese.sort.uniq)[1]/N*100#10.3% of patients obese
mnth=as.numeric(format(as.Date(obese.sort.uniq$admit_date,format="%m/%d/%Y"), format = "%m"))
yr=as.numeric(format(as.Date(obese.sort.uniq$admit_date,format="%m/%d/%Y"), format = "%Y"))
wk=as.numeric(strftime(as.Date(obese.sort.uniq$admit_date,format="%m/%d/%Y"), format = "%V"))
wkyr=c(as.numeric(table(yr,factor(wk,levels=1:53))[1,]),as.numeric(table(yr,factor(wk,levels=1:53))[2,]))

obese2=data.frame(obese)
write.csv(obese2,"C://Users//pmk8828//Documents//practicum//ObeseFips.csv")

