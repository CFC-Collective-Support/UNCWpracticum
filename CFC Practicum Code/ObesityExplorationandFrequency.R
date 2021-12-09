library(forecast)
library(dplyr)

###
data2=read.csv(path)
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

#childhood obesity
length(which(((yr==2019&obese.sort.uniq$age<=20&obese.sort.uniq$age>=4)|
                (yr==2020&obese.sort.uniq$age<=19&obese.sort.uniq$age>=3))))

#6995 obese children
length(which(((yr==2019&obese.sort.uniq$age<=20&obese.sort.uniq$age>=4)|
                (yr==2020&obese.sort.uniq$age<=19&obese.sort.uniq$age>=3))))/dim(obese.sort.uniq)[1]*100

length(which(((yr==2019&obese.sort.uniq$age<=20&obese.sort.uniq$age>=4)|
                (yr==2020&obese.sort.uniq$age<=19&obese.sort.uniq$age>=3))))/N*100

obesech=obese.sort.uniq[which(((yr==2019&obese.sort.uniq$age<=20&obese.sort.uniq$age>=4)|
                                 (yr==2020&obese.sort.uniq$age<=19&obese.sort.uniq$age>=3))),]
mnth.c=as.numeric(format(as.Date(obesech$admit_date,format="%m/%d/%Y"), format = "%m"))
yr.c=as.numeric(format(as.Date(obesech$admit_date,format="%m/%d/%Y"), format = "%Y"))
wk.c=as.numeric(strftime(as.Date(obesech$admit_date,format="%m/%d/%Y"), format = "%V"))
wkyr.c=c(as.numeric(table(yr.c,factor(wk.c,levels=1:53))[1,]),as.numeric(table(yr.c,factor(wk.c,levels=1:53))[2,]))

abline(v=53+19)

#look at other codes 
childid=obesech$ï..patient_id
length(data2$encounter_code[which(data2$ï..patient_id%in%childid)])
records=data.frame(code=names(sort(table(data2$encounter_code[which(data2$ï..patient_id%in%childid)]),decreasing=T)),
      num_tot_records=as.numeric(sort(table(data2$encounter_code[which(data2$ï..patient_id%in%childid)]),decreasing=T)),
      perc_tot_records=as.numeric(sort(table(data2$encounter_code[which(data2$ï..patient_id%in%childid)]),decreasing=T))/length(data2$encounter_code[which(data2$ï..patient_id%in%childid)])*100)
for (i in 1:dim(records)[1]){
  if (records$code[i]=="NULL"){
    records$desc[i]="NULL"
  } else{
    records$desc[i]=data2$encounter_description[which(data2$encounter_code==records$code[i])[1]]
  }
}
write.csv(records, path)


#look at codes for all 
yr.d=as.numeric(format(as.Date(data2$admit_date,format="%m/%d/%Y"), format = "%Y"))
child=data2[which((yr.d==2019&data2$age<=20&data2$age>=4)|
                    (yr.d==2020&data2$age<=19&data2$age>=3)),]
child.r=child[-which(child$ï..patient_id%in%childid),]
dim(child)
dim(child.r)
dim(obesech)
length(which(child$ï..patient_id%in%childid))
sum(records$num_tot_records)
sum(records$num_tot_records)-length(which(child$ï..patient_id%in%childid))#3774
length(which(is.na(data2$encounter_code[which(data2$ï..patient_id%in%childid)])))
length(which((data2$ï..patient_id%in%childid)&((yr.d==2020&data2$age>19)|(yr.d==2019&data2$age>20))))
records2=data.frame(code=names(sort(table(child.r$encounter_code),decreasing=T)),
                   num_tot_records=as.numeric(sort(table(child.r$encounter_code),decreasing=T)),
                   perc_tot_records=as.numeric(sort(table(child.r$encounter_code),decreasing=T))/dim(child.r)[1]*100)
for (i in 1:dim(records2)[1]){
  if (records2$code[i]=="NULL"){
    records2$desc[i]="NULL"
  } else{
    records2$desc[i]=data2$encounter_description[which(data2$encounter_code==records2$code[i])[1]]
  }
}
write.csv(records2,path)
