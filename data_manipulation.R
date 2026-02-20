 
  ## keep, drop , rename, lowcase

library("pharmaversesdtm")
library("dplyr")

dm <- pharmaversesdtm::dm

#to get colnames: both names & colnames functions are working same

names(dm)

colnames(dm)

# to convert varnames to lowcase


colnames(dm)<-tolower(colnames(dm))

# to keep , drop , rename

keep=dm%>%select (subjid,age,sex,race)

drop=dm%>%select(-c(subjid,age,sex,race))


##to get  colnames

##sorting , distint 

vs<-pharmaversesdtm::vs

sort=vs%>%arrange(USUBJID,VSTESTCD,VSTEST,desc(VISITNUM))

dist=vs%>%distinct(USUBJID,VSTESTCD,keep_all=TRUE)


##first. and last.

dt=vs%>%group_by(USUBJID,VSTESTCD,VISITNUM,VSTPTNUM)%>%
  
  #char functions
  
  rquote <- "r's internals are irrefutably intriguing"

chars <- strsplit(rquote, split = "")[[1]]

#to count when 'r' was there in input
#to break when 'u' was there in input

#loop with if function
rcount<-0
for(char in chars){
  if (char=='r') {
    rcount=rcount+1
  }
  if (char=='u'){
    break
  }
}

print(rcount)

#to know anything
#it provide documentaion on R help tab
help(sd)
?sd

#to get avg
linkedin <- c(16, 9, 13, 5, 2, 17, 14)
facebook <- c(17, 7, 5, 16, 8, 13, 14)

avg<-mean(linkedin+facebook)
avgt<-mean(linkedin+facebook,trim=0.2)

#NA default working
linkedin <- c(16, 9, 13, 5, NA, 17, 14)
facebook <- c(17, NA, 5, 16, 8, 13, 14)

mean(linkedin)

mean(linkedin,na.rm=TRUE)

#user defined created function

pow_two<-function(x,print_info=TRUE){
  y<-x^2
  if(print_info){
  print(paste(y, "is the sqaure route of", x))
  }
  return(y)
}

pow_two(2)
pow_two(2,FALSE)

## a is not overwritten

triple<-function(x){
  x<-3*x
  x}

a<-5

triple(a)

## function with different ways

increment<-function(x,inc=1){
  x<-x+inc
  x
}

increment(5)

count<-5

a<-increment(count,2)
b<-increment(count)
count<-increment(count,2)

#dates
today<-Sys.Date()
class(today)

time<-Sys.time()
class(time)

my_date=as.Date('2025-11-10')

class(my_date)

#read & write files
#excel file 

library(readxl)

data <- read_excel("C:/Users/a076924/OneDrive - Syneos Health/Desktop/int/R/check.xlsx")


file.exists("C:/Users/a076924/OneDrive - Syneos Health/Desktop/int/R/check.xlsx")


##sas & spss stata

library("haven")

#stata: dta
a2<-read_dta('C:/Users/a076924/OneDrive - Syneos Health/Desktop/int/R/data2.dta')

#spss

a3<-read_sav('C:/Users/a076924/OneDrive - Syneos Health/Desktop/int/R/data2.sav')

#csv

a4<-read.csv('C:/Users/a076924/OneDrive - Syneos Health/Desktop/int/R/data2.csv')

#keep

keep<-a2%>%select(main_id,disease)

#drop

drop<-a3%>%select(- ht,-wt,-motor)

#filter == , !=

filter<-a4%>%filter(bicycle!=1)

filter<-a4%>%filter(ht>=150)

#create char version of numerci by factor function

level<-a3%>%mutate(smoking=factor(smoking,
                                levels=c(0,1),
                                       labels=c('Non-smoker','smoker'),exclude=NA))

str(level$smoking)

level<-a3%>%mutate(smoking=factor(smoking,
                                  levels=c(0,1),
                                  labels=c('non-smoker','smoker'),
                                  exclude=NA),
              urbanrural=factor(urbanrural,
                                levels=c(0,1),
                                labels=c('Rural','urban'),exclude=NA),
              ses=factor(ses,
                         levels=c(1,2,3),
                         labels=c('poor','middle','rich'),exclude=NA))

#labels from expss

labels<-a4%>%apply_labels(main_id='Study id',
                  disease='disease_staus')


save(labels,file='C:/Users/a076924/OneDrive - Syneos Health/Desktop/int/R/labels.RData')


#deriving new vars

der<-a4%>%mutate(htm=ht/100,
                 bmi=wt/htm^2)%>%apply_labels(bmi="Body mass index")

der1<-der%>%
  mutate(newc=case_when(bmi<18.5 ~1,
                                 bmi>=25 ~ 3,
                                 bmi>=18.5 & bmi<25 ~ 2,
                             is.na(bmi) ~ NA
           ),               catc=factor(newc,
                             levels=c(1,2,3),
                           labels=c('under weight','normal weight','over weight'),
                           exclude=NA)
               )

str(der1$catc)

save(der1,file='C:/Users/a076924/OneDrive - Syneos Health/Desktop/int/R/der1.RData')

load('C:/Users/a076924/OneDrive - Syneos Health/Desktop/int/R/der1.RData')

#histogram
library('ggplot2')

der1%>%ggplot(aes(x=wt))+geom_histogram(color='black',fill='skyblue')

#boxplot

der1%>%ggplot(aes(y=ht))+geom_boxplot(outliers = TRUE, outlier.color='red')

level%>%ggplot(aes(y=wt,x=smoking,fill=smoking))+geom_boxplot()

der1%>%mutate(smoking=factor(smoking))%>%ggplot(aes(x=smoking,y=wt))+geom_boxplot()

plot<-level%>%ggplot(aes(x=smoking,y=ht,fill=ses))+
  geom_boxplot(outlier.color='red',outlier.size=2,notch=TRUE)+
  labs(x='smoking status',
       y='Body height in cm',
       fill='socio economic status',
       title='Distrubtion of height')+
theme(plot.title=element_text(hjust=2,face='bold',size=13),
      legend.position='top')

##save plot
ggsave('plot.jpeg',height=20,width=15,dpi=300,device='jpeg',
       path='C:/Users/a076924/OneDrive - Syneos Health/Desktop/int/R')

#summary
library('gtsummary')

level%>%select(disease,smoking,urbanrural,ses,ht,wt,motor,bicycle)%>%
  tbl_summary(missing='no',by=smoking,
              statistic=list(
                all_categorical()~'{n} ({p})',
                all_continuous()~'{mean} + {sd}' 
                
              ))


level%>%select(disease,smoking,ses,ht,wt,ses)%>%
  tbl_summary(by=smoking,missing='no',
              statistic=list(
              all_categorical()~'{n} ({p})',
              all_continuous()~'{N_obs} , {mean}, {median} ,{sd} ,{min},{max}'),
              digits=list(
                all_categorical()~ c(0,1),
                all_continuous()~ c(0,1,1,2,0,0)
              ))%>%
        add_overall(last=TRUE)%>%
    bold_labels%>%
  add_p(test=list(
    all_categorical()~'chisq.test',
    all_continuous()~'t.test'
  ),
  pvalue_fun=~style_pvalue(.x,digits=3))

glm(disease~smoking,data=level,family=binomial(link='logit'))

#date functions

myd<-as.Date('1971-01-01',format='%Y-%m-%d')

?strptime








