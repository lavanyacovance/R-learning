install.packages(c('dplyr', 'lubridate', 'stringr','tibble','pharmaversesdtm','admiral'))

install.packages("admiraldev")

library('dplyr')
library('lubridate')
library('stringr')
library('tibble') 
library('pharmaversesdtm')
library('admiral')
library('admiraldev')


dm<-pharmaversesdtm::dm
ex<-pharmaversesdtm::ex
ds<-pharmaversesdtm::ds
vs<-pharmaversesdtm::vs
ae<-pharmaversesdtm::ae
lb<-pharmaversesdtm::lb
vs<-pharmaversesdtm::vs
admiral_adsl<-admiral::admiral_adsl

#use_ad_template("adsl")

dm<-convert_blanks_to_na(dm)
ds<-convert_blanks_to_na(ds)
ex<-convert_blanks_to_na(ex)
ae<-convert_blanks_to_na(ae)
lb<-convert_blanks_to_na(lb)
vs<-convert_blanks_to_na(vs)


#user defind function

#race group

format_racegrp<-function(x){
  case_when(toupper(x)=='WHITE'~'white',
         !is.na(toupper(x))=='WHITE'~'non-white',
            TRUE ~ "Missing")}
  
#names(dm)<-tolower(names(dm))

#names(ex)=tolower(names(ex))
 
  
#age group  
  
age_group<-function(x){
  case_when(x<18~"<18",
            between(x,18,64)~"18-64",
            x>64~">64",
            TRUE~"Missing")
} 

#region 

region_group<-function(x){
  case_when(x %in% c('CAN', 'USA') ~ "NA",
            !is.na(x)~'Row',
            TRUE~'Missing')
}

format_eoss<-function(x){
  case_when(toupper(x)=="COMPLETED" ~ "Completed",
            toupper(x)=="SCREEN FAILURE" ~ NA_character_,
            !is.na(x) ~ "Discontinued",
            TRUE ~ "ONGOING")
}

#racegr1=format_racegrp(RACE),

dm1<-dm%>%mutate(agegr1=age_group(AGE),
                 region1=region_group(COUNTRY))




ex_st<-ex %>% derive_vars_dtm(dtc=EXSTDTC,time_imputation='first',
                              new_vars_prefix='EXST')


ex_en<-ex%>%derive_vars_dtm(dtc=EXENDTC,new_vars_prefix='EXEN',
                            time_imputation='last')           

#derive trt vras

adsl<-dm %>% mutate(trt01p=ARMCD,trt01a=ACTARMCD)%>%
  derive_vars_merged(dataset_add=ex_st,by_vars=exprs(STUDYID,USUBJID),
                   new_vars=exprs(TRTSDTM=EXSTDTM,TRSTMF=EXSTTMF),
                     filter_add=(EXDOSE>=0 & !is.na(EXSTDTM) & str_detect(toupper(EXTRT),'PLACEBO')),
                     order=exprs(USUBJID,EXSTDTM,EXSEQ),
                     mode='first')%>%
  derive_vars_merged(dataset_add=ex_en,by_vars=exprs(STUDYID,USUBJID),
                     filter_add=(EXDOSE>=0 & str_detect(toupper(EXTRT),"PLACEBO")),
                     new_vars=exprs(TRTEDTM=EXENDTM,TRTETMF=EXENTMF),
                     order=exprs(STUDYID,EXENDTM,EXSEQ),
                     mode='last')%>%
  derive_vars_dtm_to_dt(source_vars=exprs(TRTSDTM,TRTEDTM))%>%
  derive_var_trtdurd()

#names(ds)=toupper(names(ds))

ds<-derive_vars_dt(dataset=ds,
               dtc=DSSTDTC,
               new_vars_prefix='DSST',
               )
  

#screen failure date

#names(ds)<-toupper(names(ds))

#ds<-ds%>%select(-c(DOMIAN))

adsl<-adsl%>%
  derive_vars_merged(dataset_add=ds,
                     by_vars=exprs(STUDYID,USUBJID),
                     filter=(DSDECOD=='SCREEN FAILURE' & DSCAT=='DISPOSITION EVENT'),
                     new_vars=exprs(SCRFDT=DSSTDT))%>%
  derive_vars_merged(dataset_add=ds,by_vars=exprs(STUDYID,USUBJID),
                     filter=(DSCAT=='DISPOSITION EVENT' & DSDECOD!='SCREEN FAILURE'),
                     new_vars=exprs(EOSDT=DSSTDT))

ads11<-adsl%>%
  derive_vars_merged(dataset_add=ds,by_vars=exprs(STUDYID,USUBJID),
                     filter=(DSCAT=='DISPOSITION EVENT'),
                     new_vars=exprs(EOSTT=format_eoss(DSDECOD)),
                     missing_values=exprs(EOSTT='Ongoing'))

adsl2<-ads11 %>% 
  derive_vars_merged(dataset_add=ds,by_vars=exprs(STUDYID,USUBJID),
                     filter=(DSCAT=='OTHER EVENT' & DSDECOD=='FINAL RETRIEVAL VISIT'),
                     new_vars=exprs(FRUDT=DSSTDT)) %>%
  
  derive_vars_merged(dataset_add=ds,by_vars=exprs(STUDYID,USUBJID),
                     filter=(DSDECOD=='RANDOMIZED' ),
                      new_vars=exprs(RANDT=DSSTDT)) %>% 
  
  derive_vars_dt(dtc=DTHDTC,new_vars_prefix='DTH',date_imputation='first',
                 highest_imputation='M'
                 ) %>% 
  derive_vars_duration(new_var=DTHDY,
                       start_date=DTHDT,
                       end_date=TRTSDT) %>%

derive_vars_duration(new_var=LDDTHELD,start_date=TRTEDT,end_date=DTHDT,add_one=FALSE)

#dthcause

adsl3<-derive_vars_extreme_event(dataset=adsl2,by_vars=exprs(STUDYID,USUBJID),
                                 events=list(event(dataset='ae',condition=AEOUT=='FATAL',
                                                   set_values_to=exprs(DTHCAUS=AEDECOD,DTHDOM='ae')),
        event(dataset='ds',condition=DSDECOD=='Death',set_values_to=exprs(DTHCAUS=DSDECOD,DTHDOM='Ds'))),
        source_dataset=list(ae=ae,ds=ds),
        tmp_event_nr_var=event_nr,
        order=exprs(event_nr),
        mode='first',
         new_vars=exprs(DTHCAUS,DTHDOM)) 

  adsl4<-adsl3 %>% mutate(DTHCGR1=case_when(DTHDOM=='ae' ~ 'Adverseevent',
          str_detect(DTHCAUS,'PROGRESSIVE DISEASE|DISEASE RELAPSE')~'PROGRESSIVE DISEASE',
         .default = 'OTHER',
         is.na(DTHDOM)~NA_character_))
                     
  #LSTALVDT
adsl5<- derive_vars_extreme_event(adsl4,by_vars=exprs(STUDYID,USUBJID),
             events=
         list(event(dataset_name='ae',condition=!is.na(AESTDTC),
   set_values_to=exprs(LSTALVDT=convert_dtc_to_dt(AESTDTC,highest_imputation='M'),seq=AESEQ ),
         order=exprs(AESTDTC,AESEQ)),
         event(dataset_name='ae',condition=!is.na(AEENDTC),
 set_values_to=exprs(LSTALVDT=convert_dtc_to_dt(AEENDTC,highest_imputation='M'),seq=AESEQ),
               order=exprs(AEENDTC,AESEQ)),
         event(dataset_name='lb',condition=!is.na(LBDTC),
  set_values_to=exprs(LSTALVDT=convert_dtc_to_dt(LBDTC,highest_imputation='M'),seq=LBSEQ),
                                   order=exprs(LBDTC,LBSEQ)),
event(dataset_name='adsl4',condition=!is.na(TRTEDT),
                     set_values_to=exprs(LSTALVDT=TRTEDT,seq=NA_integer_),order=exprs(TRTEDT))),
              source_datasets=list(ae=ae,lb=lb,adsl4=adsl4),
         tmp_event_nr_var=event_nr,
         order=exprs(LSTALVDT,seq,event_nr),
         mode='last',
         new_vars=exprs(LSTALVDT))

#lddthgr1

format_lddthgr1<-function(x){
  case_when(x<=30~'<=30',
  x>30~'>30',
  .default=NA_character_)
}
  
  final_adsl<-adsl5 %>% mutate(RACEGR1=format_racegrp(RACE),
         AGEGR1=age_group(AGE),
         REGION1=region_group(COUNTRY),
         LDDTHGR1=format_lddthgr1(LDDTHELD),
       DTH30FL=if_else(LDDTHGR1<=30, 'Y',NA_character_),
       DTHA30FL=if_else(LDDTHGR1>30,'Y',NA_character_)) %>% 
    
    derive_var_merged_exist_flag(dataset_add=ex,by_vars=exprs(STUDYID,USUBJID),
                                 new_var=SAFFL,
                                 condition=EXDOSE>=0 & str_detect(EXTRT,'PLACEBO'),
                                 false_value='N',
                                 missing_value='N')
colnames(final_adsl)
getwd()


save(final_adsl,file='./adsl.rda')

load('./adsl.rda')