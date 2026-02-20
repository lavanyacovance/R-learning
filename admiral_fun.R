templates <- list_all_templates()
temp_onco<-list_all_templates('admiralonco')
use_ad_template("adsl")
use_ad_template("adae")
use_ad_template("adlb")
use_ad_template(adam_name="ADRS",package='admiralonco')

convert_blanks_to_na(c("a", "b", "", "d", ""))

df <- tribble(
  ~USUBJID,   ~RFICDTC,
  "1001", "2000-01-01",
  "1002", "2001-01-01",
  "1003",           ""
)
class(df)
print(df)
convert_blanks_to_na(df)

#race group

#derive_vars_dtm

#dataset with incomplete dates

mhdt<-tribble(~mhstdtc,
              "2019-07-18T15:25:40",
              "2019-07-18T15:25",
              "2019-07-18",
              "2019-02",
              "2019",
              "2019---07",
              "")

#highest_imputation:M ; it means imputation will do until month
dtm<-derive_vars_dtm(dataset=mhdt,highest_imputation="M",
                     new_vars_prefix='ast',
                     dtc=mhstdtc)



dt1<-derive_vars_dtm(dataset=mhdt,dtc=mhstdtc,highest_imputation="D",
                     new_vars_prefix='ast')

dt2<-derive_vars_dtm(dataset=mhdt,dtc=mhstdtc,new_vars_prefix='ast',
                     highest_imputation='Y')

#to keep both char & numeric

check<-derive_vars_dtm(dataset=mhdt,dtc=mhstdtc,new_vars_prefix='mhstdt',date_imputation='first',
                       time_imputation='first')



#merge

names(vs)<-tolower(names(vs))


com<-
  derive_vars_merged(dataset=vs,dataset_add=select(dm,-('domain')),
                      by_vars=exprs(studyid,usubjid))%>%
  select(usubjid,vstestcd,visit,vsstresn,age,ageu,arm,armcd)


#merge dm+vs

com1<-derive_vars_merged(dataset=dm,
       dataset_add=select(vs,c('usubjid','vstestcd','vsstresn','vsdtc'))
   ,  by_vars=exprs(usubjid)
   , new_vars=exprs(weighbl=vsstresn),
       filter_add=(vstestcd=='WEIGHT'),
       order=exprs(usubjid,convert_dtc_to_dtm(vsdtc)),
       mode='last')




pra1<-derive_vars_merged(dataset=dm,dataset_add=select(vs,-domain),
                         by_vars=exprs(studyid,usubjid),
        filter_add=(vstestcd=='WEIGHT'),
        order=exprs(convert_dtc_to_dtm(vsdtc)),
        mode='last',
        new_vars=exprs(lstwtcat=if_else(visit=='BASELINE','Baseline','Post-Baseline')),
        exist_flag=wtchk,
        true_value="Y",
        false_value='Missing')

help(package="admiral")
ls("package:admiral")



#derive_vars_dtm_to_dt

sl<-tibble(USUBJID=c("01-701-1015", "01-701-1028"),
           ASTDTM=as.POSIXct(c("2021-06-01 08:35:00", "2021-06-05 14:20:00")),
           AENDTM=as.POSIXct(c("2021-06-10 17:00:00", "2021-06-15 09:15:00")))


derive_vars_dtm_to_dt(dataset=sl,source_vars=exprs(ASTDTM,AENDTM))

#derive_vars_trtdurd

du<-tibble(USUBJID=c("01-001", "01-002", "01-003"),
           ASTDT=as.Date(c("2021-01-01", "2021-02-15", "2021-03-10")),
           AENDT=as.Date(c("2021-01-10", "2021-02-20", "2021-03-15")))

du1<-derive_var_trtdurd(dataset=du,start_date=ASTDT,end_date=AENDT)


#derive_vars_dt
dt<-tibble(usubjid=c("01-701-1015", "01-701-1023", "01-701-1028"),
           stdtc=c("2021-05-12", "2021-05", "2021") )

dt1<-derive_vars_dt(dataset=dt,dtc=stdtc,highest_imputation='M',
                   new_vars_prefix='std',date_imputation='first',flag_imputation='date')


mhd<-tibble(mhstdtc=c("2019-07-18T15:25:40",
                        "2019-07-18T15:25",
                        "2019-07-18",
                        "2019-02",
                        "2019",
                        "2019---07",
                        ""))

# Create ASTDT and ASTDTF
# No imputation for partial date
mhd1<-derive_vars_dt(dataset=mhd,dtc=mhstdtc,new_vars_prefix='mhst')

# Create ASTDT and ASTDTF
# Impute partial dates to first day/month
mhd2<-derive_vars_dt(dataset=mhd,dtc=mhstdtc,new_vars_prefix='mhst',
                     highest_imputation='M',
                     )


# Impute partial dates to 6th of April
mhd2<-derive_vars_dt(dataset=mhd,dtc=mhstdtc,new_vars_prefix='mhst',
                     date_imputation = "04-06",
                     highest_imputation='M')

# Create AENDT and AENDTF
# Impute partial dates to last day/month
mhd3<-derive_vars_dt(dataset=mhd,dtc=mhstdtc,new_vars_prefix='AEN',
                    highest_imputation='M',
                    date_imputation='last')

# Create BIRTHDT
# Impute partial dates to 15th of June. No Date Imputation Flag
mhd4<-derive_vars_dt(dataset=mhd, dtc=mhstdtc,new_vars_prefix='BIRTH',
                                         date_imputation='06-15',
                     highest_imputation = 'M',
                      flag_imputation='none'   )
 
# Impute AE start date to the first date and ensure that the imputed date
# is not before the treatment start date

data=tibble(aestdtc=c("2020-12","2020-11"),
            trtsdtm=ymd_hms('2020-12-06T12:12:12','2020-12-06T12:12:12'))

mhd5<-derive_vars_dt(dataset=data,dtc=aestdtc,new_vars_prefix='ast',
                     highest_imputation='M',
                     min_dates=exprs(trtsdtm))

# A user imputing dates as middle month/day, i.e. date_imputation = "mid" can
# use preserve argument to "preserve" partial dates.  For example, "2019---07",
# will be displayed as "2019-06-07" rather than 2019-06-15 with preserve = TRUE

derive_vars_dt(
  data=mhd1,
  new_vars_prefix = "AST",
  dtc = mhstdtc,
  highest_imputation = "M",
  date_imputation = "mid",
  preserve = TRUE
)

# templated load

library('admiral')

list_all_templates(package='admiral')


#derive_vars_duration

dt<-data.frame(subjid=c('01','02','03'),
               stdt=as.Date(c("2021-01-01", "2021-02-15", "2021-03-10")),
               endt=as.Date(c("2021-01-10", "2021-03-01", "2021-03-15")))

dt1<-derive_vars_duration(dt,new_var=dur,start_date=stdt,end_date=endt,out_unit="minutes",
                          floor=TRUE,
                          add_one=FALSE)
               
               
  #derive_vars_extreme_event
adsl <- tribble(
  ~STUDYID, ~USUBJID, ~TRTEDT, ~DTHDT,
  "PILOT01", "01-1130", ymd("2014-08-16"), ymd("2014-09-13"),
  "PILOT01", "01-1133", ymd("2013-04-28"), as.Date(""),
  "PILOT01", "01-1211", ymd("2013-01-12"), ymd(""),
  "PILOT01", "09-1081", ymd("2014-04-27"), ymd(""),
  "PILOT01", "09-1088", ymd("2014-10-09"), ymd("2014-11-01"),
)

lb <- tribble(
  ~STUDYID,  ~DOMAIN,  ~USUBJID, ~LBSEQ,             ~LBDTC,
  "PILOT01",    "LB", "01-1130",    219, "2014-06-07T13:20",
  "PILOT01",    "LB", "01-1130",    322, "2014-08-16T13:10",
  "PILOT01",    "LB", "01-1133",    268, "2013-04-18T15:30",
  "PILOT01",    "LB", "01-1133",    304, "2013-05-01T10:13",
  "PILOT01",    "LB", "01-1211",      8, "2012-10-30T14:26",
  "PILOT01",    "LB", "01-1211",    162, "2013-01-08T12:13",
  "PILOT01",    "LB", "09-1081",     47, "2014-02-01T10:55",
  "PILOT01",    "LB", "09-1081",    219, "2014-05-10T11:15",
  "PILOT01",    "LB", "09-1088",    283, "2014-09-27T12:13",
  "PILOT01",    "LB", "09-1088",    322, "2014-10-09T13:25"
) %>% 
  mutate(ADT=convert_dtc_to_dt(dtc=LBDTC))

#last known alive date LSTALVDT
chk<-derive_vars_extreme_event(adsl,by_vars=exprs(STUDYID,USUBJID),
              events=list(
                event(dataset_name='adsl',condition=!is.na(DTHDT),
                       set_values_to=exprs(LSTALVDT=DTHDT,DTHFL='Y')),
                event(dataset_name='lb',condition=!is.na(ADT),
                      order=exprs(ADT),
                       mode='last',set_values_to=exprs(LSTALVDT=ADT,DTHFL='N')),
              event(dataset_name='adsl',condition=!is.na(TRTEDT),
                    order=exprs(TRTEDT),set_values_to=exprs(LSTALVDT=TRTEDT,DTHFL='N'))),
              source_datasets=list(adsl=adsl,lb=lb),
              tmp_event_nr_var=event_nr,
              order=exprs(LSTALVDT,event_nr),
              mode='last',
              new_vars=exprs(LSTALVDT,DTHFL))


adsl <- tribble(
  ~STUDYID,  ~USUBJID,
  "STUDY01", "PAT01",
  "STUDY01", "PAT02",
  "STUDY01", "PAT03"
)
ae <- tribble(
  ~STUDYID, ~USUBJID, ~AESEQ, ~AEDECOD, ~AEOUT, ~AEDTHDTC,
  "STUDY01", "PAT01", 12, "SUDDEN DEATH", "FATAL", "2021-04-04",
  "STUDY01", "PAT01", 13, "CARDIAC ARREST", "FATAL", "2021-04-03",
)

ds <- tribble(
  ~STUDYID, ~USUBJID, ~DSSEQ, ~DSDECOD, ~DSTERM, ~DSSTDTC,
  "STUDY01", "PAT02", 1, "INFORMED CONSENT OBTAINED", "INFORMED CONSENT OBTAINED", "2021-04-03",
  "STUDY01", "PAT02", 2, "RANDOMIZATION", "RANDOMIZATION", "2021-04-11",
  "STUDY01", "PAT02", 3, "DEATH", "DEATH DUE TO PROGRESSION OF DISEASE", "2022-02-01",
  "STUDY01", "PAT03", 1, "DEATH", "POST STUDY REPORTING OF DEATH", "2022-03-03"
)



#DTHCUASE

chk<-derive_vars_extreme_event(dataset=adsl,by_vars=exprs(STUDYID,USUBJID),
                               events=list(event(
                                 dataset='ae',conditio=AEOUT=='FATAL',set_values_to=exprs(DTHCAUS=AEDECOD,
                                                DTHDT=convert_dtc_to_dt(AEDTHDTC)),
                                                order=exprs(DTHDT)
                               ),
                               event(dataset='ds',condition=DSDECOD=='DEATH',
                                     set_values_to=exprs(DTHCAUS=DSTERM,DTHDT=convert_dtc_to_dt(DSSTDTC)),
                                     order=exprs(DHDT))),
                               source_datasets=list(ae=ae,ds=ds),
                               tmp_event_nr_var=event_nr,
                               order=exprs(DTHDT),
                               mode='first',
                               new_vars=exprs(DTHCAUS,DTHDT))


#derive_var_merged_exist_flag

dm<-tribble(
   ~domain, ~usubjid, ~age, ~ageu,
     "dm" ,  "101" , "71" ,"years",
     "dm" , "102" , "84", "years",
     "dm", "103", "72","years"
)

ae<-tribble(
  ~domain,~usubjid,~aeterm,~aerel,
  "ae","101","erythema","possible",
  "ae","102","pruritus","probable",
  "ae","103","syncope","possible",
  "ae","103","syncope","probable"
)

derive_var_merged_exist_flag(dataset=dm,dataset_add=ae,by_vars=exprs(usubjid),
                              condition=toupper(aerel)=="PROBABLE",
                              new_var=AERELFL)

vs<-tibble(usubjid=rep(c('101','102','101'),each=4),
          domain= rep(c('dm'),times=12),
          vsblfl=c('NA','Y','NA','Y','Y','Y','NA','Y','Y','Y','Y','Y'),
          visit=rep(c('screening','baseline','week4'),times=c(2,1,1)) %>% rep(times=3),
          vstestcd=rep(c('height','weight'),times=c(1,3))%>%rep(times=3)) %>% 
  mutate(vsstresn=ifelse(vstestcd=='height',runif(12,min=150,max=170),runif(12,min=60,max=90)))

derive_var_merged_exist_flag(dataset=dm,dataset_add=vs,
    by_vars=exprs(usubjid),filter_add=vstestcd=='weight' & vsblfl=='Y',
     new_var=WTBLHFL,
     condition=vsstresn>90,
     false_value='N',
     missing_value='M')

#derive_vars_dy

datain=tribble(
  ~TRTSDTM, ~ASTDTM,~AENDT,
  "2014-01-17T23:59:59", "2014-01-18T13:09:O9", "2014-01-20"
)

derive_vars_dy(datain,reference_date=TRTSDTM,
               source_vars=exprs(ASTDTM,AENDT))