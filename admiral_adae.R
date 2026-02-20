#adae
#to load datasets.

library('admiral')
library('pharmaversesdtm')
library('dplyr')
library('lubridate')

ae<-pharmaversesdtm::ae
suppae<-pharmaversesdtm::suppae
adsl<-admiral::admiral_adsl
ex_single<-admiral::ex_single

##blank to na

convert_blanks_to_na(ae)
convert_blanks_to_na(ex_single)

#adsl vars

adsl_vars<-exprs(TRTSDT,TRTEDT,DTHDT,EOSDT)

#merging adsl dataset ae

ae1<-derive_vars_merged(dataset=ae,dataset_add=adsl,by_vars=exprs(STUDYID,USUBJID),
                  new_vars=adsl_vars) 
  
  ae2<-derive_vars_dtm(dataset=ae1,dtc=AESTDTC,highest_imputation='M',
                     new_vars_prefix='AST',min_date=exprs(TRTSDT)) %>% 
    
    derive_vars_dtm(dtc=AEENDTC,new_vars_prefix='AEN',
                    highest_imputation='M',date_imputation='last',
                    time_imputation='last',
                    max_date=exprs(DTHDT,EOSDT)) 
  
  ae3<- ae2 %>%  derive_vars_dtm_to_dt(exprs(ASTDTM,AENDTM))
  
  ae4<-ae3 %>% derive_vars_dy(reference_date = TRTSDT,
                      source_vars=exprs(ASTDT,AENDT))
  
  ae5<-ae4%>%derive_vars_duration(start_date=ASTDT,end_date=AENDT,in_unit='days',
                                 out_unit='days',add_one=TRUE,new_var=ADURN,
                                 new_var_unit=ADURU)

  
  
 ex <-ex %>% derive_vars_dtm(dtc=EXSTDTC,new_var='EXST',
                             ) 