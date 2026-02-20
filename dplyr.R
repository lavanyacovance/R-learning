#summarise

df <- data.frame(
  name = c("A", "B", "C", "D"),
  score = c(85, 90, 78, 92)
)

df1<-summarise(df,avg=mean(score),mx=max(score),count=n())


df <- data.frame(
  group = c("X", "X", "Y", "Y", "Y"),
  value = c(10, 20, 15, 25, 30)
)

df1<-df%>%group_by(group) %>% 
  summarise(avg=mean(value),mn=min(value),mx=max(value),sm=sum(value) ,count=n())

mtcars %>% summarise(av=mean(disp),count=n())

mtcars%>%group_by(cyl) %>% summarise(mean(disp),n())

unique(mtcars$disp)


#load adsl datset from admiral
library('admiral')
data(package='admiral')

adsl<-admiral::admiral_adsl

convert_blanks_to_na(adsl)

head(adsl)

vs<-pharmaversesdtm::vs
convert_blanks_to_na(vs)

#dplyr select

adsl_select <-adsl %>%  select(USUBJID,AGE,SEX)

#not select
non_Select <-adsl %>% select(!c(RFSTDTC,RFENDTC))

#select with helpig functions 
#starts_With

adsl%>%select(starts_with('RF'))

adsl %>% select(starts_with('SU'))

#ends with

adsl %>% select(ends_with('DTC'))

treatment<- c('ARM','ARMCD','ACTARM','ACTARMCD')

#all_of

adsl %>% select(all_of(treatment))

#where

adsl %>% select(where(is.numeric))

adsl %>%  select(where(is.character))

#filter

adsl %>% filter(SEX=='F' & AGE >=63)

#&, ", " both will work on same 

chk<-adsl%>%filter(SEX=='F',AGE>=63)
#filter with supporting function %in%

adsl %>% filter(RACE %in% c('WHITE' ))

select<-adsl %>% filter(AGE %in% c(30, 40, 63))

adsl %>%filter(!RACE %in% c('WHITE' ,'ASIAN'))

adsl%>%select(SEX,AGE,RACE)%>%arrange(SEX,desc(AGE))

#mutate case_when

chk<-adsl %>% mutate(SEXN=case_when(SEX=='F'~0,
                                    SEX=='M'~1,
                                    TRUE  ~ NA_integer_),.after=SEX)

unique(adsl$SEXN)


x <- c(1, 2, 3, 4, NA)


x1<- case_when(x %%2==0 ~ 'even',
               
               TRUE  ~ 'odd',
               is.na(x) ~ NA_character_ )

x2<-case_when( x %%2 ==0 ~'even',
               .default = 'odd',
               is.na(x) ~ NA)


sum<-adsl%>%group_by(SEX)%>%summarise(n=n(),
                                      m_age=mean(AGE,na.rm=TRUE),
                                      m_median=median(AGE))


#joins in r

df1 <- data.frame(ID = c(1, 2, 3),
                  Name = c("A", "B", "C"))

df2 <- data.frame(ID = c(2, 3, 4),
                  Score = c(90, 85, 88))

#inner_join : Keeps only rows with matching keys in both tables.

match_both<- inner_join(df1,df2,by='ID')

#left_join Keeps only rows with matching keys in both tables.

left_mat<-left_join(df1,df2,by='ID')

#right_join Keeps all rows from the right table, adds matching data from the left.

rightmat<-right_join(df1,df2,by='ID')

#full_join Keeps all rows from both tables.

all<-full_join(x=df1,y=df2,by='ID')

#semi_join Keeps rows from the left table that have a match in the right table (no duplicates from right).
#if a and not b
semi<-semi_join(df1,df2,by='ID')
#anti join
#if not a and b
anti<-anti_join(df1,df2,by='ID')

#full_join with diferrent senario

df1 <- data.frame(EmployeeID = c(1, 2, 3),
                  Name = c("Alice", "Ben", "Clara"),
                  Department = c("HR", "IT", "Finance"))

df2 <- data.frame(EmployeeID = c(1, 2, 4),
                  Salary = c(50000, 60000, 55000))

full<- full_join(df1,df2,by=c('EmployeeID'))

sales <- data.frame(Month = c("Jan", "Feb", "Mar"),
                    Year = c(2023, 2023, 2023),
                    Sales = c(10000, 12000, 15000))

expenses <- data.frame(Month = c("Jan", "Feb", "Apr"),
                       Year = c(2023, 2023, 2023),
                       Expenses = c(5000, 6000, 5500))

full<- full_join(sales,expenses,by=c('Month','Year'))

install.packages('nycflights13')

library('nycflights13')

org<-flights 

data<-flights %>% drop_na(arr_delay) %>% group_by(carrier) %>% 
  summarise(avg_delay=mean(arr_delay)) %>% 
  arrange(desc(avg_delay))

data1<-flights%>%drop_na(arr_delay)%>%mutate(carrier,arr_delay,avg_delay=mean(arr_delay),.keep="none")

#wt like grouping var
data <-flights %>% count(flight,wt=year,sort=TRUE)

#glimpse

glimpse(mtcars)

