data<-tibble(article=c('Kill Me If You Dare','The Spy','The World We Make',
                       'Watchman','Mo Gilligan: Momentum','Domino','TUNA GIRL',
                       'Bard of Blood','Dragons: Rescue Riders'),
             type=c('Movie','Tv show','Movie','Movie','Movie','Movie','Movie','Tv show','Movie'),
             duration=c('125 min','100 min','108 min','64 min','89 min','90 min','90 min', '60 min','70min')) 


#seperate
sep1<-data %>% separate(duration,into=c('min','Season'),sep=' ')

sep2<-data %>% separate(duration,into=c('min','season'),sep=' ',convert=TRUE)

#separate_rows

sep3<-data %>% separate_rows(article,sep=' ')

ex1<-tibble(drink=c('Chocolate milk','Orange juice','Cappuccino'),
            ingredients=c('milk 0.3 L; chocolate 40 g; sugar 10 g','oranges 3; sugar 20 g','milk 0.1 L; water 0.1 L; coffee 30 g; sugar 5 g'))


sep4<-ex1 %>% separate_rows(ingredients,sep='; ')%>%
      separate(ingredients,into=c('ingredients','quantity','units'),sep=' ',convert=TRUE)%>%
  group_by(ingredients,units) %>% summarize(tot_qty=sum(quantity))


sep5<-ex1 %>% separate_rows(ingredients,sep='; ')%>%count(ingredients,sort=TRUE)

#pivot_wider & pivot_longer

vs<-pharmaversesdtm::vs

eg<-pharmaversesdtm::eg

vs1<-vs %>% select(USUBJID,VISIT,VSTPT,VSTESTCD,VSORRES)

vs2<-pivot_wider(vs1,names_from=VSTESTCD,values_from = VSORRES)

vs3<pivot_longer(vs2,cols=c('HEIGHT','WEIGHT','DIABP','PULSE','SYSBP','TEMP'),names_to='VSTESTCD',values_to='VSORRES')

table(vs1$VSTESTCD,b=vs$VISIT)

chk<-vs%>%count(VISIT,VSTESTCD)

#fill function

df <- data.frame(
  team = c('A', 'A', 'A', 'A', 'B', 'B', 'B', 'B'),
  points = c(99, 68, 86, 88, 95, 74, 78, 93),
  assists = c(22, NA, 31, 35, 34, NA, 28, 31),
  rebounds = c(30, NA, NA, 24, 30, 36, 30, 29)
)

df1 <- df %>% fill(assists)

df2<- df %>% fill(rebounds, .direction="up")

df3<- df %>%  fill(assists, .direction = "downup")

df4 <- df %>% fill(rebounds, .direction="updown")

#replace_na

df1<-df %>% replace_na(list(assists=0,rebounds=0L))

class(df$assists)
class(df$rebounds)

obesity <- tribble(
  ~country,                 ~female, ~male, ~both_sexes,
  "Afghanistan",              3.2,    7.6,     5.5,
  "Albania",                 21.6,   21.8,    21.7,
  "Algeria",                 19.9,   34.9,    27.4,
  "Andorra",                 25.9,   25.3,    25.6,
  "Angola",                   4.0,   12.1,     8.2,
  "Antigua and Barbuda",     11.6,   25.9,    18.9,
  "Argentina",               27.3,   29.0,    28.3,
  "Armenia",                 17.1,   23.0,    20.2,
  "Australia",               29.6,   28.4,    29.0,
  "Austria",                 21.9,   18.3,    20.1
)

#simple 1
cc=pivot_longer(obesity,cols=c(female,male),names_to='sex',values_to='pct')

  ggplot(cc,aes(x=pct,y=forcats::fct_reorder(country,sex),color=sex))+
  geom_point(size=3)+
    labs(x='percent',y='country',title='graph')+
    theme_minimal()
  
  chk=obesity %>% pivot_longer(col=c(female,male),
                           names_to='sex',
                           values_to='pct',
                           values_drop_na=TRUE,
                           values_transform=list(pct=as.integer))%>%
    ggplot(aes(x=country,y=pct,fill=sex))+
    geom_bar()

  #2 logic
  stock_df <- tibble(
    company = c("Amazon", "Apple", "Facebook", "Google", "Microsoft"),
    '2019_week1' = c(1848, 73.4, 205, 1337, 158),
    '2019_week2' = c(1641, 38.1, 144, 1057, 103),
    '2019_week3' = c(1696, 39.2, 150, 1098, 108)
  )

  stock_df%>%pivot_longer(cols=-company,names_to=c('year','week'),values_to='stock',
                          names_sep='_week',
                          names_transform=list(week=as.integer,
                                               year=as.integer)
                      )
  
  #3 logic

  space_dogs_df =tibble(date=as.Date(c(  "1960-08-19",
                                         "1960-12-01",
                                         "1961-03-25")),
                        result=c(   "Recovered safely",
                                    "Spent one day in orbit",
                                    "One orbit, recovered safely"),
                        name_1=c("Belka", "Mushka", "Zvezdochka"),
                        name_2=c("Strelka", "Pchyolka", NA),
                        gender_1=c("Female", "Female", "Female"),
                        gender_2=c("Female", "Female", NA))
  
  dogs<-space_dogs_df %>% pivot_longer(cols=c(name_1,name_2,gender_1,gender_2),
                     names_sep='_',
                     names_to=c('.value','dogid'),
                     values_drop_na=TRUE)
  
  health_df <-tibble(country=c("Afghanistan", "Albania", "Algeria", "Angola", "Antigua"),
                     '2000_male_pct.obese' = c(1.2, 11.7, 10.3, 1.4, 6.8),
                     '2000_male_life.exp' = c(54.6, 70.4, 69.4, 45.2, 69.7),
                     '2000_female_pct.obese' = c(3.4, 14.7, 24.2, 5.6, 18.2),
                     '2000_female_life.exp' = c(57.3, 76, 72.2, 49.5, 75.1),
                     '2001_male_pct.obese' = c(1.3, 12.0, 10.5, 1.5, 7.0),
                     '2001_male_life.exp'= c(55, 70.6, 69.6, 45.5, 70),
                     '2001_female_pct.obese' = c(3.5, 15, 24.5, 5.8, 18.5),
                     '2001_female_life.exp' = c(57.5, 76.2, 72.5, 49.7, 75.5)
  )
  
  cc<-health_df %>% pivot_longer(-country,names_to=c('year','name','.value'),
                                 names_sep='_',
                                 names_transform=list(year=as.integer))
  
  cc %>% ggplot(aes(x=pct.obese,y=life.exp,color=name))+
                geom_point()
                  
  
  #pivot_wider
  
  exp<-dogs
  
  wid<-exp %>% pivot_wider(id_cols=date,names_from='dogid',values_from='gender',
                   names_prefix='gender_')%>%
    mutate(same_gen=gender_1==gender_2)%>%
    summarize(pct_same_gender=mean(same_gen,na.rm=TRUE))
  
  