library(stringr)

string<-"this is a string"

cat(string)

string1<- 'if i want to double "qoute", i will use single sring'

cat(string1)

str_view(string1)

#combining string

x=tibble(name=c('kiran',NA))

x1<-x %>% mutate(gre=str_c('hi my name',name,'!'))

x=tibble(det=c('name','father name','mother name'))

x %>%mutate(x1= str_length(det))

x%>% mutate(x2=str_sub(det,2,1))

#str_glue
df <- data.frame(
  name = c("Alice", "Bob", "Charlie"),
  age = c(25, 30, 28),
  city = c("New York", "London", "Sydney"),
  stringsAsFactors = FALSE
)

df1<-df%>%mutate(desc=str_glue("{name} is {age} years old and lives in {city}"))




#base R
#gsub
x <- "This is a fun sentence"

x1<-gsub('fun','great',x)

x <- c('Mavs', 'Mavs', 'Spurs', 'Nets', 'Spurs', 'Mavs')

x2<-gsub('Mavs','M',x)

x <- c('A', 'A', 'B', 'C', 'D', 'D')

x3 <- gsub('A|B|C','x',x)

df <- data.frame(team=c('A', 'B', 'C', 'D'),
                 conf=c('West', 'West', 'East', 'East'),
                 points=c(99, 98, 92, 87),
                 rebounds=c(18, 22, 26, 19))

df1<-df%>%mutate(conf1=gsub('West','W',conf))

df$conf=gsub('East','E',df$conf)

x <- "The jacket was blue"

x1<-gsub('JACKET','shidt',x,ignore.case=TRUE)