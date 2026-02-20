library("ggplot2")

#line plot 

gdf<-tibble(year=c(NA,NA,NA,2019,NA,NA,NA,202),
            quarter=c('q1','q2','q3','q4','q1','q2','q3','q4'),
            sales=c(12498,20461,19314,20314,13494,19314,23640,22920))

  
gdf1 <- gdf %>%fill(year,.direction="downup") %>%
  ggplot(aes(x=quarter,y=sales,color=year,group=year))+
          geom_line(colour='blue')


stock_df <- tibble(
  company = c("Amazon", "Apple", "Facebook", "Google", "Microsoft"),
  '2019_week1' = c(1848, 73.4, 205, 1337, 158),
  '2019_week2' = c(1641, 38.1, 144, 1057, 103),
  '2019_week3' = c(1696, 39.2, 150, 1098, 108)
)

longer<-stock_df%>%pivot_longer(-company,names_to=c('year','week'),
                        values_to='price',
                        names_sep='_week',
                        names_transform=list(year=as.integer,
                                             week=as.integer))

longer%>%ggplot(aes(x=year,y=price,color=company))+
       geom_line()+facet_grid(.~year)


#geom_text 

planets_df <- tibble(
  planet = rep(c("Mercury", "Venus", "Earth", "Mars"),each=3),
  metric = rep(c("diameter", "distance_to_sun", "temperature"),times=4),
value = c(4879, 57.9, 167,
          12104, 108, 464,
          12756, 150, 15,
          6792, 228, -65))


planets_df %>% pivot_wider(names_from='metric',values_from='value')%>%
  ggplot(aes(x=distance_to_sun,y=temperature))+
  geom_point()
  geom_text(aes(label=planet),vjust=-1)+
    labs(x = "Distance to sun (million km)", 
         y = "Mean temperature (°C)") +
    theme(legend.position = "none")
