data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

library(dplyr)
library(ggplot2)
library(maps)
library(stringr)


#-----------Summary Data---------------



#Incarcerations per 100,000 in population in 1970
incar_rate_1970 <- data %>%
  group_by(year) %>%
  filter(year == 1970) %>%
  summarise(ratio = sum(total_jail_pop,na.rm = TRUE)/sum(total_pop,na.rm = TRUE)*100000)%>%
  pull(ratio)

#Incarcerations per 100,000 in population in 2018
incar_rate_2018 <- data %>%
  group_by(year) %>%
  filter(year == 2018) %>%
  summarise(ratio = sum(total_jail_pop,na.rm = TRUE)/sum(total_pop,na.rm = TRUE)*100000)%>%
  pull(ratio)

#How many times higher the total incarceration rate is in 2018 than in 1970
dif_in_incar_rate_multiple <- incar_rate_2018/incar_rate_1970


#location with most incarcerations in 1970
location_highest_1970 <- data %>%
  group_by(year) %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE) & year == 1970) %>%
  select(county_name,state,region)
print(location_highest_1970)

#location with most incarcerations in 2018
location_highest_2018 <- data %>%
  group_by(year) %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE) & year == 2018) %>%
  select(county_name,state,region)
print(location_highest_2018)

#

black_incar_perc_2018 <- data %>%
group_by(year) %>%
  filter(year == 2018) %>%
  summarise(perc_black= sum(black_jail_pop,na.rm = TRUE)/sum(total_jail_pop,na.rm = TRUE))%>%
  pull(perc_black)

black_pop_perc_2018 <- data %>%
  group_by(year) %>%
  filter(year == 2018) %>%
  summarise(perc_black= sum(black_pop_15to64,na.rm = TRUE)/sum(total_pop_15to64,na.rm = TRUE))%>%
  pull(perc_black)

#How many more times the rate of Black people in jail compared to Black people in general population 
dif_in_black_incar_rate_to_total <- black_incar_perc_2018/black_pop_perc_2018

white_incar_perc_2018 <- data %>%
  group_by(year) %>%
  filter(year == 2018) %>%
  summarise(perc_white= sum(white_jail_pop,na.rm = TRUE)/sum(total_jail_pop,na.rm = TRUE))%>%
  pull(perc_white)

white_pop_perc_2018 <- data %>%
  group_by(year) %>%
  filter(year == 2018) %>%
  summarise(perc_white= sum(white_pop_15to64,na.rm = TRUE)/sum(total_pop_15to64,na.rm = TRUE))%>%
  pull(perc_white)

#How many more times the rate of white people in jail compared to white people in general population 
dif_in_white_incar_rate_to_total <- white_incar_perc_2018/white_pop_perc_2018


latinx_incar_perc_2018 <- data %>%
  group_by(year) %>%
  filter(year == 2018) %>%
  summarise(perc_latinx= sum(latinx_jail_pop,na.rm = TRUE)/sum(total_jail_pop,na.rm = TRUE))%>%
  pull(perc_latinx)

latinx_pop_perc_2018 <- data %>%
  group_by(year) %>%
  filter(year == 2018) %>%
  summarise(perc_latinx= sum(latinx_pop_15to64,na.rm = TRUE)/sum(total_pop_15to64,na.rm = TRUE))%>%
  pull(perc_latinx)

#How many more times the rate of Latinx people in jail compared to Latinx people in general population 
dif_in_latinx_incar_rate_to_total <- latinx_incar_perc_2018/latinx_pop_perc_2018


aapi_incar_perc_2018 <- data %>%
  group_by(year) %>%
  filter(year == 2018) %>%
  summarise(perc_aapi= sum(aapi_jail_pop,na.rm = TRUE)/sum(total_jail_pop,na.rm = TRUE))%>%
  pull(perc_aapi)

aapi_pop_perc_2018 <- data %>%
  group_by(year) %>%
  filter(year == 2018) %>%
  summarise(perc_aapi= sum(aapi_pop_15to64,na.rm = TRUE)/sum(total_pop_15to64,na.rm = TRUE))%>%
  pull(perc_aapi)

#How many more times the rate of aapi people in jail compared to aapi people in general population 
dif_in_aapi_incar_rate_to_total <- aapi_incar_perc_2018/aapi_pop_perc_2018

#-------------Plots------------

incar_rate_by_year <- data %>%
  group_by(year) %>%
  mutate(year_incar_rate = sum(total_jail_pop, na.rm = TRUE)/sum(total_pop_15to64,na.rm = TRUE)*100000)%>%
  mutate(black_year_incar_rate = sum(black_jail_pop, na.rm = TRUE)/sum(black_pop_15to64,na.rm = TRUE)*100000)%>%
  mutate(white_year_incar_rate = sum(white_jail_pop, na.rm = TRUE)/sum(white_pop_15to64,na.rm = TRUE)*100000)%>%
  mutate(latinx_year_incar_rate = sum(latinx_jail_pop, na.rm = TRUE)/sum(latinx_pop_15to64,na.rm = TRUE)*100000)%>%
  mutate(aapi_year_incar_rate = sum(aapi_jail_pop, na.rm = TRUE)/sum(aapi_pop_15to64,na.rm = TRUE)*100000)%>%
  mutate(native_year_incar_rate = sum(native_jail_pop, na.rm = TRUE)/sum(native_pop_15to64,na.rm = TRUE)*100000)%>%
  select(year,year_incar_rate, black_year_incar_rate, white_year_incar_rate,latinx_year_incar_rate,aapi_year_incar_rate,native_year_incar_rate)

time_plot <- ggplot(incar_rate_by_year, aes(x = year, color= "Race")) +
  geom_line(mapping = aes(y = year_incar_rate), size=1.5) +
  geom_line(mapping = aes(y = black_year_incar_rate), color="navy",size=1.5) +
  geom_line(mapping = aes(y = white_year_incar_rate), color="aquamarine1",size=1.5) +
  geom_line(mapping = aes(y = latinx_year_incar_rate), color="aquamarine3",size=1.5) +
  geom_line(mapping = aes(y = aapi_year_incar_rate), color="cyan",size=1.5) +
  geom_line(mapping = aes(y = native_year_incar_rate), color="darkcyan",size=1.5) +
  scale_color_manual("Race",values = c("Black" = "navy", "Native American" = "darkcyan","National Incarceration Rate" = "grey40", "Latinx"="aquamarine3", "White"="aquamarine1", "AAPI"="cyan"))+
  labs(y="Incarcerated per 100,000 people", x="Year", title="Incarceration Rates of Each Race Per Year (By Incarcerated per 100,000)")+
  xlim(1990,NA)

time_plot

#

x <- data %>%
  filter(year==2018)%>%
  mutate(perc_black = black_pop_15to64/total_pop_15to64*100)%>%
  mutate(perc_black_incar = black_jail_pop/total_jail_pop*100)%>%
  select(perc_black_incar,perc_black)

variable_plot <- ggplot(x) +
  geom_point(mapping = aes(x=perc_black, y=perc_black_incar))+
  geom_smooth(mapping = aes(x=perc_black, y=perc_black_incar))+
  ylim(0,100)+
  labs(y="Percent of Prisoners Being Black(%)", x="Percent of Total Population Being Black(%)", title="% of Black People in Prison as % of Black People in General Population Increase")

variable_plot


#--------------Map------------

WA_data <- data %>%
  filter(year=="2018")%>%
  filter(state=="WA")%>%
  mutate(county=tolower(str_remove_all(county_name," County")))%>%
  mutate(perc_black_incar = black_jail_pop/total_jail_pop*100)%>%
  select(county,perc_black_incar)

WA_map <- map_data("county","washington") %>%
  rename(county = subregion) %>%
  left_join(WA_data, by="county")


WA_black_incar_perc <- ggplot(WA_map)+
  geom_polygon(mapping=aes(x=long,y=lat,group=group,fill=perc_black_incar),color="white", size=0.2)+
  coord_map()+
  scale_fill_gradient("% of Incarcerated being Black",low="thistle1", high="magenta4")+
  labs(x="Longitude", y="Latitude", title="Percentage of Incarcerated Being Black by WA County")

WA_black_incar_perc











