download.file(url="https://raw.githubusercontent.com/swcarpentry/r-novice-gapminder/gh-pages/_episodes_rmd/data/gapminder-FiveYearData.csv",destfile="./data/gapminder-FiveYearData.csv",method="auto")

download.file(url="https://raw.githubusercontent.com/swcarpentry/r-novice-gapminder/gh-pages/_episodes_rmd/data/gapminder_wide.csv",destfile="./data/gapminder_wide.csv",method="auto")

gapminder <- read.csv("./data/gapminder-FiveYearData.csv")

library(tidyverse)
library(dplyr)

##vignette() #tutorial or examples for all
##vignette("ggplot2-specs") #for 1 library only

year_country_gdp <-select(gapminder, year, country, gdpPercap)

head(year_country_gdp)

#pipe %>% 

year_country_gdp <- gapminder %>% select(year,country, gdpPercap)
year_country_gdp

#look at first 5 rows
head(year_country_gdp)

#don't need to assign first
gapminder %>% select(year,country, gdpPercap) %>% head(10)

gapminder %>% 
  filter(continent=="Europe") %>% 
  select(year,country,gdpPercap) %>% 
  head(10)


gapminder %>% 
  filter(continent == "Africa") %>% 
  select(country,lifeExp,year) %>% 
  head(10)

gapminder %>% 
  filter(continent %in% c("Africa","Europe")) %>% 
  select(country,year,lifeExp) 

#order matters
gapminder %>% 
  select(country,year,lifeExp) %>% 
  filter(continent %in% c("Africa","Europe")) %>% 
  head(10)


mean(gapminder[gapminder$continent == "Africa","gdpPercap"])

gapminder %>% 
  group_by(continent) %>% 
  summarise(mean_gdp=mean(gdpPercap))

gapminder %>% 
  group_by(country) %>% 
  summarise(avg_life=mean(lifeExp)) %>% 
  filter(avg_life ==min(avg_life) | avg_life ==max(avg_life))
  
gapminder %>% 
  group_by(country) %>% 
  summarise(avg_life=mean(lifeExp)) %>% 
  arrange(avg_life)
  
#count n for cals n()
gapminder %>% 
  group_by(country) %>% 
  summarise(se_pop=sd(lifeExp)/sqrt(n()))

#how to add a coumn to the dataframe, default adds new column to end
gapminder %>% 
  mutate(gdp_billion = gdpPercap*pop/10^9) %>% 
  head(10)

#how to add a coumn to the dataframe, adds new col to beginning
gapminder %>% 
  mutate(gdp_billion = gdpPercap*pop/10^9) %>% 
  select (gdp_billion, everything()) %>% 
  head(10)
  
#pipe mod table to ggplot

gapminder %>% 
  #Start/stop tells which characters to look at.
  filter(substr(country,start =1, stop=1) %in% c("A","Z")) %>%
  ggplot(aes(x=year,y=lifeExp,color=continent)) +
  geom_line() +
  #facet makes 1 graph for every country, ~ means display
  facet_wrap(~ country)

#tidy data

str(gapminder)

gap_wide <- read_csv("data/gapminder_wide.csv")
str(gap_wide)

#convert from wide to long 
# long is transposed, where 1 row is like key-value row
gap_wide %>% 
  gather(obs_type,obs_vlues,-continent,-country) %>% 
  head()

my_gap_long <- gap_wide %>% 
  gather(obs_type,obs_vlues,-continent,-country)

#simple
write.table(my_gap_long,"data/my_gap_long.csv")

#formatted
write.table(my_gap_long,"data/my_gap_long_formatted.csv",
            sep=",",quote=FALSE,row.names = FALSE)

names(my_gap_long)

#split column 
my_gap_long2 <- gap_wide %>% 
  gather(obs_type,obs_vlues,-continent,-country) %>% 
  separate(obs_type,into=c("obs_tp","Year"),sep="_")

head(my_gap_long2)

#convert from long to wide
my_gap_wide <- my_gap_long2 %>%
  unite(var_ID, continent,country,sep="_") %>% 
  unite(var_names,obs_tp,Year, sep="_") %>%
  spread(var_names,obs_vlues) %>% 
  separate(var_ID,c("continent","country"),sep="_")  


write.table(my_gap_wide,"data/my_gap_wide_formatted.csv",
            sep=",",quote=FALSE,row.names = FALSE)
