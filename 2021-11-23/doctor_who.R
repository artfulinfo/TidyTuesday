#Coded by Tom Vento @ https://www.artfulinfo.net

#Load libraries
library(ggplot2)
library(tidyverse)
library(magrittr)
library(dplyr)
library(sysfonts)
library(extrafont)
font_import()
loadfonts(device = "win")

font_add(family = "Conthrax Sb", regular = "conthrax-sb.ttf",
         bold = "conthrax-sb.ttf")

#Import TidyTuesday data 2021-11-23
directors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/directors.csv')
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/episodes.csv')
writers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/writers.csv')
imdb <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/imdb.csv')

#Left join the supplementary tables to episodes table. 
#Caution: duplication is created due to a many-to-one relationship between writer and episode, so we will need to group before performing certain functions
#df = merge(x=episodes, y=writers, by="story_number",all.x = TRUE)
who = merge(x=episodes, y=imdb, by.x=c("season_number","episode_number"), by.y=c("season","ep_num"),all.x = TRUE)
who = merge(x=who, y=directors, by="story_number",all.x = TRUE)


#Convert air_date to date datatype in place
who %<>%
  mutate(air_date=as.Date(air_date, format = "%d %b. %Y"))

#Remove season 13 as there is incomplete data
who <- who %>% 
  filter(season_number!=13)

#create grouped table to get average rating by director
piv <- aggregate(x = who$rating.x, 
                      by = list(who$director),
                      FUN=mean)

#grab top 5 directors
top <- head(piv[order(-piv$x),], n = 4) %>% 
  mutate(TopBotType="TOP")

#grab bottom 5 directors
bot <- head(piv[order(piv$x),], n = 4) %>% 
  mutate(TopBotType= "Bottom")

#combine top and bottom 5
topbot5 <- rbind(top,bot) %>% 
  mutate(TopBotDir= ifelse(row_number()>4,paste(str_pad(nrow(piv)-(row_number()-5), 2, pad = " "),Group.1), 
          ifelse(row_number()<=4,paste(str_pad(row_number(), 2, pad = " "),Group.1),NA)))

#merge directors onto episode info
who = merge(x=who, y=topbot5, by.x="director",by.y="Group.1", all.x = TRUE, all.y = TRUE,type="full")

#by season breakdown of viewers ratings
whop <- ggplot(who, aes(x=episode_number, y=rating.x, size=uk_viewers,color=TopBotDir))+
  geom_point(alpha=0.9,shape=18)+
  facet_grid(cols = vars(season_number))+
  facet_wrap(~season_number, ncol = 6)+
  scale_fill_viridis_c()+ 
  xlab("Episode Number") + 
  ylab("Episode Rating")+
  ggtitle(expression(atop("Director Who", atop(italic("The top and bottom 5 performing directors by avg. episode rating in the revival era of Doctor Who"), ""))))

#set manual colour pallete - teals for top 5 and purples for bottom 5
cbp2 <- c("#034946", "#07aba3", "#3bf7ed", "#9dfbf6",
          "#e6cffc", "#a855f6", "#760bda", "#350561")

#style the plot and titles
whop + scale_colour_manual("Director",values=cbp2)+
theme(
  panel.background = element_rect(fill = "#e6ffff",
                                  colour = "#e6ffff",
                                  size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.3, linetype = 'solid',
                                  colour = "#99ffff"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "#99ffff"),
  plot.background = element_rect(fill = "white"),
  plot.title = element_text(color="black", size=16, family="Conthrax Sb",hjust = 0.5),
  axis.title.x = element_text(color="black", size=11, family="Conthrax Sb"),
  axis.title.y = element_text(color="black", size=11, family="Conthrax Sb") 

)