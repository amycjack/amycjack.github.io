---
layout: single
title: "TidyTuesday and Hollywood Ages Gaps"
date: "14 Feburary, 2023"
categories: bioinformatics data-visualisation tutorial
tags: data-visualisation tidytuesday
---


This week's TidyTuesday (14/02/2023) is representing the [Hollywood Age Gaps](https://hollywoodagegap.com/). 

[Click here](https://https://github.com/amycjack/TidyTuesdays/blob/main/14.02.23%20Hollywood%20Age%20Gaps) for the repo, or see below for the R code.

![This is an image](https://github.com/amycjack/TidyTuesdays/blob/main/14.02.23%20Hollywood%20Age%20Gaps/gg214022023.png)

```R
age_gaps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-14/age_gaps.csv')

library(rlang)
library(tidyverse)
library(here)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(ggthemes)
library(ggExtra)

men <- age_gaps %>% 
  group_by(release_year) %>% 
  summarise(man_avg_age = mean(actor_1_age))  %>%
  filter(release_year>=2000)


women <- age_gaps %>% 
  group_by(release_year) %>% 
  summarise(women_avg_age = mean(actor_2_age)) %>%
  filter(release_year>=2000)

df <- merge(men,women, by="release_year") %>%
    transform(new.col = man_avg_age - women_avg_age) %>% 
    mutate(new.col = sprintf('%.1f', new.col))

df %>%
  summarise(mean = mean(women_avg_age),
            SE = sd(women_avg_age)) %>%
  mutate(meanpos = mean + 1 *SE,
         meanneg = mean - 1 *SE)-> stats_women

df %>%
  summarise(mean = mean(man_avg_age),
            SE = sd(man_avg_age)) %>%
  mutate(meanpos = mean + 1 *SE,
         meanneg = mean - 1 *SE)-> stats_men


gg <- ggplot(df, aes(x=women_avg_age, xend=man_avg_age, y=release_year)) + 
  geom_segment(aes(x=25, 
                   xend=52, 
                   y=release_year, 
                   yend=release_year), 
               color="#b2b2b2", size=0.15)+
  geom_dumbbell(size=2, color="#bbbbbb", 
                colour_x = "#EE7523", 
                colour_xend = "#6D3A5D",
                dot_guide=FALSE) +
  geom_text(data=filter(df, release_year=="2022"),
            aes(x=women_avg_age, y=release_year, label="Women"),
            color="#EE7523", size=2, vjust=-1.2, fontface="bold", family="Lato") +
  geom_text(data=filter(df, release_year=="2022"),
            aes(x=man_avg_age, y=release_year, label="Men"),
            color="#6D3A5D", size=2, vjust=-1.2, fontface="bold", family="Lato") +
  geom_rect(data=df, aes(xmin=52, xmax=54, ymin=-Inf, ymax=Inf), fill="#efefe3") +
  geom_text(data=df, aes(label=new.col, y=release_year, x=53), fontface="bold", size=2, family="Calibri") +
  geom_text(data=filter(df, release_year=="2022"), aes(x=53, y=release_year, label="Difference"),
              color="#7a7d7e", size=2, vjust=-1.5, fontface="bold", family="Calibri") + 
  theme_bw(base_family="Calibri") +
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border=element_blank()) +
  theme(axis.ticks=element_blank()) +
  theme(plot.title=element_text(face="bold")) +
  theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(b=12))) +
theme(plot.caption=element_text(size=7, margin=margin(t=12), color="#7a7d7e")) 


gg2 <- gg + geom_rect(xmin = stats_women$meanneg, xmax = stats_women$meanpos,
               ymin = 2022, ymax = 2000, fill = "#EE7523", alpha = .005) + 
  geom_rect(xmin = stats_men$meanneg, xmax = stats_men$meanpos,
               ymin = 2022, ymax = 2000, fill = "#6D3A5D", alpha = .005) +
  geom_vline(xintercept = stats_men$mean, linetype = "solid", size = .5, alpha = .8, color = "#762a83") +
  geom_vline(xintercept = stats_women$mean, linetype = "solid", size = .5, alpha = .8, color = "#EE7523") + 
  labs(title = "Average Age of  <span style='color:#6D3A5D'>**Male**</span> and <span style = 'color: #EE7523'>Female</span> leads in Film",
       x = "Age", y = "Film Release Year") +
  theme(plot.title=element_markdown(size=12, family="bell",lineheight=1.3,color="grey60"),
        plot.subtitle=element_markdown(size=8, family="bell",lineheight=1.3,color="grey60"),
        plot.caption=element_text(size=7,family="bell",color="grey40",hjust=.5, margin=margin(t=10)),
        plot.background = element_rect(color = "#ffffff", fill = "#ffffff"),
        legend.background = element_rect(color = NA, fill = "#ffffff"),) +
  labs(caption="#TidyTuesday 14-02-23: Hollywood Age Gaps | Github: amycjack | Twitter: amycjack")


ggsave(paste0("gg2", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 7,
       height =  4.1)
       height = 8)
```
