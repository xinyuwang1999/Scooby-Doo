Scooby Doo
================
Amy Wang
2/4/2022

## Introduction

Scooby-Doo is a funny and nervous Great Dane. In its original series, in
each episode, Scooby-Doo hunts ghosts with four human teens, including
Shaggy Rogers (owner and best friend of Scooby-Doo), smart Velma, Fred,
and beautiful Daphne. The show follows the iconic mystery solving
detectives, known as Mystery Inc., as they set out in the Mystery
Machine to solve crime and unmask criminals, bent on revenge or
committing criminal acts for their own personal gain. Scooby-doo is
rewarded with “Scooby-Doo Treats”, dog food that makes him go wild with
joy.[^1]

The *Scooby* dataset was collected from the well-known American
animation show Scooby-Doo, which contains information about every
Scooby-Doo episode and movie’s various variables. It took *plummye*
around 1 year to watch every Scooby-Doo iteration and track every
variable. [^2] The dataset contains 75 columns. Since Scooby-Doo is a
popular show in the US, the IMDB rating is one of the scores to reflect
whether the audiences like or dislike the show. Therefore, in this
project, I will explore some factors that may be related to the IMDB
rating for Scooby-Doo.

## IMDB Rating

The median for the IMDB rating was 7.4, and the mean was 7.3490637,
which indicates that most episodes have pretty high ratings in general.
Most IMDB rating scores are between 7 to 8.

``` r
ggplot(scooby,aes(x = imdb))+
  geom_histogram(fill="#FFF466",color="#FED38C")+
  labs(x = "IMDB Rating",
       y = "",
       title = "The Distribution of IMDB Rating")+ 
  theme_bw()+
  theme(plot.title = element_text(color="#009DDC",face="bold", size=18),
        axis.title.x = element_text(color="#020307",size=14),  
        axis.text = element_text(color="#020307", size = 12))
```

![](Wang-Project-1_files/figure-gfm/IMDB%20Histogram-1.png)<!-- -->

``` r
ggplot(scooby,aes(x = imdb))+
  geom_boxplot(fill="#FFF466",color="#FED38C")+
  labs(x = "IMDB Rating",
       y = "",
       title = "The Boxplot of IMDB Rating")+ 
  theme_bw()+
  theme(plot.title = element_text(color="#009DDC",face="bold", size=18),
        axis.title.x = element_text(color="#020307",size=14),  
        axis.text = element_text(color="#020307", size = 12),
        axis.text.y = element_blank())
```

![](Wang-Project-1_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

The boxplot above suggests that there are still several outliers in the
data. However, since there could be extreme rating scores for the show,
I didn’t omit them from the set.

## Factors that associated with IMDB rating

#### Will monster arrested or not impact IMDB rating?

Because the main characters are trying to solve crime and unmask
criminals in the show, it is interesting to see that if the monster(s)
get arrested or not will influence the IMDB rating.

``` r
scooby%>%
  drop_na(arrested)%>%
  ggplot()+
  aes(x = imdb, fill=arrested)+
  geom_density(alpha = 0.6)+   
  labs(x = "IMDB Rating",
       y = "",
       title = "Effect of Monster Arrested on IMDB Rating",
       subtitle  = "The distribution of IMDB rating by monster arrested")+ 
  theme_bw()+
  scale_fill_manual(values = c("#F7921E","#ACE0EE"),
                    name="",
                    labels=c("Not Arrested", "Arrested"))+
  theme(plot.title = element_text(color="#A44138",face="bold", size=18),
        plot.subtitle = element_text(color = "#B06E0E",size=15),
        legend.position = c(0.7, 0.98),
        legend.justification = c(0,1),
        legend.text = element_text(color="#020307", size = 14),
        axis.title.x = element_text(color="#020307",size=14),  
        axis.text = element_text(color="#020307", size = 12))
```

![](Wang-Project-1_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

As we see from the graph above, when the monster(s) are arrested, those
episodes tend to have higher IMDB ratings.

#### Will the monster type impact IMDB rating?

When we look at the *“monster_type”* column in the dataset, we see there
are some rows containing several monster names, NAs, and typos.
Therefore, before making the graphs, we need to clean the data to
prepare the analysis.

``` r
monster1<-scooby%>%
  separate_rows(monster_type, sep = ",", convert = TRUE)%>%
  drop_na(monster_type)%>%
  filter(monster_type!= "")%>%
  mutate(monster_type=str_trim(monster_type))%>%
  mutate(monster_type = recode(monster_type,
                               Disguise = "Disguised",
                               Disugised = "Disguised", 
                               Possessed = "Possessed Object"))
```

``` r
ggplot(monster1, aes(x = reorder(monster_type,imdb,median,na.rm=TRUE), y = imdb))+
  geom_boxplot(fill ="#79D19F")+
  labs(x = "Monster Type",
       y = "IMDB rating",
       title = "Effect of Monster Type on IMDB Rating",
       subtitle = "The boxplot of IMDB rating by monster type")+
  theme_bw()+
  coord_flip()+
  theme(plot.title = element_text(color="#6352A3",face="bold", size=18),
        plot.subtitle = element_text(color ="#A091C6",size=15),
        axis.title = element_text(color="#020307",size=14),  
        axis.text = element_text(color="#41525C", size = 11))
```

![](Wang-Project-1_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Based on the boxplot, we can see that there are 14 types of monsters.
Those episodes with monster *Skeleton* have the highest IMDB rating.
Those episodes with monster *Super-Villain* have the lowest IMDB rating.
There are still several outliers for each monster type, which may be
caused by the extreme rating scores.

#### Will the monster type impact engagement (Number of reviews on IMDB)?

``` r
ggplot(monster1 , aes(x = monster_type, y = engagement))+
  geom_col( fill ="#B8BE19")+
  labs(x = "Monster Type",
       y = "Number of reviews on IMDB (Engagemnet) ",
       subtitle = "Number of reviews on IMDB (Engagemnet) by monster type",
       title = "Effect of monster type on Engagement")+  
  theme_bw()+ 
  coord_flip()+
  theme(plot.title = element_text(color="#A44138",face="bold", size=18),
        plot.subtitle = element_text(color ="#B06E0E",size=15),
        axis.title = element_text(color="#020307",size=14),  
        axis.text = element_text(color="#41525C", size = 11))
```

![](Wang-Project-1_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

As the bar graph shows, those episodes with monster *Ghost* have the
highest engagement, which indicates that more audiences reviewed those
episodes on IMDB. Those episodes with monster *Dr. Trebal* have the
lowest IMDB rating. There is also an interesting finding from the graph,
monster *Skeleton* has the second-lowest engagement, however, it has the
highest IMDB rating. It can be explained that although there are fewer
people who wrote reviews on IMDB for episodes with monster *Skeleton*,
most of them gave high IMDB ratings for those episodes.

## Conclusion

Through my visualizations of the *Scooby* dataset, we found many factors
that affect the IMDB ratings, such as whether the monster(s) are
arrested or not, and monster type. Since both of those variables are
discrete, it will also be interesting to see if any continuous variable
will have an influence on IMDB ratings.

[^1]: <https://scoobydoo.fandom.com/wiki/Scoobypedia>

[^2]: <https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-07-13/readme.md>
