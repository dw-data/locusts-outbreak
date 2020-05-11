How East Africa is fighting locusts amid coronavirus
====================================================

East Africa is still battling its worst locust invasion in decades. Amid
the COVID-19 crisis, countries are fighting to stop a new generation of
locusts swarms.

*In this repository, you will find the methodology, data and code behind
the story that came out of this analysis.*

**Read the full article on DW:**
[English](https://www.dw.com/a-53357078) \|
[German](https://www.dw.com/a-53371174)

**Story by:** [Kira Schacht](https://www.twitter.com/daten_drang)


Data sources
============

-   [FAO Locust Hub](https://locust-hub-hqfao.hub.arcgis.com/)
-   [FAO Desert Locust Information
    Service](http://www.fao.org/ag/locusts/en/info/info/index.html)
-   [IPC Global Report on Food Crises
    2020](http://www.ipcinfo.org/ipcinfo-website/featured-stories/news-details/fr/c/1152628/)

Methodology
===========

Setup
-----

Load necessary packages and DW color presets

``` r
## install and load needs, if not yet present
# install.packages("needs")
library(needs)

# packages used in this markdown document
needs(tidyverse, lubridate, dwplot)

#DW colours
dwcols = c("hellblau" = "#00A5FF", "mittelblau" = "#0064b4", "dunkelblau" = "#002d5a", "grey1" = "#f1f3f5")
dw_grey = c("grey14" = "#323c45", "grey13" = "#3b444d", "grey12" = "#4b545c", "grey11" = "#5c666e",
            "grey10" = "#6d7780", "grey9" = "#7f8891", "grey8" = "#9099a3", "grey7" = "#a1abb4", "grey6" = "#b2bcc5",
            "grey5" = "#bfc7ce", "grey4" = "#cbd2d8", "grey3" = "#d8dde2", "grey2" = "#e4e8eb", "grey1" = "#f1f3f5") %>% rev()
dw_info = c("hellblau" = "#00a5ff", "dunkelblau" = "#002d5a", "orangerot" = "#d44820",
            "grün" = "#96be00", "rot" = "#be232d", "gelb" = "#f0c80f")
dw_gradient = c("blau6" = "#002d5a", "blau5" = "#004887", "blau4" = "#0064b4",
                "blau3" = "#007acd", "blau2" = "#008fe6", "blau1" = "#00a5ff",
                "gelb" = "#f0c80f", "orangegelb" = "#f0aa00", "Hellorange" = "#ee8c0a",
                "Orange" = "#eb6e14", "Orangerot" = "#d44820", "rot" = "#be232d")
```

Read data
---------

For this project, we’ll mainly be using this data from the [FAO Locust
Hub](https://locust-hub-hqfao.hub.arcgis.com/). It contains information
on the location and size of loocust groups over time, as well as the
accompanying control operations

``` r
# Read 4 datasets on locust locationa: Adults, Bands, Hoppers and Swarms, and bind into one dataset
d = lapply(list.files("data",pattern = "geo_[ABHS].*csv", full.names = T), read.csv,
           stringsAsFactors = F, na.strings = c(" ", "","NA")) %>%
  bind_rows() %>% select(-(22:134))

# Clean up dataset: Convert dates to timestamp, rename location data columns to be more descriptive
d = d %>% mutate(
  STARTDATE = gsub(" 00:00:00","",STARTDATE) %>% paste(TmSTARTDAT) %>% as.POSIXct(tz = "GMT"),
  FINISHDATE = gsub(" 00:00:00","",FINISHDATE) %>% paste(TmFINISHDA) %>% as.POSIXct(tz = "GMT"),
  CTLSTDATE = gsub(" 00:00:00","",CTLSTDATE) %>% gsub("1899/12/30",NA,.) %>% as.Date,
  CTLFNDATE = gsub(" 00:00:00","",CTLFNDATE) %>% gsub("1899/12/30",NA,.) %>% as.Date) %>% 
  rename(LONG = X, LAT = Y) %>% select(-FID, -TmSTARTDAT, -TmFINISHDA)
  
# Save dataset
save.image("data.RData")

# Check: Share of NAs per column
d %>% summarise_all(funs(sum(is.na(.)))) %>% t() %>% as.data.frame %>%
  mutate(share = round((V1/nrow(d))*100,2), var = row.names(.)) %>% select(var, na = V1, share)
```

The `AREAHA` column, which notes the affected area, is never missing,
which suggests a value of 0 default. So we might might underestimate the
area affected if we use this column.

Which countries are most affected?
----------------------------------

We want to know who is currently being hit worst. For this, we calculate
the area affected by swarms by country, as well as the number of swarms
by country, in April 2020. We leave out Bands, Hoppers and Adults for
this. Hoppers and Adults are categories of solitary locusts, which are
not dangerous to crops. Bands are young groups of gregarious locusts
which can’t fly yet.

``` r
x = d %>%
  filter(STARTDATE >= as.Date("2020-04-01"), CAT %in% c("Swarm")) %>%
  mutate(month = floor_date(STARTDATE, "month")) %>% 
  group_by(month, COUNTRYID) %>% 
  summarise(area = sum(AREAHA, na.rm = F), swarms = n()) %>% 
  arrange(month, -area)

head(x)
```

    ## # A tibble: 6 x 4
    ## # Groups:   month [1]
    ##   month               COUNTRYID    area swarms
    ##   <dttm>              <chr>       <dbl>  <int>
    ## 1 2020-04-01 00:00:00 ET        171447.    252
    ## 2 2020-04-01 00:00:00 KE         45829.    328
    ## 3 2020-04-01 00:00:00 <NA>       27700      33
    ## 4 2020-04-01 00:00:00 IR         15600      39
    ## 5 2020-04-01 00:00:00 UG           500       5
    ## 6 2020-04-01 00:00:00 SS           300       3

``` r
#Ethiopia, Kenya and Iran are most affected by swarms at the moment

#sum of swarm area in april

a_ha = sum(x$area[x$COUNTRYID %in% c("ET","KE","UG","SS","DJ","OM")]) #218206.2 ha
a_km = a_ha/100 #2182 km2

#how many times nairobi city /county area (696 km^2) is that?
a_km/696 #around 3 times
```

    ## [1] 3.135147

``` r
#how much food do they eat? 1km^2 swarm can eat as much as 35000 people 
a_km * 35000 / 10^6 # as much as 75.3 million people
```

    ## [1] 76.37218

``` r
# compared to the population of Kenya (51.39 mn people) and Somalia (15.01 mn)
a_km  * 35000 / ((51.39+15.01)*10^6) #1.15 times as much
```

    ## [1] 1.150183

How many people will experience food insecurity?
------------------------------------------------

``` r
ipc = read.csv2("data/IPC_locusts.csv", stringsAsFactors = F) %>% 
  arrange(pop_ge_3) %>% filter(country != "Yemen", country != "Pakistan") %>%
  mutate(country = factor(country, levels = country),
         trend = factor(trend, levels = c("Increase", "Stable", "Decrease")))

ggplot(ipc, aes(country, pop_ge_3, fill = trend, label = pop_ge_3)) + geom_col() +
  geom_text(size = 25, hjust = 0, nudge_y = 0.3) +
  coord_flip() +
  ggtitle("Food insecurity is already high\nin many locust-affected countries") +
  #Theme settings
  scale_y_continuous(expand = expand_scale(add = c(0,3))) +
  scale_x_discrete(expand = expand_scale(add = c(0,0))) +
  scale_fill_manual(values = c(dw_info[1], dw_grey[9], dw_info[2]) %>% unname) +
  theme(panel.grid.major.x = element_line(color = dw_grey[12], size = .5),
        panel.grid.major.y = element_blank())
```

![](locusts_files/figure-markdown_github/unnamed-chunk-4-1.png)

Map movement of swarms in current outbreak
------------------------------------------

line chart: area affected by swarms over time by country

``` r
x = d %>%
  filter(between(as.Date(STARTDATE), as.Date("2018-01-01"), as.Date("2020-03-31")),
         CAT %in% c("Swarm"), COUNTRYID %in% c("SO","ET","KE")) %>%
  mutate(month = floor_date(STARTDATE, "month")) %>% 
  group_by(month, COUNTRYID) %>% 
  summarise(area = sum(AREAHA, na.rm = F), swarms = n()) %>% 
  arrange(month, -area)
ggplot(x, aes(month, area/100, colour = COUNTRYID)) + geom_line() + labs(y = "km^2") +
  ggtitle("Area affected by locust swarms in current outbreak by country")
```

![](locusts_files/figure-markdown_github/unnamed-chunk-5-1.png)

Locations of swarms in April

``` r
needs(rgdal, broom)
#source:https://thematicmapping.org/downloads/world_borders.php
shp <- readOGR("data/TM_WORLD_BORDERS_SIMPL-0.3/TM_WORLD_BORDERS_SIMPL-0.3.shp", "TM_WORLD_BORDERS_SIMPL-0.3",
               stringsAsFactors=FALSE, verbose = F)
tmp = shp@data %>% mutate(id = seq(0,nrow(.)-1) %>% as.character)
shp = shp %>% tidy() %>% left_join(tmp, by = "id");rm(tmp)
```

``` r
#shp_plot = merge(shp, swingstates, by="ks", all.y=T)
d_plot = d %>% filter(STARTDATE >= as.Date("2018-01-01"), CAT %in% c("Swarm","Band")) %>% 
  mutate(month = floor_date(STARTDATE, "month")) %>% arrange(month)

months = unique(d_plot$month)
for(i in 1:length(months)){
  plot = ggplot(data=d_plot %>% filter(month == months[i]), aes(x=LONG, y=LAT)) + 
    geom_polygon(data=shp, aes(x=long, y=lat, group=group), colour = "black", alpha = 0.1) +
    geom_point(aes(colour = CAT), size = 5) +
    scale_x_continuous(limits = c(-20,80)) + scale_y_continuous(limits = c(-5,35)) +
    scale_colour_manual(values = dw_info[2:1] %>% unname) +
    theme_void() + theme(legend.position = "none") +
    coord_map()
  ggsave(paste0("plots/locusts_map_",months[i],".svg"), plot, "svg",
         width=1920/72, height = 1920/72, units = "in", dpi = 72)
}
```

Area treated in current outbreak by country over time
-----------------------------------------------------

``` r
x = d %>%
  filter(between(as.Date(STARTDATE), as.Date("2018-01-01"), as.Date("2020-04-30")),
         COUNTRYID %in% c("CD","CG","DJ","EG","ER","ET","KE","LY","SO","SS","SU","TZ","TC","UG")) %>%
  mutate(month = floor_date(STARTDATE, "month")) %>%
  group_by(month, LOCUSTID) %>% summarise(AREAHA = unique(AREAHA), CTLARTREA = unique(CTLARTREA)) %>%
  group_by(month) %>% summarise(`Affected area` = sum(AREAHA, na.rm = F), `Control area` = sum(CTLARTREA, na.rm = T))

plot = ggplot(x, aes(month, ha/100, color = var)) + geom_line(size = 5) + labs(y = "km^2") +
  ggtitle("Pesticides are sprayed over thousands\nof km2 each month to control locusts") +
  #Theme settings
  scale_y_continuous(expand = expand_scale(mult = c(0,.05))) +
  scale_color_manual(values = dw_info[2:1] %>% unname) +
  theme(legend.position = "none",
        panel.grid.major.x = element_line(color = dw_grey[12], size = .5))
```
