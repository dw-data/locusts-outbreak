# How East Africa is fighting locusts amid coronavirus

East Africa is still battling its worst locust invasion in decades. Amid
the COVID-19 crisis, countries are fighting to stop a new generation of
locusts swarms.

*In this repository, you will find the methodology, data and code behind
the story that came out of this analysis.*

**Read the full article on DW:**
[English](https://www.dw.com/a-53357078) |
[German](https://www.dw.com/a-53371174)

**Story by:** [Kira
Schacht](https://www.twitter.com/daten_drang)

# Files

| Name          | Content                                                                           |
| ------------- | --------------------------------------------------------------------------------- |
| `locusts.Rmd` | The main R markdown script. Run in RStudio to reproduce this analysis.            |
| `data.RData`  | The R Data file containing the imported datasets. Use if csv import doesn’t work. |
| `data/...`    | Data files                                                                        |

# Data sources

  - [FAO Locust Hub](https://locust-hub-hqfao.hub.arcgis.com/)
  - [FAO Desert Locust Information
    Service](http://www.fao.org/ag/locusts/en/info/info/index.html)
  - [IPC Global Report on Food
    Crises 2020](http://www.ipcinfo.org/ipcinfo-website/featured-stories/news-details/fr/c/1152628/),
    see table page 214-215

# Methodology

Here is a step-by-step-explanation of the code we used in this analysis.
You can explore it yourself by opening `locusts.Rmd` in RStudio.

## Setup

Load necessary packages and presets

``` r
## install and load needs, if not yet present
# install.packages("needs")
library(needs)

# packages used in this markdown document
needs(tidyverse, lubridate, rgdal, broom)
```

## Read data

For this project, we’ll mainly be using this data from the [FAO Locust
Hub](https://locust-hub-hqfao.hub.arcgis.com/). It contains information
on the location and size of loocust groups over time, as well as the
accompanying control operations.

``` r
# Generate file list
file_list = list.files("data",pattern = "geo_[ABHS].*csv", full.names = T)

# Read 4 datasets on locust locationa: Adults, Bands, Hoppers and Swarms, and bind into one dataset
d = lapply(file_list, read.csv, stringsAsFactors = F, na.strings = c(" ", "","NA")) %>%
  bind_rows() %>% select(-(22:134))
rm(file_list)
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
area affected.

## Which countries are most affected?

We want to know which country is currently being hit worst. For this, we
calculate the area affected by swarms by country, as well as the number
of swarms by country, in April 2020.

**We only look at swarms in this step**, since they do the most damage.
Hoppers and Adults are categories of solitary locusts, which are not
dangerous to crops. Bands are young groups of gregarious locusts which
can’t fly yet.

``` r
x = d %>%
  #Filter for only current month, only swarms
  filter(STARTDATE >= as.Date("2020-04-01"), CAT == "Swarm") %>%
  #Calculate area per country
  group_by(COUNTRYID) %>%
  summarise(area = sum(AREAHA, na.rm = F), swarms = n()) %>% 
  arrange(-area)

head(x)
```

    ## # A tibble: 6 x 3
    ##   COUNTRYID    area swarms
    ##   <chr>       <dbl>  <int>
    ## 1 ET        171447.    252
    ## 2 KE         45829.    328
    ## 3 <NA>       27700      33
    ## 4 IR         15600      39
    ## 5 UG           500       5
    ## 6 SS           300       3

``` r
#Ethiopia, Kenya and Iran are most affected by swarms at the moment
```

## How much area was covered by swarms in East Africa in April?

To illustrate the scale of the problem, we calculate the area covered by
locust swarms in East Africa and compare it with Lake Tana, the largest
lake in Ethiopia.

We also know that a swarm covering 1 km² can eat as much as 35000
people, so we can calculate how much food the swarms currently consume,
and compare it to the population of East African countries.

*Countries included:* `ET` Ehtiopia, `KE` Kenya, `UG` Uganda, `SS` South
Sudan, `DJ` Djibouti

*Criteria:* Countries in East Africa which had any swarms in April

``` r
# Sum of affected area in April, in East African countries
a_ha = sum(x$area[x$COUNTRYID %in% c("ET","KE","UG","SS","DJ")]) #218176.2 ha
a_km = a_ha/100 #2182 km²

#How many times Lake Tana (2156 km²) is that?
a_km/2156 #Almost the same  area (1.01 times)
```

    ## [1] 1.011949

``` r
#How much food do this many locusts eat? (1 km² swarm can eat as much as 35000 people)
a_km * 35000 / 10^6 # as much as 76.3 million people
```

    ## [1] 76.36168

``` r
# How much is that compared to the population of Kenya (51.39 mn people) and Somalia (15.01 mn)
a_km  * 35000 / ((51.39+15.01)*10^6) #1.15 times as much
```

    ## [1] 1.150025

``` r
rm(a_ha, a_km)
```

The swarms in East African countries in April alone could have covered
Lake Tana entirely. Swarms of this size eat more per day than the
population of Kenya and Somalia combined.

## How many people will experience food insecurity?

Many locust-affected countries already suffer from food insecurity. To
illustrate the impact of the locust upsurge, we want to see how many
people are are likely to be affected this year.

The [2020 Global Report on Food
Crises](http://www.ipcinfo.org/ipcinfo-website/featured-stories/news-details/fr/c/1152628/)
by the IPC (Integrated Food Security Phase Classification) estimates how
many people will suffer from acute food insecurity at one time in 2020,
and why (see table page 214-215).

We filtered this report for East African countries affected by the
locust upsurge. The figure we looked at is found in the dataset column
`pop_ge_3` and describes the `Estimated population in IPC/CH Phase 3 or
above, in millions, during the 2020 peak period`

  - `Peak period`: The point in 2020 when food insecurity will be
    highest.

  - `IPC/CH PHASE 3 or above`: The IPC measures food insecurity on a
    range from *Phase 1 (“None/Minimal”)* to *5 (“Catastrophe/Famine”)*.
    *Phase 3 (“Crisis”)* is described as a situation where \> Households
    either: \> - Have food consumption gaps that are reflected by high
    or above-usual acute malnutrition; OR \> - Are marginally able to
    meet minimum food needs but only by depleting essential livelihood
    assets or through crisis-coping strategies.
    
    (see IPC GRFC 2020 page 14)

<!-- end list -->

``` r
#Read IPC data
ipc = read.csv2("data/IPC_locusts.csv", stringsAsFactors = F) %>% arrange(pop_ge_3) %>%
  #Exclude Yemen and Pakistan
  filter(country != "Yemen", country != "Pakistan") %>%
  mutate(country = factor(country, levels = country),
         trend = factor(trend, levels = c("Increase", "Stable", "Decrease")))
```

![](locusts_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Map movement of swarms in current outbreak

To illustrate how the current upsurge developed, we created an animated
GIF showing swarms and bands as dots on a map, cycling through the
months. For that, we exported one image file for each month, to be
edited in Illustrator and combined into a GIF.

![](locusts_files/162_en_locusts_map_0.5.gif)

## Area treated vs area affected in current outbreak

How have control operations in Africa managed to keep up with the
growing locust populations? To analyze, we looked at the area affected
by locusts (`AREHA`) versus the area treated with pesticides (`CTLAREA`)
in the current outbreak over time.

*Countries included:* `CD` Democratic Republic of the Congo, `CG` Congo,
`DJ` Djibouti, `EG` Egypt, `ER` Eritrea, `ET` Ethiopia, `KE` Kenya, `LY`
Lybia, `SO` Somalia, `SS` South Sudan, `SU` Sudan, `TZ` Tanzania, `TC`
Chad, `UG` Uganda

*Criteria:* Countries in Africa which which were affected by swarms or
bands since the start of 2018.

``` r
x = d %>%
  filter(between(as.Date(STARTDATE), as.Date("2018-01-01"), as.Date("2020-04-30")),
         COUNTRYID %in% c("CD","CG","DJ","EG","ER","ET","KE","LY","SO","SS","SU","TZ","TC","UG")) %>%
  mutate(month = floor_date(STARTDATE, "month")) %>%
  #remove duplicate locust IDs. multiple types of locusts (e.g. swarms and bands) at same location get multiple entries
  group_by(month, LOCUSTID) %>% summarise(AREAHA = unique(AREAHA), CTLARTREA = unique(CTLARTREA)) %>%
  #calculate affected area and control area by month
  group_by(month) %>% summarise(`Affected area` = sum(AREAHA, na.rm = F), `Control area` = sum(CTLARTREA, na.rm = T)) %>% gather(var, ha, 2:3)
```

![](locusts_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
