
# https://www.hudexchange.info/resource/3031/pit-and-hic-data-since-2007/


# libraries ---------------------------------------------------------------

library(tidyverse)
options(tibble.print_max = 80, tibble.print_min = 80) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats
library(glue)
library(lubridate)
library(slider) # part of tidyverse
library(fs)
library(vctrs)
library(vroom)

# input and output
# library(vroom) # do not load vroom, it causes too many problems, but make sure it is installed
library(readxl)
library(openxlsx)
library(data.table)
library(archive)

library(tools)
library(btools)


# constants ---------------------------------------------------------------
uhic_coc <- "https://www.huduser.gov/portal/sites/default/files/xls/2007-2021-HIC-Counts-by-CoC.xlsx"
uhic_state <- "https://www.huduser.gov/portal/sites/default/files/xls/2007-2021-HIC-Counts-by-State.xlsx"
upit_coc <- "https://www.huduser.gov/portal/sites/default/files/xls/2007-2021-PIT-Counts-by-CoC.xlsx"
upit_state <- "https://www.huduser.gov/portal/sites/default/files/xls/2007-2021-PIT-Counts-by-State.xlsx"


# functions ---------------------------------------------------------------



# ONETIME: downloads ------------------------------------------------------
# https://www.hudexchange.info/resource/3031/pit-and-hic-data-since-2007/

files <- c(uhic_coc, uhic_state, upit_coc, upit_state)

file_info(uhic_coc) |> glimpse()
# path_sanitize(uhic_coc)
basename(uhic_coc)

uhic_coc |> 
  fs_path() |> 
  path_ext_remove()

for(file in files){
  fname <- basename(file)
  print(fname)
  download.file(file, here::here("data", fname), mode="wb")
}



# point in time counts ----------------------------------------------------
path <- here::here("data", basename(upit_coc))
excel_sheets(path)

year <- 2020
spit <- read_excel(path, as.character(year))
# $ `CoC Number`                                                                                                <chr> "AK-500", "AK-501", "AL-5…
# $ `CoC Name`                                                                                                  <chr> "Anchorage CoC", "Alaska …

dim(spit) # nrow, ncol
nrow(spit)
glimpse(spit)

f <- function(.year, .path=path){
  print(.year)
  df1 <- read_excel(.path, as.character(.year), col_types = "text")
  if(.year==2021) df2 <- df1 |> 
      rename(cocnum=1, cocname=2, coctype=3, counttype=4) else
        df2 <- df1 |>
          rename(cocnum=1, cocname=2) |> 
          mutate(coctype=NA_character_, counttype=NA_character_) |> 
          select(cocnum, cocname, coctype, counttype, everything())
  dflong <- df2 |> 
    mutate(year=.year) |> 
    pivot_longer(cols=-c(year, cocnum, cocname, coctype, counttype)) |>
    select(year, cocnum, cocname, coctype, counttype, everything())
  dflong
}
f(2021)  
f(2020)
f(2007)

dflong <- purrr::map_dfr(2007:2021, f)

dflong2 <- dflong |> 
  mutate(stabbr=str_sub(cocnum, 1, 2),
         basename=str_extract_before_first(name, ","))

tmp <- count(dflong2, basename) |>
  group_by(n) |> 
  mutate(id = cur_group_id()) |> 
  ungroup()
tmp |> filter(n==max(n))
tmp |> filter(id==12)
tmp |> filter(id==11)
tmp |> filter(id==10)
tmp |> filter(n==min(n))

tmp <- count(dflong2, year, basename) |>
  pivot_wider(names_from = year, values_from = n)
tmp 
# SH supportive housing appeared in 2010
# age breakdowns in 2013
# race, gender in 2015
# nonconforming gender 2017
# 2021: lot of variables not available

(tmp <- count(dflong2, stabbr)) # we have totals and footnotes

dflong3 <- dflong2 |> 
  mutate(vname=case_when(basename=="Overall Homeless" ~ "overall_total",
                         basename=="Sheltered ES Homeless" ~ "shelter_es",
                         basename=="Sheltered TH Homeless" ~ "shelter_th",
                         basename=="Sheltered Total Homeless" ~ "shelter_total",
                         basename=="Unsheltered Homeless" ~ "unsheltered_total"))

count(dflong3, vname)

tmp2 <- dflong3 |> 
  filter(!is.na(vname)) |> 
  select(year, stabbr, cocnum, cocname, vname, value) |> 
  mutate(value=as.numeric(value)) |> 
  pivot_wider(names_from = vname) |> 
  mutate(check1=shelter_total + unsheltered_total,
         check2=shelter_es + shelter_th,
         diff1=check1 - overall_total,
         diff2=check2 - shelter_total,
         pct2=diff2 / shelter_total) |> 
  arrange(desc(abs(pct2)))

# there's no total for 2021 -- footnote: 
# "* Total estimates for overall homeless population and unsheltered homeless
# population were removed due to the limited data available from approximately
# half of CoCs. Summing these columns will not provide an accurate estimate."

dfwide <- dflong3 |> 
  filter(!is.na(cocname)) |> 
  filter(!is.na(vname)) |> 
  select(year, stabbr, cocnum, cocname, vname, value) |> 
  mutate(value=as.numeric(value)) |> 
  pivot_wider(names_from = vname) |>
  mutate(shelter_other=shelter_total - shelter_es - shelter_th) 

# nyc 	NY-600
# san fran CA-501

cnum <- "CA-501"
cnum <- "NY-600"

dfwide |> 
  filter(cocnum==cnum, year<2021) |>
  mutate(value=shelter_total / overall_total) |> 
  mutate(value=shelter_total) |>
  mutate(value=unsheltered_total) |> 
  ggplot(aes(year, value)) +
  geom_line() +
  geom_point()


dfwide |> 
  filter(stabbr=="NY", year==2019) |> 
  arrange(desc(overall_total)) |> 
  mutate(sum=sum(overall_total),
         pct=overall_total / sum,
         cumpct=cumsum(pct)) |> 
  select(year, cocnum, cocname, overall_total, shelter_total, pct, cumpct, sum)
  # relocate(sum, pct, .before=overall_total)


# 2 "a "    3637  footnote at bottom re MO-604
# 3 "DC"    3637
# 4 "GU"    3637
# 5 "MP"    2506  Northern Mariana Islands 
# 6 "PR"    7274
# 7 "VI"    3637
# 8  NA     7274  Totals row

tmp <- dflong2 |> filter(stabbr=="MP")
tmp <- dflong2 |> filter(is.na(stabbr))

dflong3 <- dflong2 |> 
  filter(!stabbr %in% c("* ", "a "), !is.na(stabbr)) |> 
  mutate(value=as.numeric(value))
summary(dflong3) # good, no missing values

(tmp <- count(dflong3, stabbr)) 
setdiff(tmp$stabbr, state.abb) # "DC" "GU" "MP" "PR" "VI" NA  

tmp <- dflong3 |> 
  filter(stabbr=="CA") |> 
  count(cocnum, cocname)
# CA-501 San Francisco CoC
# CA-500 San Jose/Santa Clara City & County CoC

tmp <- dflong3 |> 
  filter(stabbr=="CA", year==2020) |> 
  filter(str_detect(name, "Overall Homeless,")) |> 
  arrange(desc(value))

tmp |> 
  select(year, cocnum, cocname, value)
# year cocnum cocname                                value
# <int> <chr>  <chr>                                  <dbl>
# 1  2020 CA-600 Los Angeles City & County CoC          63706
# 2  2020 CA-500 San Jose/Santa Clara City & County CoC  9605
# 3  2020 CA-502 Oakland, Berkeley/Alameda County CoC    8137
# 4  2020 CA-501 San Francisco CoC                       8124
# 5  2020 CA-601 San Diego City and County CoC           7638
# 6  2020 CA-602 Santa Ana, Anaheim/Orange County CoC    6978
# 7  2020 CA-503 Sacramento City & County CoC            5511
# 8  2020 CA-514 Fresno City & County/Madera County CoC  3641
# 9  2020 CA-609 San Bernardino City & County CoC        3125
# 10  2020 CA-608 Riverside City & County CoC             2884


tmp <- dflong3 |> 
  filter(cocnum=="CA-501") |> 
  filter(str_detect(name, "Overall Homeless,"))

dflong3 |> 
  filter(cocnum=="CA-501") |> 
  filter(str_detect(name, "Sheltered Total Homeless, 2")) |> 
  ggplot(aes(year, value)) +
  geom_point() +
  geom_line()





