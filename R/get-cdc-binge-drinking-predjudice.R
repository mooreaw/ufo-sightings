library(tidyverse)
library(stringr)
library(rvest)

ele <- "table:nth-child(4) td , table:nth-child(4) th"

binge <- "https://www.cdc.gov/alcohol/data-table-text.htm#prevalence" %>%
  read_html %>%
  html_nodes(ele) %>%
  html_text

binge_clean <- data_frame(txt = binge) %>%
  filter(!str_detect(txt, "\\("), txt != "") %>%
  mutate(txt = str_c(txt, "-", lead(txt))) %>%
  filter(!str_detect(txt, "^[[:digit:]]"), !str_detect(txt, "%")) %>%
  separate(txt, c("state", "pct"), "-") %>%
  mutate(pct = as.numeric(pct))

write_csv(binge_clean, "../data/2015-binge-prev-cdc.csv")
