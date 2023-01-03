

library(here)
library(tidyverse)
library(MCOE)
library(googlesheets4)

library(RColorBrewer)
library(ggtext)
library(glue)



con <- mcoe_sql_con()

gsheet <- "https://docs.google.com/spreadsheets/d/1J6SYEaGQJrNfYKegi7HZ_NgjMvYXgFa2fCaMxZK-U7M/edit#gid=0"


dash_census <- tbl(con, "DASH_CENSUS") %>% 
    filter(countyname == "Monterey",
           rtype == "D",
           reportingyear  == 2019
    ) %>%
#           head(20) %>%
    collect() 


# districts <- c("Greenfield",
#                "King City", 
#                "Salinas Union",
#                "Santa Rita"
# )
# 
# districts.list <- paste0(districts, collapse = "|")
# 
# 

groups <- dash_census %>%
    filter(subgrouptotal >= 30)


foster <- dash_census %>%
    filter(studentgroup == "FOS" & subgrouptotal >= 15 & subgrouptotal <=30)


groups <- bind_rows(groups,foster) %>%
    select(districtname, studentgroup)


write_sheet(groups, gsheet)
