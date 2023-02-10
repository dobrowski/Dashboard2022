

library(readxl)

# Title 1 list downloaded from https://www.cde.ca.gov/sp/sw/t1/schoolallocations.asp

title1 <- read_xlsx(here("data","tipaschlallocation2022.xlsx"),
                    range = "A6:E9791") %>%
    rename(cds = `CDS Code`) %>%
    mutate(title1school = TRUE)


### 

elpi.sch <- dash2 %>%
    filter(rtype == "S",
           studentgroup == "EL",
           indicator == "elpi",
           statuslevel != 0
    ) %>%
    pivot_wider(id_cols = c(cds, districtname,schoolname), names_from = indicator, values_from = c(statuslevel.orig, currstatus)   )
    
    





csi2 <- dash2 %>%
    filter(rtype == "S",
           studentgroup == "ALL",
           statuslevel != 0
    ) %>%
    pivot_wider(id_cols = c(cds, districtname,schoolname), names_from = indicator, values_from = c(statuslevel, currstatus)   ) %>%
    
    left_join(elpi.sch) %>%
    left_join(title1) %>%
    
    rowwise() %>%
   mutate(num.ind =  sum(!is.na(c_across(starts_with("status")))) ,
          sum.ind =  sum(c_across(starts_with("status")), na.rm = TRUE ),
          num.1s =  sum(c_across(starts_with("status")) == 1, na.rm = TRUE ),
          csi.grad = case_when(currstatus_grad <= 68 ~ "Grad",
                               TRUE ~ "No"),
          csi.all =  ifelse(num.ind == sum.ind,"All 1", "No"),
          csi.but.1 =  ifelse(num.ind  - num.1s == 1 & num.1s > 0 ,"All but 1", "No"),
   #       csi.majority = case_when(num.ind >= 5  & num.1s >= ceiling(num.ind/2 ) ~ "Majority",
   #                                TRUE ~ "No")
          ) %>%
    arrange(cds) %>%
#    mutate(csi = ifelse( (csi.all == "No" & csi.grad == "No"), FALSE, TRUE) 
           mutate(csi = ifelse( (csi.all == "No" & csi.grad == "No" & csi.but.1 == "No"), FALSE, TRUE) 
                  
    )
  
library(janitor)
csi2 %>%
    filter(title1school == TRUE) %>% 
    tabyl(csi.but.1)
    

csi.mry <- csi2 %>%
    filter(str_starts(cds,"27"),
           csi == TRUE
           )

clipr::write_clip(csi.mry)


### ATSI -------


atsi.cde <- read_xlsx(here("data","essaassistance22.xlsx"),
                      range = "A3:AF9949" )



atsi.cde.mry <- atsi.cde %>%
    filter(countyname == "Monterey",
           AssistanceStatus2022 == "ATSI") %>%
    pivot_longer(cols = AA:WH) %>%
    filter(value > 0)



atsi <- dash2 %>%
    filter(str_starts(cds,"27"),
           rtype == "S",
       #    studentgroup == "ALL",
           statuslevel != 0
    ) %>%
    pivot_wider(id_cols = c(cds, districtname,schoolname, studentgroup), names_from = indicator, values_from = c(statuslevel, currstatus)   ) %>%
    
#    left_join(elpi.sch) %>%
    left_join(title1) %>%
    
    rowwise() %>%
    mutate(num.ind =  sum(!is.na(c_across(starts_with("status")))) ,
           sum.ind =  sum(c_across(starts_with("status")), na.rm = TRUE ),
           num.1s =  sum(c_across(starts_with("status")) == 1, na.rm = TRUE ),
           atsi.grad = case_when(currstatus_grad <= 68 ~ "Grad",
                                TRUE ~ "No"),
           atsi.all =  ifelse(num.ind == sum.ind,"All 1", "No"),
           atsi.but.1 =  ifelse(num.ind  - num.1s == 1 & num.1s > 0 ,"All but 1", "No"),
           #       csi.majority = case_when(num.ind >= 5  & num.1s >= ceiling(num.ind/2 ) ~ "Majority",
           #                                TRUE ~ "No")
    ) %>%
    arrange(cds) %>%
    #    mutate(csi = ifelse( (csi.all == "No" & csi.grad == "No"), FALSE, TRUE) 
    mutate(atsi = ifelse( (atsi.all == "No" & atsi.grad == "No" & atsi.but.1 == "No"), FALSE, TRUE) 
           
    )


atsi.mry <- atsi %>%
    filter(str_starts(cds,"27"),
  #         atsi == TRUE
    ) %>%
    select(cds, studentgroup, starts_with("status"), starts_with("atsi")) %>%
    mutate(name = recode(studentgroup,
                                 "MR" = "TOM",
                          #       "ELO" = "EL"
                         )
           )


atsi.joint <- atsi.cde.mry %>%
    left_join(atsi.mry) %>%
    filter(str_detect(districtname, "Salinas City"))





# 
# atsi.scesd <- atsi.mry %>%
#     filter(str_detect(districtname, "Salinas City"),
#            studentgroup %notin% c("ALL","EO","SBA"))
# 
# 











write_csv(atsi.joint, "scesd-atsi v2.csv")
