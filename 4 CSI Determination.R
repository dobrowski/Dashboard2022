

library(readxl)

# Title 1 list downloaded from https://www.cde.ca.gov/sp/sw/t1/schoolallocations.asp

title1 <- read_xlsx(here("data","tipaschlallocation2022.xlsx"),
                    range = "A6:E9791") %>%
    rename(cds = `CDS Code`) %>%
    mutate(title1school = TRUE)


### 

csi2 <- dash2 %>%
    filter(rtype == "S",
           studentgroup == "ALL",
           statuslevel != 0
    ) %>%
    pivot_wider(id_cols = c(cds, districtname,schoolname), names_from = indicator, values_from = c(statuslevel, currstatus)   ) %>%
    
    left_join(title1) %>%
    
    rowwise() %>%
   mutate(num.ind =  sum(!is.na(c_across(starts_with("status")))) ,
          sum.ind =  sum(c_across(starts_with("status")), na.rm = TRUE ),
          num.1s =  sum(c_across(starts_with("status")) == 1, na.rm = TRUE ),
          csi.grad = case_when(currstatus_grad <= 68 ~ "Grad",
                               TRUE ~ "No"),
          csi.all =  ifelse(num.ind == sum.ind,"All 1", "No"),
          csi.but.1 =  ifelse(num.ind  - num.1s == 1,"All but 1", "No"),
          csi.majority = case_when(num.ind >= 5  & num.1s >= ceiling(num.ind/2 ) ~ "Majority",
                                   TRUE ~ "No")
          ) %>%
    arrange(cds) %>%
#    mutate(csi = ifelse( (csi.all == "No" & csi.grad == "No"), FALSE, TRUE) 
           mutate(csi = ifelse( (csi.all == "No" & csi.grad == "No" & csi.but.1 == "No"), FALSE, TRUE) 
                  
    )
  

csi2 %>%
    filter(title1school == TRUE) %>% 
    tabyl(csi.but.1)
    

csi.mry <- csi2 %>%
    filter(str_starts(cds,"27"),
           csi == TRUE
           )

clipr::write_clip(csi.mry)
