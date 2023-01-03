



school_dir <- tbl(con, "SCHOOL_DIR") %>%
    collect() %>%
    rename("cds" = "cds_code")



exit.crit <- dash.mry.da %>%
    left_join(school_dir) %>%
    filter(rtype == "D",
           DA.eligible == "DA",
           statuslevel == 1,
           studentgroup != "ALL"
           ) %>%
    mutate(status.thresh = case_when(indicator == "grad" ~  68,
                                     indicator == "chronic" ~  20,
                                     indicator == "susp" & doc %in% c(52)  ~  6,  # Elem
                                     indicator == "susp" & doc %in% c("00",54)  ~  8,  # Unified
                                     indicator == "susp" & doc %in% c(56)  ~  9,  # High
                                     indicator == "ela" & doc %in% c(52,"00",54)  ~  -70,  # Elem
                                     indicator == "ela" & doc %in% c(56)  ~  -45,  # High
                                     indicator == "math" & doc %in% c(52,"00",54)  ~  -95,  # Elem
                                     indicator == "math" & doc %in% c(56)  ~  -115,  # High
                                     
                                     ),
           change.thresh = case_when(indicator == "grad" ~  68,
                                       indicator == "chronic" ~ currstatus -.5,
                                     indicator == "susp" & doc %in% c(52)  ~  currstatus -.3,  # Elem
                                     indicator == "susp" & doc %in% c(00,54)  ~  currstatus -.3,  # Unified
                                     indicator == "susp" & doc %in% c(56)  ~  currstatus -.5,  # High
                                     indicator == "ela"   ~ currstatus + 3,  
                                     indicator == "math"   ~ currstatus + 3,  
                                     
           )
    ) %>%
    rowwise() %>%
    mutate(
           thresh = case_when(indicator == "grad" ~  max(change.thresh,status.thresh),
                                indicator == "chronic" ~ max(change.thresh,status.thresh),
                                indicator == "susp"   ~  max(change.thresh,status.thresh),
                                indicator == "ela"   ~ min(change.thresh,status.thresh),
                                indicator == "math"   ~ min(change.thresh,status.thresh)
           ),
           pass.count = case_when(# indicator %in% c('math','ela') ~ thresh,
                     indicator %in% c('chronic','susp') ~ floor( currdenom*thresh/100),
                     indicator %in% c('grad') ~  ceiling( currdenom*thresh/100)
           ),
           comper = case_when(indicator %in% c('math','ela') ~ '',
                     indicator %in% c('chronic','susp') ~ 'or less',
                     indicator %in% c('grad') ~ 'or more'
           
                      ),
           adjective = case_when(indicator == 'chronic' ~ 'chroncially absent',
                            indicator == 'susp' ~ 'suspended',
                            indicator == 'grad' ~ 'graduate'
                              
           )
           ) %>%
    select(cds, districtname, studentgroup.long, currstatus, currdenom , indicator, ends_with("thresh"), pass.count, comper, adjective) %>%
    mutate(#sentence_short = glue("{studentgroup.long}  of {currdenom}"),
            sentence_full = ifelse(indicator %in% c("ela","math"),
            glue("{studentgroup.long} student group should have an average of {thresh} from standard or higher based on the {indicator} CAASPP exam to not be in Red."),
                glue("{studentgroup.long} student group should have {pass.count} {comper} students {adjective} based on the count in 2022 of {currdenom} to not be in Red.")
    )
    )



