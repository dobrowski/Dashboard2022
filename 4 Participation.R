

#  Looks at participation rates 



elpi.partic <- dash.mry.da %>%
    filter(! is.na( flag95pct ),
           nsizemet == "Y")


acad.partic <- dash.mry.da %>%
    filter(! is.na( num_prloss ) ,
           prate_enrolled >= 30) %>%
    select(cds, districtname, studentgroup, studentgroup.long, currstatus, currstatus_without_prloss, statuslevel, indicator) %>%
    left_join(school_dir) %>%
    mutate(status.thresh = case_when(indicator == "grad" ~  68,
                                     indicator == "chronic" ~  20,
                                     indicator == "susp" & doc %in% c(52)  ~  6,  # Elem
                                     indicator == "susp" & doc %in% c("00",54)  ~  8,  # Unified
                                     indicator == "susp" & doc %in% c(56)  ~  9,  # High
                                     indicator == "ela" & doc %in% c(52,"00",54)  ~  -70,  # Elem
                                     indicator == "ela" & doc %in% c(56)  ~  -45,  # High
                                     indicator == "math" & doc %in% c(52,"00",54)  ~  -95,  # Elem
                                     indicator == "math" & doc %in% c(56)  ~  -115,  # High
                                     
    ) ) %>%
    select(cds, districtname, studentgroup, studentgroup.long, currstatus, currstatus_without_prloss, statuslevel, indicator, status.thresh) 






acad.part <- function(schooly) {
    
 dash2 %>%
    filter(! is.na( num_prloss ) ,
            str_detect(schoolname, schooly),
           prate_enrolled >= 30) %>%
    select(cds, districtname, schoolname, studentgroup, studentgroup.long, currstatus, currstatus_without_prloss, statuslevel, indicator) %>%
    left_join(school_dir) %>%
    mutate(status.thresh = case_when(indicator == "grad" ~  68,
                                     indicator == "chronic" ~  20,
                                     indicator == "susp" & doc %in% c(52)  ~  6,  # Elem
                                     indicator == "susp" & doc %in% c("00",54)  ~  8,  # Unified
                                     indicator == "susp" & doc %in% c(56)  ~  9,  # High
                                     indicator == "ela" & doc %in% c(52,"00",54)  ~  -70,  # Elem
                                     indicator == "ela" & doc %in% c(56)  ~  -45,  # High
                                     indicator == "math" & doc %in% c(52,"00",54)  ~  -95,  # Elem
                                     indicator == "math" & doc %in% c(56)  ~  -115,  # High
                                     
    ) ) %>%
    select(cds, districtname, schoolname, studentgroup, studentgroup.long, currstatus, currstatus_without_prloss, statuslevel, indicator, status.thresh) 
}

part.green <- acad.part("Greenfield High")
