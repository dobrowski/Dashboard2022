
### For Michelle's documents lists of DA qualifying----


da.list <- dash.mry  %>%
    select(districtname, studentgroup, statuslevel, indicator) %>%
    pivot_wider(id_cols = c(districtname,studentgroup),
                names_from = indicator,
                values_from = statuslevel
    ) %>%
    transmute(districtname, 
              studentgroup,
              priority4 = case_when(ela == 1 & math == 1 & elpi == 1 ~ "ELA and Math and ELPI",
                                    ela == 1 & math == 1 ~ "ELA and Math",
                                    elpi == 1 ~ "ELPI",
                                    TRUE ~ ""),
              priority5 = case_when(grad == 1 & chronic == 1 ~ "Graduation and Chronic Absenteeism",
                                    grad == 1 ~ "Graduation",
                                    chronic == 1 ~ "Chronic Absenteeism",
                                    TRUE ~ ""),
              priority6 = case_when(susp == 1 ~ "Suspension",
                                    TRUE ~ ""),
              DA.eligible  = case_when(ifelse(str_length(priority4),1,0) + ifelse(str_length(priority5),1,0) + ifelse(str_length(priority6),1,0) >=2 ~ TRUE,
                                       TRUE ~ FALSE)
    )


da.list %>%
    filter(DA.eligible == "TRUE",
           studentgroup != "ALL") %>%
    left_join_codebook("DASH_CENSUS", "studentgroup") %>%
    select(-studentgroup,-DA.eligible) %>%
    arrange(districtname) %>%
    sheet_write(ss = "128WiDUFJ4GfQEoqSBORh6LMrTTw11hKyhHhWk6Bs4rU",
                sheet = "Qualifying Groups")



### Caryn version ---


dash.mry  %>%
    select(districtname, studentgroup, statuslevel, indicator) %>%
    pivot_wider(id_cols = c(districtname,studentgroup),
                names_from = indicator,
                values_from = statuslevel
    ) %>%
    transmute(districtname, 
              studentgroup,
              ela, math, elpi, grad, chronic, susp,
              priority4 = case_when(ela == 1 & math == 1 & elpi == 1 ~ "ELA and Math and ELPI",
                                    ela == 1 & math == 1 ~ "ELA and Math",
                                    elpi == 1 ~ "ELPI",
                                    TRUE ~ ""),
              priority5 = case_when(grad == 1 & chronic == 1 ~ "Graduation and Chronic Absenteeism",
                                    grad == 1 ~ "Graduation",
                                    chronic == 1 ~ "Chronic Absenteeism",
                                    TRUE ~ ""),
              priority6 = case_when(susp == 1 ~ "Suspension",
                                    TRUE ~ ""),
              DA.eligible  = case_when(ifelse(str_length(priority4),1,0) + ifelse(str_length(priority5),1,0) + ifelse(str_length(priority6),1,0) >=2 ~ TRUE,
                                       TRUE ~ FALSE)
    ) %>%
    filter(DA.eligible == "TRUE",
           studentgroup != "ALL") %>%
    left_join_codebook("DASH_CENSUS", "studentgroup") %>%
    select(-studentgroup,-DA.eligible, -starts_with("priority")) %>%
    arrange(districtname) %>%
    
    sheet_write(ss = "128WiDUFJ4GfQEoqSBORh6LMrTTw11hKyhHhWk6Bs4rU",
                sheet = "Two pager"
    )


