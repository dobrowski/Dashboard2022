
library(vroom)
library(scales)


import_files <- function(dir,globy,naming){
    setwd(dir)
    
    files <- fs::dir_ls(glob = globy)
    
    print(files)
    
    output <- map_df(files, ~vroom(.x, .name_repair = ~ janitor::make_clean_names(., case = naming), id = "YEAR"))
    
    setwd(here())
    
    output
}


dash <- import_files(here("data"),"*txt","none") 

### Coalesce and rename for common column names ------
dash2 <- dash %>%
    mutate(cds = coalesce(cds,CDS),
           rtype = coalesce(rtype,Rtype),
           rtype = coalesce(rtype,RType),
           schoolname = coalesce(schoolname,SchoolName),
           districtname = coalesce(districtname,DistrictName),
           countyname = coalesce(countyname,CountyName),
           charter_flag = coalesce(charter_flag, Charter_Flag),
           coe_flag = coalesce(coe_flag,COE_Flag),
           dass_flag = coalesce(dass_flag,DASS_Flag),
           studentgroup = coalesce(studentgroup,StudentGroup),
           currstatus = coalesce(currstatus, CurrStatus),
           currdenom = coalesce(currdenom,CurrDenom),
           currnumer = coalesce(currnumer,CurrNumer),
           statuslevel = coalesce(statuslevel,StatusLevel),
           certifyflag = coalesce(certifyflag,CertifyFlag), 
           reportingyear = coalesce(reportingyear,ReportingYear),
           ) %>%
    select(-CDS,
           -Rtype,
           -RType,
           -SchoolName,
           -DistrictName,
           -CountyName,
           -Charter_Flag,
           -COE_Flag,
           -DASS_Flag,
           -StudentGroup,
           -CurrStatus,
           -CurrDenom,
           -CurrNumer,
           -StatusLevel,
           -CertifyFlag, 
           -ReportingYear) %>%
    mutate(studentgroup = replace_na(studentgroup,"EL")) %>%
    left_join_codebook("DASH_SUSP", "studentgroup") %>%
    mutate(definition = recode(definition, "Student Group" = "English Learner")) %>% 
    mutate(indicator = str_split_i(YEAR,"_",2)) %>%
    mutate(indicator2 = recode(indicator,
                               "ELA" = "4 -  ELA",
                               "MATH" = "4 -  Math",
                               "ELPI" = "4 - ELPI",
                               "Grad" = "5 - Grad",
                               "Chronic" = "5 - Chronic \nAbsenteeism",
                               "Suspension" = "6 - Suspension"
    )) %>%
    mutate(statuslevel.orig = statuslevel,
           statuslevel = case_when(currdenom >= 30  ~ statuslevel.orig,
                                   currdenom >= 15 & studentgroup %in% c("HOM", "FOS") ~ statuslevel.orig,
                                   TRUE ~ 0
                                       )
           )

### Select Monterey Districts -------

dash.mry <- dash2 %>%
    filter(countyname == "Monterey",
           rtype == "D")



### Determine DA eligibility --------

da.list <- dash.mry  %>%
    select(districtname, studentgroup, statuslevel, indicator) %>%
    pivot_wider(id_cols = c(districtname,studentgroup),
                names_from = indicator,
                values_from = statuslevel
    ) %>%
    transmute(districtname, 
              studentgroup,
              priority4 = case_when(ELA == 1 & MATH == 1 ~ TRUE,
                                    ELPI == 1 ~ TRUE,
                                    TRUE ~ FALSE),
              priority5 = case_when(Grad == 1 | Chronic == 1 ~ TRUE,
                                    TRUE ~ FALSE),
              priority6 = case_when(Suspension == 1 ~ TRUE,
                                    TRUE ~ FALSE),
              DA.eligible  = case_when(priority4+priority5+priority6 >=2 ~ "DA",
                                       TRUE ~ "Not")
    )

dash.mry.da <- left_join(dash.mry, da.list)

### Matrix Graphs -----

dash.graph <- function(df, dist) {
    
    

        # mutate(name = str_replace(name, ":","\n"), # To make the labels wrap to be shorter
        #        definition = str_replace(definition, "D","<br>D"),
        #        definition = str_replace(definition, "/","/<br>"),
        #        `DA Eligible` = ifelse(`DA Status`=="DA" & value == 1, "DA", "Not"),
        #        definition = ifelse( `DA Status`=="DA",
        #                             glue("<span style='color:red'>{definition}</span>"),
        #                             glue("<span style='color:black'>{definition}</span>") # Used to make the axis labels red for DA groups
        #        )
        
    df %>%
        filter(str_detect(districtname,dist),
               statuslevel !=0,
               !is.na(definition)
               ) %>%
    ggplot() +
        geom_tile(aes(y = reorder(definition, desc(definition)),  # Student group
                      x = as.factor(indicator2),  # Indicator
                      fill = factor(statuslevel, levels = c("1","2","3","4","5")),   # Status rating
                     # color = "black",  # as.factor(`DA Eligible`), 
                      width=0.95, # width and heigth are adjusted to allow the color borders to go fully around
                      height=0.95
        ),
        lwd = .75,
        color = "black"
        )  +
        ggthemes::theme_hc() +
    #           geom_text(size = 2, position = position_dodge(width = 1)),
    ggplot2::theme(plot.title.position = "plot") +

    #     mcoe_theme # +
        # scale_fill_brewer(palette = "Purples",
        #                   na.value = "White",
        #                   direction = -1) +
        scale_fill_manual(values = purp.pal,
                          drop = FALSE) +
  #      scale_color_manual( values = da.pal) +
        labs(title = paste0("2022 Dashboard Status by Student Group for ",dist),
             x = "",
             y = ""
        )  +
        guides(#color = guide_legend(title = "DA Eligible",   # Prettify the legends
        #                             title.position = "top",
        #                             label.position = "bottom"
        # ),
         fill = guide_legend(title = "Dashboard Status \nCell Phone Bars",
                            title.position = "top",
                            title.hjust = .5,
                            label.position = "bottom",
                            nrow = 1)
        ) #+
       # theme(axis.text.y = element_markdown())   # Used to make the axis labels red for DA groups
    
    # ggsave(here("figs",glue("{dist} Dashboard Status 2022 - {Sys.Date()}.png")),
    #        width = 8, height = 8)
    # 
}


dash.graph(dash.mry,"Salinas Union")


dash.graph(dash.mry,"Gonzales")


dash.graph(dash.mry,"San Ardo")



# With the addition 11-29 n-size groups

dash.mry %>%
    mutate(statuslevel = statuslevel.orig) %>%
    dash.graph("Gonzales")

# Bright spots


dash.mry %>%
    mutate(statuslevel = statuslevel.orig) %>%
    filter(statuslevel >= 4) %>%
    dash.graph("Carmel")



# Same graph but red highlighting for DA eligibility 

dash.graph.da <- function(df, dist) {
    

    df %>%
        filter(str_detect(districtname,dist),
               statuslevel !=0,
               !is.na(definition)
        ) %>%
        mutate(`DA Eligible` = ifelse(DA.eligible =="DA" & statuslevel == 1 & studentgroup != "ALL", "DA", "Not"),
               definition = ifelse( DA.eligible=="DA" & studentgroup != "ALL",
                                    glue("<span style='color:red'>{definition}</span>"),
                                    glue("<span style='color:black'>{definition}</span>") # Used to make the axis labels red for DA groups
               )
               ) %>%
        ggplot() +
        geom_tile(aes(y = reorder(definition, desc(definition)),  # Student group
                      x = as.factor(indicator2),  # Indicator
                      fill = factor(statuslevel, levels = c("1","2","3","4","5")),   # Status rating
                       color = as.factor(`DA Eligible`), 
                      width=0.95, # width and height are adjusted to allow the color borders to go fully around
                      height=0.95
        ),
        lwd = .75,
 #       color = "black"
        )  +
        ggthemes::theme_hc() +
        #           geom_text(size = 2, position = position_dodge(width = 1)),
        ggplot2::theme(plot.title.position = "plot") +
        scale_fill_manual(values = purp.pal,
                          drop = FALSE) +
        scale_color_manual( values = da.pal) +
        labs(title = paste0("2022 Dashboard Status by Student Group for ",dist),
             x = "",
             y = ""
        )  +
        guides(color = guide_legend(title = "DA Eligible",   # Prettify the legends
                                         title.position = "top",
                                         label.position = "bottom"
             ),
            fill = guide_legend(title = "Dashboard Status \nCell Phone Bars",
                                title.position = "top",
                                title.hjust = .5,
                                label.position = "bottom",
                                nrow = 1)
        ) +
     theme(axis.text.y = element_markdown())   # Used to make the axis labels red for DA groups
    
    # ggsave(here("figs",glue("{dist} Dashboard Status 2022 - {Sys.Date()}.png")),
    #        width = 8, height = 8)
    # 
}


dash.graph.da(dash.mry.da,"Salinas Union") 


dash.graph.da(dash.mry.da,"Gonzales")


dash.graph.da(dash.mry.da,"Mission")


ggsave("Gonzales DA.png", width = 8, height = 8)


###  Chronic ----


    


indicator.bar <- function(df, dist, indie) {
    
    tit <- case_when(indie == "MATH" ~ "Math",
                     indie == "Chronic" ~ "Chronic Absenteeism",
                     indie == "Grad" ~ "Graduation Rate",
                     indie == "ELPI" ~ "English Languague Progress (ELPI)",
                     TRUE ~ indie) 
    
    
    df %>%
        filter( str_detect(districtname, dist),
                indicator == indie,
                statuslevel != 0,
                !is.na(definition)) %>%
        mutate(shifty = ifelse(currstatus >0, 1, -.05 ) ,
               labby = case_when(indie == "MATH" ~ as.character(currstatus),
                                 indie == "ELA" ~ as.character(currstatus),
                                 TRUE ~ percent(accuracy = 0.1, x = currstatus/100)),
               labby.col = ifelse(statuslevel < 4, "white", "black")
               ) %>%
        ggplot( aes(x = reorder(definition, currstatus ),
                    y = currstatus,
                    fill = factor(statuslevel, levels = c("1","2","3","4","5")),
                    label = labby)
        ) + 
        geom_col() +
        geom_text(position = position_dodge2(width = 1),
                  aes(hjust =  shifty, color = labby.col)
        ) +
        coord_flip() +
        ggthemes::theme_hc() +
        ggplot2::theme(plot.title.position = "plot") +
        scale_color_manual(guide = FALSE, values = bw.pal) +
        scale_fill_manual(values = purp.pal,
                          drop = FALSE) +
        labs(title = paste0(tit," by Student Group for ",dist),
             x = "",
             y = ""
        )  +
        guides(fill = guide_legend(title = "Dashboard Status \nCell Phone Bars",
                                   title.position = "top",
                                   title.hjust = .5,
                                   label.position = "bottom",
                                   nrow = 1)
        )
    
}




indicator.bar(dash.mry, "Carmel", "Chronic")

indicator.bar(dash.mry, "North Monterey", "Grad")

indicator.bar(dash.mry, "Salinas Union", "Suspension")


indicator.bar(dash.mry, "Carmel", "MATH")


indicator.bar(dash.mry, "North Monterey", "ELA")


### Save all -----


indicator.list <- dash.mry$indicator %>% unique()

run.everything <- function(dist) {
    
dash.graph(dash.mry,dist)

ggsave(here("figs", dist, paste0(dist," "," Dashboard Basic chart.png")), width = 8, height = 8)

dash.mry %>%
    mutate(statuslevel = statuslevel.orig) %>%
    dash.graph(dist) +
    labs(subtitle = "Including Student Groups with 11-29 students")

ggsave(here("figs", dist, paste0(dist," "," Dashboard Bonus chart.png")), width = 8, height = 8)

dash.mry %>%
    mutate(statuslevel = statuslevel.orig) %>%
    filter(statuslevel >= 4) %>%
    dash.graph(dist) + 
    labs(title = paste0("Bright Spots on 2022 Dashboard by Student Group for ",dist),
         subtitle = "Includes Student Groups at Status Level 4 or 5")


ggsave(here("figs", dist, paste0(dist," "," Dashboard Bright Spot chart.png")), width = 8, height = 8)

dash.graph.da(dash.mry.da,dist)+ 
    labs(title = paste0("2022 Dashboard Status by Student Group with DA Eligibilty for ",dist))

ggsave(here("figs", dist, paste0(dist," "," Dashboard DA chart.png")), width = 8, height = 8)

for (i in indicator.list) {

indicator.bar(dash.mry, dist, i)
    
    ggsave(here("figs", dist, paste0(dist," ",i, " barchart.png")), width = 8, height = 8)
    
}    

}

run.everything("North Monterey County")

run.everything("Santa Rita")
