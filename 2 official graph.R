
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



### Coalesce and rename for common column names ------
# 
# # Import the original preview files 
# dash <- import_files(here("data"),"*txt","none") 
# 
# 
# dash2 <- dash %>%
#     mutate(cds = coalesce(cds,CDS),
#            rtype = coalesce(rtype,Rtype),
#            rtype = coalesce(rtype,RType),
#            schoolname = coalesce(schoolname,SchoolName),
#            districtname = coalesce(districtname,DistrictName),
#            countyname = coalesce(countyname,CountyName),
#            charter_flag = coalesce(charter_flag, Charter_Flag),
#            coe_flag = coalesce(coe_flag,COE_Flag),
#            dass_flag = coalesce(dass_flag,DASS_Flag),
#            studentgroup = coalesce(studentgroup,StudentGroup),
#            currstatus = coalesce(currstatus, CurrStatus),
#            currdenom = coalesce(currdenom,CurrDenom),
#            currnumer = coalesce(currnumer,CurrNumer),
#            statuslevel = coalesce(statuslevel,StatusLevel),
#            certifyflag = coalesce(certifyflag,CertifyFlag), 
#            reportingyear = coalesce(reportingyear,ReportingYear),
#            ) %>%
#     select(-CDS,
#            -Rtype,
#            -RType,
#            -SchoolName,
#            -DistrictName,
#            -CountyName,
#            -Charter_Flag,
#            -COE_Flag,
#            -DASS_Flag,
#            -StudentGroup,
#            -CurrStatus,
#            -CurrDenom,
#            -CurrNumer,
#            -StatusLevel,
#            -CertifyFlag, 
#            -ReportingYear) %>%
#     mutate(studentgroup = replace_na(studentgroup,"EL")) %>%
#     left_join_codebook("DASH_SUSP", "studentgroup") %>%
#     mutate(definition = recode(definition, "Student Group" = "English Learner")) %>% 
#     mutate(indicator = str_split_i(YEAR,"_",2)) %>%
#     mutate(indicator2 = recode(indicator,
#                                "ELA" = "<br><img src='icons/1ela.png' width='40' /><br>4 -  ELA",
#                                "MATH" = "<br><img src='icons/2math.png' width='40' /><br>4 -  Math",
#                                "ELPI" = "<br><img src='icons/3elpi.png' width='40' /><br>4 - ELPI",
#                                "Grad" = "<br><img src='icons/4grad.png' width='40' /><br>5 - Grad",
#                                "Chronic" = "<br><img src='icons/5chronic.png' width='40' /><br>5 - Chronic <br>Absenteeism",
#                                "Suspension" = "<br><img src='icons/6suspend.png' width='40' /><br>6 - Suspension"
#     )) %>%
#     mutate(statuslevel.orig = statuslevel,
#            statuslevel = case_when(currdenom >= 30  ~ statuslevel.orig,
#                                    currdenom >= 15 & studentgroup %in% c("HOM", "FOS") ~ statuslevel.orig,
#                                    TRUE ~ 0
#                                        )
#            )
# 



### Using the SQL tables ----


dash2 <- tbl(con,"DASH_ALL_2022") %>%
    collect () %>%
    mutate(indicator2 = recode(indicator,
                               "ela" = "<br><img src='icons/1ela.png' width='40' /><br>4 -  ELA",
                               "math" = "<br><img src='icons/2math.png' width='40' /><br>4 -  Math",
                               "elpi" = "<br><img src='icons/3elpi.png' width='40' /><br>4 - ELPI",
                               "grad" = "<br><img src='icons/4grad.png' width='40' /><br>5 - Grad",
                               "chronic" = "<br><img src='icons/5chronic.png' width='40' /><br>5 - Chronic <br>Absenteeism",
                               "susp" = "<br><img src='icons/6suspend.png' width='40' /><br>6 - Suspension"
    ))


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
              priority4 = case_when(ela == 1 & math == 1 ~ TRUE,
                                    elpi == 1 ~ TRUE,
                                    TRUE ~ FALSE),
              priority5 = case_when(grad == 1 | chronic == 1 ~ TRUE,
                                    TRUE ~ FALSE),
              priority6 = case_when(susp == 1 ~ TRUE,
                                    TRUE ~ FALSE),
              DA.eligible  = case_when(priority4+priority5+priority6 >=2 ~ "DA",
                                       TRUE ~ "Not")
    )

dash.mry.da <- left_join(dash.mry, da.list)

### Matrix Graphs -----

dash.graph <- function(df, dist) {
    

    df %>%
        filter(str_detect(districtname,dist),
               statuslevel !=0,
               !is.na(studentgroup.long)
               ) %>%
    ggplot() +
        geom_tile(aes(y = reorder(studentgroup.long, desc(studentgroup.long)),  # Student group
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
    ggplot2::theme(plot.title.position = "plot")    +
        
        theme(axis.text.x = element_markdown(color = "black", size = 11) ) +
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
                            nrow = 1
                            )
        ) +
        theme(legend.key.size = unit(2, 'cm' ))#+
       # theme(axis.text.y = element_markdown())   # Used to make the axis labels red for DA groups
    
    # ggsave(here("figs",glue("{dist} Dashboard Status 2022 - {Sys.Date()}.png")),
    #        width = 8, height = 8)
    # 
}


dash.graph(dash.mry,"Carmel") 


ggsave(here("figs", dist, paste0(dist," "," Dashboard Basic chart.png")), width = 8, height = 6)


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
               !is.na(studentgroup.long)
        ) %>%
        mutate(`DA Eligible` = ifelse(DA.eligible =="DA" & statuslevel == 1 & studentgroup != "ALL", "DA", "Not"),
               studentgroup.long = ifelse( DA.eligible=="DA" & studentgroup != "ALL",
                                    glue("<span style='color:red'>{studentgroup.long}</span>"),
                                    glue("<span style='color:black'>{studentgroup.long}</span>") # Used to make the axis labels red for DA groups
               )
               ) %>%
        ggplot() +
        geom_tile(aes(y = reorder(studentgroup.long, desc(studentgroup.long)),  # Student group
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
        theme(axis.text.x = element_markdown(color = "black", size = 11) ) +  # For Icons on axis
        
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
        theme(legend.key.size = unit(2, 'cm' )) +
     theme(axis.text.y = element_markdown())   # Used to make the axis labels red for DA groups
    
    # ggsave(here("figs",glue("{dist} Dashboard Status 2022 - {Sys.Date()}.png")),
    #        width = 8, height = 8)
    # 
}


dash.graph.da(dash.mry.da,"Carmel") 



dash.graph.da(dash.mry.da,"Gonzales")


dash.graph.da(dash.mry.da,"Mission")


ggsave("Gonzales DA.png", width = 8, height = 8)


###  Indicator Bar Graphs ----


school_dir <- tbl(con, "SCHOOL_DIR") %>%
    collect() %>%
    rename("cds" = "cds_code")


indicator.bar <- function(df, dist, indie) {
    
    
    
    
    
    tit <- case_when(indie == "math" ~ "<img src='icons/2math.png' width='40' /> Math",
                     indie == "chronic" ~ "<img src='icons/5chronic.png' width='40' /> Chronic Absenteeism",
                     indie == "grad" ~ "<img src='icons/4grad.png' width='40' /> Graduation Rate",
                     indie == "elpi" ~ "<img src='icons/3elpi.png' width='40' /> English Languague Progress (ELPI)",
                     indie == "ela" ~ "<img src='icons/1ela.png' width='40' /> ELA",
                     indie == "susp" ~ "<img src='icons/6suspend.png' width='40' /> Suspension",
                     TRUE ~ indie) 
    
    subtit <- case_when(indie == "math" ~ "Points represent average Distance from Standards",
                     indie == "chronic" ~ "Percentage of students missing at least 10% of days",
                     indie == "grad" ~ "Percentage of four-year cohort graduates",
                     indie == "elpi" ~ "Percentage of EL that improve on the ELPAC",
                     indie == "ela" ~ "Points represent average Distance from Standards",
                     indie == "susp" ~ "Percentage of students Suspended at least 1 full day",
                     TRUE ~ indie) 
    
    
doc <-    school_dir %>%
        filter(str_detect(district, dist),
               school == "No Data") %>%
    select(doc)%>% 
    unlist()
    
    verts <- case_when(indie == "math" & doc %in% c(52,"00",54) ~ c(-95,-25,0,35),
                       indie == "math" & doc %in% c(56) ~ c(-115,-60,0,25),
                        indie == "chronic" ~ c(20,10,5,2.5),
                        indie == "grad" ~ c(95,90.5,80,68),
                        indie == "elpi" ~ c(65,55,45,35),
                        indie == "ela" & doc %in% c(52,"00",54) ~ c(-70,-5,10,45),
                       indie == "ela" & doc %in% c(56) ~ c(-45,0,30,75),
                       indie == "susp" & doc %in% c(52) ~ c(6,3,1.5,0.5),
                       indie == "susp" & doc %in% c("00",54) ~ c(8,4.5,2.5,1),
                       indie == "susp" & doc %in% c(56) ~ c(9,6,3.5,1.5)
                       ) 
    
 
    df %>%
        filter( str_detect(districtname, dist),
                indicator == indie,
                statuslevel != 0,
                !is.na(studentgroup.long)) %>%
        mutate(shifty = ifelse(currstatus >0, 1, -.05 ) ,
               labby = case_when(indie == "math" ~ as.character(currstatus),
                                 indie == "ela" ~ as.character(currstatus),
                                 TRUE ~ percent(accuracy = 0.1, x = currstatus/100)),
               labby.col = ifelse(statuslevel < 4, "white", "black")
               ) %>%
        ggplot( aes(x = reorder(studentgroup.long, currstatus ),
                    y = currstatus,
                    fill = factor(statuslevel, levels = c("1","2","3","4","5")),
                    label = labby)
        ) + 
        geom_col() +
        geom_text(position = position_dodge2(width = 1),
                  aes(hjust =  shifty, color = labby.col)
        ) +
        geom_hline(yintercept = verts, linetype = "longdash" ) +
        coord_flip() +
        ggthemes::theme_hc() +
        ggplot2::theme(plot.title.position = "plot",
                       plot.title = element_markdown(size = 15)) +
        scale_color_manual(guide = "none", values = bw.pal) +
        scale_fill_manual(values = purp.pal,
                          drop = FALSE) +
        labs(title = paste0(tit," by Student Group for ",dist),
             subtitle = subtit,
             x = "",
             y = ""
        )  +
        guides(fill = guide_legend(title = "Dashboard Status \nCell Phone Bars",
                                   title.position = "top",
                                   title.hjust = .5,
                                   label.position = "bottom",
                                   nrow = 1)
        ) +
        theme(legend.key.size = unit(2, 'cm' ))
    
}




indicator.bar(dash.mry, "Carmel", "chronic")

indicator.bar(dash.mry, "North Monterey", "grad")

indicator.bar(dash.mry, "Salinas Union", "susp")


indicator.bar(dash.mry, "Carmel", "math")


indicator.bar(dash.mry, "Santa Rita", "ela")




### Save all -----


indicator.list <- dash.mry$indicator %>% unique()

run.everything <- function(dist) {
    
dash.graph(dash.mry,dist)

ggsave(here("figs", dist, paste0(dist," "," Dashboard Basic chart.png")), width = 8, height = 6)

dash.mry %>%
    mutate(statuslevel = statuslevel.orig) %>%
    dash.graph(dist) +
    labs(subtitle = "Including Student Groups with 11-29 students")

ggsave(here("figs", dist, paste0(dist," "," Dashboard Bonus chart.png")), width = 8, height = 6)

dash.mry %>%
    mutate(statuslevel = statuslevel.orig) %>%
    filter(statuslevel >= 4) %>%
    dash.graph(dist) + 
    labs(title = paste0("Bright Spots on 2022 Dashboard by Student Group for ",dist),
         subtitle = "Includes Student Groups at Status Level 4 or 5")


ggsave(here("figs", dist, paste0(dist," "," Dashboard Bright Spot chart.png")), width = 8, height = 6)

dash.graph.da(dash.mry.da,dist)+ 
    labs(title = paste0("2022 Dashboard Status by Student Group with DA Eligibilty for ",dist))

ggsave(here("figs", dist, paste0(dist," "," Dashboard DA chart.png")), width = 8, height = 6)

for (i in indicator.list) {

indicator.bar(dash.mry, dist, i)
    
    ggsave(here("figs", dist, paste0(dist," ",i, " barchart.png")), width = 8, height = 6)
    
}    

}

run.everything("North Monterey County")

run.everything("Santa Rita")






### Cowplot -----


library(cowplot)

p <- dash.graph(dash.mry,"Santa Rita") +
    guides(fill = "none")

ggdraw() + 
    draw_plot(p) +
    draw_image(
        "legend.png", x = 0, y = 0, hjust = 0, vjust = 0, halign = 0, valign = 0,
        width = 0.15
    )



ggsave(here("figs", "Santa Rita", paste0("Santa Rita"," "," Dashboard cowplot.png")), width = 8, height = 6)
