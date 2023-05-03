



library(here)
library(MCOE)
library(quarto)
# library(rmarkdown)



da.dists <- dash.mry.da %>% 
    filter(DA.eligible == "DA") %>%
    select(cds) %>%
    unique() %>%
    unlist()



da.dists <- c("27102720000000",
              "27659610000000",
              "27659950000000", 
              "27660350000000",
              "27660500000000",
              "27660680000000",
              "27660920000000",
              "27661420000000",
              "27661590000000",
              "27661670000000", 
              "27661750000000",
              "27661830000000",
              "27661910000000",
              "27662250000000",
              "27738250000000",
              "27754400000000",
              "27754730000000" )


da.dists <- c(
  # "27102720000000", # MCOE 2019, 2018
  # "27660680000000", # SoMoCo 2019, 2017, 2018
  # "27660920000000", #MPUSD 2019, 2018
    "27661590000000" #, # Salinas Union 2019, 2017, 2018
  # "27661910000000", # Santa Rita 2019
  # "27754400000000", # Soledad 2019, 2018
  # "27660350000000", # GReenfield 2017, 2018
  # "27660500000000", # King City 2017
  # "27661420000000", # Salinas City 2017, 2018
  # "27661670000000", # San Antonio 2018
  # "27754730000000", # Gonzales 2018
  #   
  #   "27659610000000"
    
)

# 
# da.dists <- c("27754400000000")


for(i in da.dists){
    
    dist <- list(dist =  i )
    
    
    dist.name <- mcoe_name(dist)
    
    # render("DashboardSummary.qmd",
    #        #   output_format = "all",
    # #       output_dir = "output",
    #        output_file = here("output" ,paste0("DashboardSummary", dist.name, ".html" ) ),
    #        params = dist,
    #        envir = new.env(parent = globalenv())
    # )
    
    quarto_render(#"DashboardSummary.qmd",
                  #   output_format = "all",
     #              output_dir = "output",
    #              execute_dir = "output",
                input = "DashboardSummary.qmd",

                  output_file = paste0("DashboardSummary", dist.name, ".html" ) ,
                  execute_params = dist,
    )
    
   file.rename(from = paste0("DashboardSummary", dist.name, ".html" ),
               to = here("output", paste0("DashboardSummary", dist.name, ".html" ))  )
    
}




### Schools ----

school.list2 <- dash2 %>%
    filter(str_detect(districtname,"Salinas Union"),
           rtype =="S") %>%
    select(cds, schoolname) %>%
    distinct() %>%
    mutate(url = paste0("https://da-monterey.netlify.app/dashboardsummary",cds))

school.list <- dash2 %>%
    filter(str_detect(districtname,"Salinas Union"),
           rtype =="S") %>%
    select(`cds`) %>%
    distinct() %>%
    unlist()

school.list <- school.list[8]

school.list <- c(
 # "27661592730109",
 # "27661592730273",
 "27661590124610",
 "27661596058762"
# "27660682730174",
# "27661592734481"


)


school.list <- c("27659616025993", # Fremont Alisal

 "27754406026678", # Main Street Middle
"27661346026496") # Robert Downs


school.list <- c("27659876026025" # Captain Cooper
                 ) 





school.list <- unique(atsi.cde.mry$cds) 



for(i in school.list){
    
    dist <- list(dist =  i )
    
     quarto_render(
        input = "DashboardSummarySchool.qmd",
        
        output_file = paste0("DashboardSummary", dist, ".html" ) ,
        execute_params = dist,
    )
    
    file.rename(from = paste0("DashboardSummary", dist, ".html" ),
                to = here("output", paste0("DashboardSummary", dist, ".html" ))  )
    
}





####. MPUSD Car Version --------

library(showtext)
library(emojifont)
library(ggtext)

font_add('fa-solid', here('fonts' ,"otfs"  ,"Font Awesome 6 Free-Solid-900.otf"))


school.list <-  atsi.cde.mry %>% 
    filter(str_detect(districtname, "Penin")) %>%
    distinct(cds) %>%
    unlist()




indicator.bar.school.cars <- function(df, dist, schooly, indie) {
        
        
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
        
        
        eil_code <-    school_dir %>%
            filter( str_detect(district,dist),
                    str_detect(school,schooly)
            ) %>%
            select(eil_code)%>% 
            distinct() %>%
            unlist()

        verts <- case_when(indie == "math" & eil_code %in% c("ELEM","INTMIDJR") ~ c(-95,-25,0,35),
                           indie == "math" & eil_code %in% c("HS") ~ c(-115,-60,0,25),
                           indie == "chronic" ~ c(20,10,5,2.5),
                           indie == "grad" ~ c(95,90.5,80,68),
                           indie == "elpi" ~ c(65,55,45,35),
                           indie == "ela" & eil_code %in% c("ELEM","INTMIDJR") ~ c(-70,-5,10,45),
                           indie == "ela" & eil_code %in% c("HS") ~ c(-45,0,30,75),
                           indie == "susp" & eil_code %in% c("ELEM") ~ c(6.0,3.0,1.0,0.5),
                           indie == "susp" & eil_code %in% c("INTMIDJR") ~ c(12.0,8.0,2.0,0.5),
                           indie == "susp" & eil_code %in% c("HS") ~ c(10.0,6.0,1.5,0.5)
        ) 
        
        
        df %>%
            filter(str_detect(districtname,dist),
                   str_detect(schoolname,schooly),
                   indicator == indie,
                   !is.na(currstatus),
                   #               statuslevel != 0,
                   !is.na(studentgroup.long)) %>%
            # mutate(`DA Eligible` = ifelse(DA.eligible =="DA" & statuslevel == 1 & studentgroup != "ALL", "DA", "Not"),
            #        studentgroup.long = ifelse( DA.eligible=="DA" & studentgroup != "ALL",
            #                                    glue("<span style='color:red'>{studentgroup.long}</span>"),
            #                                    glue("<span style='color:black'>{studentgroup.long}</span>") # Used to make the axis labels red for DA groups
            #        )
            # ) %>%
            mutate(shifty = ifelse(currstatus >0, 1, -.05 ) ,
                   labby = case_when(indie == "math" ~ as.character(currstatus),
                                     indie == "ela" ~ as.character(currstatus),
                                     TRUE ~ percent(accuracy = 0.1, x = currstatus/100)),
                   labby.col = ifelse(statuslevel.orig < 4, "white", "black"),
                   studentgroup.long.count = paste0(studentgroup.long," (",currdenom,")")
            ) %>%
            ggplot( aes(x = reorder(studentgroup.long.count, currstatus ),
                        y = currstatus,
                        fill = factor(statuslevel.orig, levels = c("1","2","3","4","5")),
                        label = "<span style='font-family:fa-solid'>&#xf5e4;</span>") #side car
            ) + 
            geom_col() +
            geom_richtext(
                      position = position_dodge2(width = 1),
                      size = 6,
                      fill = NA, label.color = NA, # remove background and outline
                      label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                      aes(hjust =  shifty, color = labby.col)
            ) +
            # geom_richtext(label = "<span style='font-family:fa-solid'>&#xf11e;</span>", # Flag
            #     size = 6,
            #     fill = NA, label.color = NA, # remove background and outline
            #     label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
            #     aes(x = length(currstatus),
            #     y = 0)
            # ) +
            geom_hline(yintercept = verts, linetype = "longdash" ) +
            coord_flip() +
            ggthemes::theme_hc() +
            ggplot2::theme(plot.title.position = "plot",
                           plot.title = element_markdown(size = 15)) +
 #           scale_color_manual(guide = "none", values = bw.pal) +
            scale_fill_manual(values = purp.pal,
                              drop = FALSE) +
            labs(title = paste0(tit," by Student Group<br> for ",schooly),
                 subtitle = subtit,
                 x = "",
                 y = ""
            )  +
            # guides(fill = guide_legend(title = "Dashboard Status \nCell Phone Bars",
            #                            title.position = "top",
            #                            title.hjust = .5,
            #                            label.position = "bottom",
            #                            nrow = 1)
            # ) +
#            theme(legend.key.size = unit(1.5, 'cm' )) +
            theme(axis.text.y = element_markdown())  +  # Used to make the axis labels red for DA groups +
            theme(legend.position="none")
}



indicator.bar.school.cars(df = dash2, dist = "Monterey Peninsula Unified", school =  "La Mesa" , indie =   "ela")



for (i in indicator.list) {
    
    p <- indicator.bar.school(df = dash2, dist = dist.name, school =  school.name , indie =   i)+
        theme(plot.margin = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt"))
    
    print(p)
    
}    


