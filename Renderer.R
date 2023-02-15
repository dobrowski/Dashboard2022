



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



