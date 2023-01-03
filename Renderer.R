



library(here)
library(MCOE)


da.dists <- c(
    # "27102720000000", # MCOE 2019, 2018
    # "27660680000000", # SoMoCo 2019, 2017, 2018
    # "27660920000000", #MPUSD 2019, 2018
    "27661590000000"#, # Salinas Union 2019, 2017, 2018
    # "27661910000000", # Santa Rita 2019
    # "27754400000000", # Soledad 2019, 2018
    # "27660350000000", # GReenfield 2017, 2018
    # "27660500000000", # King City 2017
    # "27661420000000", # Salinas City 2017, 2018
    # "27661670000000", # San Antonio 2018
    # "27754730000000" # Gonzales 2018
    
)

# 
# da.dists <- c("27754400000000")


for(i in da.dists){
    
    dist <- list(dist =  i )
    
    
    dist.name <- mcoe_name(dist)
    
    render("DashboardSummary.qmd", 
           #   output_format = "all", 
           output_file = here("output" , paste0("DashboardSummary", dist.name, ".html" ) ),
           params = dist, 
           envir = new.env(parent = globalenv()) 
    )
    
}
