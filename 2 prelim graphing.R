
library(RColorBrewer)
library(ggtext)
library(glue)

prelim <- read_sheet(gsheet) %>%
    left_join_codebook("DASH_CENSUS", "studentgroup")



purp.pal <- c( "5" ='#dadaeb',"4" ='#bcbddc', "3" ='#807dba',"2" ='#6a51a3',"1" ='#3f007d', "NA" = "#FFFFFF")
da.pal <- c("DA" = "Red", "Not"= "Black")
bw.pal <- c("white" = "white", "black" = "black")




DA.graph.prelim <- function(dist) {
    

speck <- prelim %>%
    filter(str_detect(districtname,dist)) %>%
    mutate(across(`4-Achievement : ELA`:`6-Climate Qualified`,as.numeric)) %>%
    pivot_longer(cols = `4-Achievement : ELA`:`6-Climate Qualified`) %>%
    mutate(value = na_if(value,0),
           `DA Status` = replace_na(`DA Status`, "Not"))  %>%
    filter(!str_detect(name,"Qualified")) %>%
    na.exclude() %>%
    mutate(name = str_replace(name, ":","\n"), # To make the labels wrap to be shorter
        definition = str_replace(definition, "D","<br>D"),
           definition = str_replace(definition, "/","/<br>"),
           `DA Eligible` = ifelse(`DA Status`=="DA" & value == 1, "DA", "Not"),
           definition = ifelse( `DA Status`=="DA",
                               glue("<span style='color:red'>{definition}</span>"),
                               glue("<span style='color:black'>{definition}</span>") # Used to make the axis labels red for DA groups
           )
           )




ggplot(speck) +
    geom_tile(aes(y = definition,  # Student group
                  x = name,  # Indicator
                  fill = as.factor(value),   # Status rating
                  color = as.factor(`DA Eligible`), 
                  width=0.95, # width and heigth are adjusted to allow the color borders to go fully around
                  height=0.95
                  ),
                  lwd = .75,
                  ) +
    mcoe_theme +
    # scale_fill_brewer(palette = "Purples",
    #                   na.value = "White",
    #                   direction = -1) +
    scale_fill_manual(values = purp.pal) +
    scale_color_manual(values = da.pal) +
    labs(title = paste0("2022 Dashboard Status by Student Group for ",dist),
         ) +
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

ggsave(here("figs",glue("{dist} Dashboard Status 2022 - {Sys.Date()}.png")),
       width = 8, height = 8)

}


DA.graph.prelim("Spreckels")


DA.graph.prelim("South Monterey County")

DA.graph.prelim("King City")

DA.graph.prelim("Monterey Peninsula")


### Graph Everyone ------

distros <- prelim$districtname %>%   # Go through every district
    unique()

for (i in distros) {
    DA.graph.prelim(i)
}


### End ------