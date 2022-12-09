
library(janitor)


prelim <- read_sheet(gsheet) %>%
    left_join_codebook("DASH_CENSUS", "studentgroup")


prelim.long <-  prelim %>%
    mutate(across(`4-Achievement : ELA`:`6-Climate Qualified`,as.numeric)) %>%
    pivot_longer(cols = `4-Achievement : ELA`:`6-Climate Qualified`) %>%
    mutate(value = na_if(value,0),
           `DA Status` = replace_na(`DA Status`, "Not"))  %>%
    filter(!str_detect(name,"Qualified"))


prelim.long %>%
    filter(!is.na( value)) %>%
    mutate(value = as.factor(value)) %>%
    ggplot(aes(x = definition, fill = value, y = value)) +
    geom_bar(position="stack", stat = "identity") + 
    facet_wrap(~name) +
    mcoe_theme




####  

da.groups <- prelim.long %>%
    filter(`DA Status` == "DA",
           value == 1) %>%
    select(districtname, studentgroup) %>%
    distinct()


tabyl(da.groups$studentgroup)


prelim.long %>%
    filter(`DA Status` == "DA",
           value == 1) %>%
    tabyl(name)
