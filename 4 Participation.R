

#  Looks at participation rates 



elpi.partic <- dash.mry.da %>%
    filter(! is.na( Flag95Pct ),
           nSizeMet == "Y")


acad.partic <- dash.mry.da %>%
    filter(! is.na( numPRLOSS ) ,
           prate_enrolled >= 30) %>%
    select(districtname, studentgroup, definition, currstatus, currstatus_withoutPRLOSS, statuslevel, indicator)

