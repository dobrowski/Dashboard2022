

fake <- tribble(
    ~column, ~height,
    1,1,
    2,2,
    3,3,
    4,4,
    5,5
    
)


ggplot(fake, aes(x = column,
                 y = height,
                 label = height,
                 fill = factor(height, levels = c("1","2","3","4","5"))
                 )
       ) + 
    geom_col(color = "black") +
    geom_text(color = "black",
               size = 5,
              vjust= -.1
               ) +
    scale_fill_manual(values = purp.pal) +
    theme_void()+
    ylim(c(0,6)) +
    guides(
           fill = "none"
    ) +
    labs(x = "", y = "")


ggsave("legend.png", width = 1, height = 1)                      
