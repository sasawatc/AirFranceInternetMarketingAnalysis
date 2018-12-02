FactorToNUm <- function(df){
  df[] <- lapply(df, function(x) {
    if(is.factor(x)) as.numeric(x) else x
  })
  return(df)
}


highlight <- high_ROA_limit[high_ROA_limit$ROA > 300 | high_ROA_limit$`Total Volume of Bookings` > 190,]
greyout <- high_ROA_limit[high_ROA_limit$ROA <= 300 & high_ROA_limit$`Total Volume of Bookings` <= 190,]

HightlightGreyout <- function(highlight.df, highlight.col.name, greyout.col.name, x.col.name, y.col.name, highlight.color, title, xlab, ylab, labs.color){
  greyout.df <- setdiff(high_ROA_limit, highlight)
  ggplot() +
    geom_point(data=greyout.df,
               mapping = aes(greyout.df$`Total Volume of Bookings`, greyout.df$ROA), color = "grey", size = 4) +
    geom_point(highlight.df, 
               mapping = aes(highlight.df$`Total Volume of Bookings`, highlight.df$ROA, color =`Publisher Name`), size =4) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(color = "black")) +
    ggtitle("Publisher Success") +
    xlab("Total Volume of Bookings") +
    ylab("ROA(%)") +
    labs(color = "Publisher Name")
}