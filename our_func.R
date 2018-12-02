FactorToNum <- function(df){
  df[] <- lapply(df, function(x) {
    if(is.factor(x)) as.numeric(x) else x
  })
  return(df)
}


highlight <- high_ROA_limit[high_ROA_limit$ROA > 300 | high_ROA_limit$`Total Volume of Bookings` > 190,]

HightlightGreyoutScatterPlot <- function(main.df, highlight.df, x.col.name, y.col.name, highlight.color, title, xlab, ylab, labs.color){
  greyout.df <- setdiff(main.df, highlight.df)
  
  ggplot() +
    geom_point(data = greyout.df,
               mapping = aes(greyout.df[[x.col.name]], greyout.df[[y.col.name]]),
               color = "grey", size = 4) +
    geom_point(data = highlight.df, 
               mapping = aes(highlight.df[[x.col.name]], highlight.df[[y.col.name]], color = highlight.color), size = 4) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(color = "black")) +
    ggtitle(title) +
    xlab(xlab) +
    ylab(ylab) +
    labs(color = labs.color)
}

HightlightGreyoutScatterPlot(main.df = high_ROA_limit, 
                             highlight.df = highlight, 
                             x.col.name = 'Total Volume of Bookings',
                             y.col.name = 'ROA',
                             highlight.color = 'Publisher Name',
                             title = 'Publisher Success',
                             xlab = 'Total Volume of Bookings',
                             ylab = 'ROA(%)',
                             labs.color = 'Publisher Name')

TestFunc <- function(df, col.name){
  df[[col.name]]
}
library(sqldf)
sqldf('SELECT `Match Type`, AVG(`Clicks`), AVG(`Amount`), AVG(`Total Volume of Bookings`)
    FROM data
    GROUP BY `Match Type`
    ;')
