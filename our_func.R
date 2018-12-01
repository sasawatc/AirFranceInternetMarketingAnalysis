factor_to_num <- function(df){
  df[] <- lapply(df, function(x) {
    if(is.factor(x)) as.numeric(x) else x
  })
  return(df)
}
