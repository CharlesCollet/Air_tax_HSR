'%!in%' <- function(x,y)!('%in%'(x,y))

weighted_mean <- function(x, w, ..., na.rm = FALSE){
  if(na.rm){
    df_omit <- na.omit(data.frame(x, w))
    return(weighted.mean(df_omit$x, df_omit$w, ...))
  } 
  weighted.mean(x, w, ...)
}
