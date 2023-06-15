cor_ipc_ipp <- function(orig_country,tax,df=df_final,period=2001:2019,year_ref=2019){
    # tax="t_UK"
    # country="UK"
    # i=1
    cor  <- rep(0,length(period))
    for(i in 1:length(period)){
    cor[i] <- 1/ppp[ppp$orig_count==orig_country & ppp$year=="2015","price_lvl"]*ipc[ipc$country==orig_country & ipc$year==year_ref,"ipc"]/ipc[ipc$country==orig_country & ipc$year==as.character(period)[i],"ipc"]
    df[df$orig_count==orig_country & df$year == period[i],tax] <- df[df$orig_count==orig_country & df$year == as.character(period)[i],tax]*cor[i]
        }
    return(df)

}

cor_ipc_ipp_bilat <- function(orig_country,tax,df=df_final,period=2001:2019,year_ref=2019){
    # tax="t_UK"
    # country="UK"
    # i=1
    cor  <- rep(0,length(period))
    for(i in 1:length(period)){
    cor[i] <- 1/ppp[ppp$orig_count==orig_country & ppp$year=="2015","price_lvl"]*ipc[ipc$country==orig_country & ipc$year==year_ref,"ipc"]/ipc[ipc$country==orig_country & ipc$year==as.character(period)[i],"ipc"]
    df[(df$orig_count==orig_country | df$dest_count==orig_country) & df$year == period[i],tax] <- df[(df$orig_count==orig_country | df$dest_count==orig_country) & df$year == as.character(period)[i],tax]*cor[i]
        }
    return(df)
}
