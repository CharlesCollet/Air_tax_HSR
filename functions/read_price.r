read_price  <- function(aggregate="A01",country_exclude=c()){
  data <- "data/Eurostat/prc_ppp_ind.tsv"
  options(warn = -1)
  capture.output(
    price <- as.data.frame(read_tsv(data))
    ,file='NUL')
  options(warn = 0)
  #Separate 1st columnn
 price  <- price %>%
    separate(col =1,sep=",",into=unlist(strsplit(colnames(price)[1],","))) %>%
    #rename country column
    dplyr::rename(orig_count="geo\\time") %>% 
    #select data type for PPP_28 = PurchasingPower Parity for Europe of 28 (including UK)
    dplyr::filter(na_item=="PPP_EU28") %>% 
    #select aggregate type AO1 = Individual real consumption
    dplyr::filter(ppp_cat==aggregate) %>% 
    #Select/exclude countries 
    dplyr::filter(!orig_count %in% country_exclude) %>%
    #Suppress unused columns
    dplyr::select(c(3,7:ncol(price))) 
    #Year columns into numeric
    price[,-1] <- sapply(price[,-1],as.numeric)
    #Wide to long
    price <- pivot_longer(price,!orig_count,names_to ="year", values_to ="price_lvl") %>% as.data.frame
    price$year <- as.numeric(price$year)
  return(price)
}
