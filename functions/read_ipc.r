read_ipc <- function(type="INX_A_AVG",country_exclude=c()){
  data <- "data/Eurostat/prc_hicp_aind.tsv"
  options(warn = -1)
  capture.output(
    price <- as.data.frame(read_tsv(data))
    ,file='NUL')
  options(warn = 0)
  #Separate 1st columnn
 price  <- price %>%
    separate(col =1,sep=",",into=unlist(strsplit(colnames(price)[1],","))) %>%
    #rename country column
    dplyr::rename(country="geo\\time") %>% 
    #select data type for PPP_28 = PurchasingPower Parity for Europe of 28 (including UK)
    dplyr::filter(coicop=="CP00") %>% 
    #select aggregate type AO1 = Individual real consumption
    dplyr::filter(unit==type) %>% 
    #Select/exclude countries 
    dplyr::filter(!country %in% country_exclude) %>%
    #Suppress unused columns
    dplyr::select(c(3,7:ncol(price))) 
    #Year columns into numeric
    price[,2:ncol(price)] <- sapply(price[,2:ncol(price)],function(x){
      x <- do.call("rbind",strsplit(x," "))[,1]
      x <- as.numeric(x)
      return(x)})
    #Wide to long
    price <- pivot_longer(price,!country,names_to ="year", values_to ="ipc") %>% as.data.frame
    price$year <- as.numeric(price$year)
  return(price)
}

