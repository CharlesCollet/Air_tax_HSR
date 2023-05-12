source("functions/read_avia_par.R")
source("functions/useful_operators.R")
source("functions/GCD_ICAO.R")


### avia_par database : number of passengers/flights by connection since 1993
setwd("data/Eurostat/avia_par")
files <- list.files(pattern="*.tsv", full.names=TRUE, recursive=FALSE)
PAS_ALL <- read_avia_par_list(files,freq_data="",unit ="PAS",arr_dep =c("DEP","ARR"))
setwd("../../..")
data <- PAS_ALL[,-1:-3]
data$nb_zero <- rowSums(data[,-1:-5] == 0)
data$pas_sum <- rowSums(data[,-1:-5])

data$symetric <- rep(FALSE,nrow(data))
data$sym_index <- rep(NA,nrow(data))
data$index <- seq.int(nrow(data))
data <- data[,c(ncol(data),1:(ncol(data)-1))]
data_dep <- filter(data,arr_dep=="DEP")
data_arr <- filter(data,arr_dep=="ARR")

arr_index <- c()
missing_arr_index <- c()
index_dep <- data_dep$index
for(i in index_dep){
  row_dep <- filter(data_dep,index==i)
  row_arr <- filter(data_arr,orig_airp==row_dep$orig_airp & dest_airp==row_dep$dest_airp)
  if(nrow(row_arr)==1){
    arr_index <- c(arr_index,row_arr$index)
    data_dep[data_dep$index==i,"symetric"] <- TRUE
    data_dep[data_dep$index==i,"sym_index"] <- row_arr["index"]
    row_arr["sym_index"] <- row_dep["index"]
    row_arr["symetric"] <- TRUE
    if(row_dep$nb_zero>=row_arr$nb_zero & row_dep$pas_sum<row_arr$pas_sum){
      data_dep[data_dep$index==i,] <- row_arr
    }
  }else{missing_arr_index <- c(missing_arr_index,i)}
}

#arrival data not used
arr_not_used <- filter(data_arr,index %!in% arr_index)

###### Adding arr data not used ####
data_dep <- rbind(data_dep,arr_not_used)

#filter for symetric flows
symetric_flows <- filter(data_dep,symetric==TRUE)

######### Manage 0 with symetric flows ####
# Get data from symetric flow if 0
for(i in 1:nrow(symetric_flows)){
  row_corres <- filter(data,index==symetric_flows[i,"sym_index"])
  for(j in 7:33){
    if(symetric_flows[i,j]==0){
      if(row_corres[j]!=0){
        symetric_flows[i,j] <- row_corres[j]
        }else{
        symetric_flows[i,j] <- NA
      }
    }  
  }
}

#Reintegrating symmetric flows
data_dep[data_dep$symetric==TRUE,] <- symetric_flows
data_dep_numbers <- data_dep[,7:33]
data_dep_numbers[as.data.frame(isZero(data_dep[,7:33]))==TRUE] <- NA
data_dep[,7:33] <-  data_dep_numbers

#suppression of same origin and destination rows
data_dep <- filter(data_dep,orig_airp!=dest_airp,arr_not_used,weird_data)
#Connection by alphabetical value
data_dep$connexion <- ifelse(paste(data_dep$orig_airp,data_dep$dest_airp,sep = "_")< paste(data_dep$dest_airp,data_dep$orig_airp,sep = "_"),
                             paste(data_dep$orig_airp,data_dep$dest_airp,sep = "_"),
                             paste(data_dep$dest_airp,data_dep$orig_airp,sep = "_"))
test <- data.frame(table(data_dep$connexion))
test2 <- test[test$Freq!=2,][1]$Var1
test2 <- as.character(test2)
no_bilat<- data_dep[data_dep$connexion %in% test2,]
data_dep <- data_dep[data_dep$connexion %!in% test2,]

#Remove intermediary variables
rm(test,test2,no_bilat,symetric_flows,data_dep_numbers,row_corres,arr_index,missing_arr_index,index_dep)

#Suppress connections with too many zeros: we define the maximum number of NA as equal to 3.
max_zero_2001_2019=3
data_kept <- data_dep[apply(data_dep,MARGIN=1,FUN=function(x){sum(is.na(x[7:25]))<=max_zero_2001_2019}),]

#We remove :
#   SK (Slovakia), HR (Croatia), RO (Romania), LT (Lithuania), PL (Poland), BG(Bulgaria)
#  IS (Iceland),
# Special case : CZ (Czech Republic) also removed because wrong ICAO codes are communicated (end with xx99)
remove_countries <- c("SK","HR","RO","LT","PL","BG","IS","CZ")
data_dep <- filter(data_dep,orig_count %!in% remove_countries & dest_count %!in% remove_countries)

#Group by NUTS2
max_zero_2001_2019=3
data_kept <- data_dep[apply(data_dep,MARGIN=1,FUN=function(x){sum(is.na(x[7:25]))<=max_zero_2001_2019}),]
tabflow_all <- flowtype(data_kept[,c("orig_airp","dest_airp","2019")], format="L", origin="orig_airp", destination="dest_airp", fij="2019",  x= "alltypes" )

#Adding airports
data_kept <- dplyr::rename(data_kept,o_ICAO=orig_airp,d_ICAO=dest_airp)

load("data/airports.Rdata")
airports[airports$city=="Ã„ngelholm","city"] <- "Angelholm"
airports_orig <- airports %>%
  dplyr::rename(o_ICAO=ICAO,o_city=city,o_airp=name,o_lat=latitude,o_long=longitude) %>% 
  dplyr::select(o_ICAO,o_city,o_airp,o_lat,o_long)
data_kept <- dplyr::left_join(data_kept,airports_orig,by="o_ICAO")
airports_dest <- airports %>%
  dplyr::rename(d_ICAO=ICAO,d_city=city,d_airp=name,d_lat=latitude,d_long=longitude) %>% 
  dplyr::select(d_ICAO,d_city,d_airp,d_lat,d_long)
data_kept <- left_join(data_kept,airports_dest,by="d_ICAO")
#airports not found
other_airports <- data_frame(ICAO=levels(factor(data_kept[data_kept$o_ICAO %!in% airports$ICAO,"o_ICAO"])))
other_airports <- data_frame(ICAO=levels(factor(data_kept[data_kept$d_ICAO %!in% airports$ICAO,"d_ICAO"])))
#We remove airport EKGF, Tyra Ost, Denmark because no localisation
data_kept <- filter(data_kept,o_ICAO!="EKGF" & d_ICAO!="EKGF")


####### Temporary -> we remove all airports not found ######
data_kept <- filter(data_kept,o_ICAO %in% airports$ICAO & d_ICAO %in% airports$ICAO)

df_final <- data_kept
df_final$dist <- rep(NA,nrow(df_final))

for(i in 1:nrow(df_final)){
  df_final[i,"dist"] <- GCD_ICAO_coord(df_final[i,c("o_long","o_lat","d_long","d_lat")])
}

####### Aggregate long data - Source Eurostats#######
df_final <-  df_final[,c(2:6,38:47,7:33)] %>%
  gather("year","passengers",-1:-15)


SPA <- read.xlsx("data/Eurostat/city_NUTS.xlsx",1)
colnames(SPA)[3:22]<- 2019:2000
SPA <- gather(SPA,"year","SPA", -1:-2)
POP <- read.xlsx("data/Eurostat/city_NUTS.xlsx",2)
colnames(POP)[3:22]<- 2019:2000
POP <- gather(POP,"year","POP",  -1:-2)
SPA <- inner_join(SPA[,-2],POP[,-2],by=c("city","year"))
df_final <-  left_join(
  filter(df_final,o_city %in% SPA$city),
  rename(SPA,o_city=city,o_SPA=SPA,o_POP=POP),by=c("o_city","year"))
df_final <-  left_join(
  filter(df_final,d_city %in% SPA$city),
  rename(SPA,d_city=city,d_SPA=SPA,d_POP=POP),by=c("d_city","year"))
df_final[,18:21] <- apply(df_final[,18:21],2,as.numeric)

df_final$lag_pas <- NA
df_final$lag_o_POP <- NA
df_final$lag_d_POP <- NA
df_final$lag_o_SPA <- NA
df_final$lag_d_SPA <- NA
for(i in 2000:2018){
  df_final[df_final$year==i+1,"lag_pas"]  <- df_final[df_final$year==i,"passengers"]
  df_final[df_final$year==i+1,"lag_o_POP"]  <- df_final[df_final$year==i,"o_POP"]
  df_final[df_final$year==i+1,"lag_d_POP"]  <- df_final[df_final$year==i,"d_POP"]
  df_final[df_final$year==i+1,"lag_o_SPA"]  <- df_final[df_final$year==i,"o_SPA"]
  df_final[df_final$year==i+1,"lag_d_SPA"]  <- df_final[df_final$year==i,"d_SPA"]
}
df_final$diff_log_pas <- log(df_final$passengers) - log(df_final$lag_pas)

#### Real price indices per transport mode (railway + car). air included #####
transport_index <- read.csv("data/EEA/real-price-indices-of-passenger-6.csv")
transport_index <- transport_index %>% mutate(orig_count=recode(ugeo.text,
                                                               "Austria"="AT","Belgium"="BE","Switzerland"="CH", "Cyprus"="CY",
                                                               "Germany (until 1990 former territory of the FRG)"="DE", "Denmark"="DK",
                                                               "Spain"="ES","Finland"="FI","France"="FR","Hungary"="HU","Ireland"="IE","Italy"="IT",
                                                               "Malta"="MT","Netherlands"="NL","Norway"="NO","Portugal"="PT","Sweden"="SE","Slovenia"="SI","United Kingdom"="UK"))

transport_index <-  transport_index %>%
  rename(year="date.number",index_type="coicop_label.text") %>% 
  select(year,orig_count,index_type,value.number) %>% 
  pivot_wider(names_from =index_type,values_from=value.number)
transport_index$dest_count <- transport_index$orig_count

transport_index$ind_road_o <- transport_index$`Passenger transport by road`
transport_index$ind_road_d <- transport_index$`Passenger transport by road`
transport_index$ind_rail_o <- transport_index$`Passenger transport by railway`
transport_index$ind_rail_d <- transport_index$`Passenger transport by railway`
transport_index$ind_cars_o <- transport_index$`Passenger cars`
transport_index$ind_cars_d <- transport_index$`Passenger cars`
transport_index$ind_air_o <- transport_index$`Passenger transport by air`
transport_index$ind_air_d <- transport_index$`Passenger transport by air`


transport_index$year <- as.character(transport_index$year)
df_final <- left_join(df_final,dplyr::select(transport_index,year,orig_count,ind_road_o,ind_rail_o,ind_cars_o,ind_air_o),by=c("year","orig_count"))
df_final <- left_join(df_final,dplyr::select(transport_index,year,dest_count,ind_road_d,ind_rail_d,ind_cars_d,ind_air_d),by=c("year","dest_count"))
df_final$year <- as.numeric(df_final$year)


#distance
df_final$dist_dummy <- rep(0,nrow(df_final))
df_final$dist_dummy <- ifelse(df_final$dist>1000,1,0)


df_final$connexion_city <- ifelse(paste(df_final$o_city, df_final$d_city,sep = "_")< paste(df_final$d_city, df_final$o_city, sep = "_"),
                                  paste(df_final$o_city, df_final$d_city, sep = "_"),
                                  paste(df_final$d_city, df_final$o_city,sep = "_"))
df_final <- filter(df_final,year>2000)

# Milan_pb <- filter(df_final,o_city %in% c("Milan","Milano") | d_city %in% c("Milan","Milano"))
df_final[df_final$o_city=="Milano", "o_city"] <- "Milan"
df_final[df_final$d_city=="Milano", "d_city"] <- "Milan"
df_final$connexion_city <-  ifelse(df_final$o_city<df_final$d_city,
                                   paste(df_final$o_city,df_final$d_city,sep="_"),
                                   paste(df_final$d_city,df_final$o_city,sep="_"))


# Remove Malta routes because of a specific tax with unknown amount between 2001 and 2008,
# Small impact because of the low number of routes affected
df_final  <- filter(df_final,dest_count!="MT" & orig_count !="MT")


#We correct an especially weird asymetry supposing bilateral flow
df_final[df_final$o_ICAO=="LFPG" & df_final$d_ICAO=="LIML"& df_final$year %in% 2001:2007,"passengers"] <- df_final[df_final$d_ICAO=="LFPG" & df_final$o_ICAO=="LIML"& df_final$year %in% 2001:2007,"passengers"]

#We remove all NA passengers
df_final$id <- paste(df_final$connexion,df_final$year,sep = "_")
df_final_no_na <- filter(df_final,!is.na(passengers))
n_occur <- data.frame(table(df_final_no_na$id)) %>% filter(Freq!=2)
df_final <-  filter(df_final_no_na,!(id %in% n_occur$Var1))

save(df_final,file="workspace_save/df_final.Rdata")
