source("functions/load_package.R")
source("functions/event_tax.R")
source("functions/event_rail.R")
source("functions/read_price.R")
source("functions/read_ipc.R")
source("functions/cor_ipc_ipp.R")

#Call database creation
# source("database.R")

#When already created, load existing database
load("workspace_save/df_final.Rdata")

###### Effects of LGV openings #####
# Tax effects : True effect only on origin
df_final$t_UK <-  rep(0,nrow(df_final))
df_final[with(df_final,(orig_count=="UK" ) & year == 2007),"t_UK"] <- 5*11/12
df_final[with(df_final,(orig_count=="UK" ) & year == 2008),"t_UK"] <- 5
df_final[with(df_final,(orig_count=="UK" ) & year == 2009),"t_UK"] <- (5*10/12 + 6*2/12)
df_final[with(df_final,(orig_count=="UK" ) & year == 2010),"t_UK"] <- (6*10/12 + 7*2/12)
df_final[with(df_final,(orig_count=="UK" ) & year == 2011),"t_UK"] <- 7
df_final[with(df_final,(orig_count=="UK" ) & year == 2012),"t_UK"] <- (7*3/12 + 8*9/12)
df_final[with(df_final,(orig_count=="UK" ) & year >= 2012),"t_UK"] <- 8
df_final$t_Dublin <-  rep(0,nrow(df_final))
df_final[with(df_final,(o_city=="Dublin" ) & year %in% 2009:2010),"t_Dublin"] <- 10
df_final[with(df_final,(o_city=="Dublin" ) & year == 2011),"t_Dublin"] <- 10*2/12+3*10/12
df_final[with(df_final,(o_city=="Dublin" ) & year %in% 2012:2013),"t_Dublin"] <- 3
df_final[with(df_final,(o_city=="Dublin" ) & year == 2014),"t_Dublin"] <- 3*3/12
df_final$t_NL <-  rep(0,nrow(df_final))
df_final[with(df_final,(orig_count=="NL" ) & year %in% 2008:2009),"t_NL"] <- 11.25/2
df_final$t_DE <-  rep(0,nrow(df_final))
df_final[with(df_final,(orig_count=="DE" ) & year == 2011),"t_DE"] <- 7.5*9/12
df_final[with(df_final,(orig_count=="DE" ) & year > 2011),"t_DE"] <- 7.5
df_final$t_AT <-  rep(0,nrow(df_final))
df_final[with(df_final,(orig_count=="AT") & year >= 2011),"t_AT"] <- 7.5
df_final[with(df_final,(orig_count=="AT") & year >= 2018),"t_AT"] <- 3.5
df_final$t_FR <-  rep(0,nrow(df_final))
df_final[with(df_final,(orig_count=="FR") & year == 2006),"t_FR"] <- 1*7/12
df_final[with(df_final,(orig_count=="FR") & year > 2006),"t_FR"] <- 1
df_final[with(df_final,(orig_count=="FR") & year >= 2014),"t_FR"] <- 1.127*df_final[with(df_final,(orig_count=="FR") & year >= 2014),"t_FR"] 
df_final$t_NO <-  rep(0,nrow(df_final))
df_final[with(df_final,(orig_count=="NO") & year == 2016),"t_NO"]  <- 82*7/12
df_final[with(df_final,(orig_count=="NO") & year == 2017),"t_NO"]  <- 82
df_final[with(df_final,(orig_count=="NO") & year == 2018),"t_NO"]  <- 83
df_final[with(df_final,(orig_count=="NO") & year == 2019),"t_NO"]  <- 84*3/12 + 75*9/12

# Correction of the tax wih PPP (Purchasing Power Parity) and Consumer Price Index (CPI)
ppp <- read_price() %>% filter(orig_count %in% names(table(df_final$orig_count)) , year >=2001)
ipc <- read_ipc() %>% filter(country %in% names(table(df_final$orig_count)) & year >=2001)

df_final <- cor_ipc_ipp("UK","t_UK",df=df_final)
df_final <- cor_ipc_ipp("IE","t_Dublin",df=df_final)
df_final <- cor_ipc_ipp("DE","t_DE",df=df_final)
df_final <- cor_ipc_ipp("NL","t_NL",df=df_final)
df_final <- cor_ipc_ipp("AT","t_AT",df=df_final)
df_final <- cor_ipc_ipp("FR","t_FR",df=df_final)
df_final <- cor_ipc_ipp("NO","t_NO",df=df_final)

#Global tax estimation
df_final$t_global <- with(df_final,t_AT + t_DE + t_Dublin + t_FR + t_NL + t_UK + t_NO)

#Rail length, prorata temporis according to the date of implementation
new_rail_apply <- function(rail,data,HSR_line,length,year,share_1st_year){
  data[data$connexion_city %in% HSR_line & data$year > year,rail] <- data[data$connexion_city %in% HSR_line & data$year > year,rail] + length
  data[data$connexion_city %in% HSR_line & data$year == year,rail] <- data[data$connexion_city %in% HSR_line & data$year ==year,rail] + length*share_1st_year
  return(data)
}
df_final$new_rail <- 0
df_final <- new_rail_apply("new_rail",df_final,"Paris_Strasbourg",301,2007,7/12)
df_final <- new_rail_apply("new_rail",df_final,"Paris_Rennes",214,2017,6/12)
df_final <- new_rail_apply("new_rail",df_final,"Bordeaux_Paris",302,2017,6/12)
df_final <- new_rail_apply("new_rail",df_final,"Barcelona_Madrid",621,2007,10/12)
df_final <- new_rail_apply("new_rail",df_final,"Madrid_Valencia",391,2011,1)
df_final <- new_rail_apply("new_rail",df_final,"Lyon_Strasbourg",138,2011,1/12)
df_final <- new_rail_apply("new_rail",df_final,"Madrid_Malaga",156,2008,1)
df_final <- new_rail_apply("new_rail",df_final,"Madrid_Santiago",87,2011,1/12)
df_final <- new_rail_apply("new_rail",df_final,"Milan_Rome",215,2008,3/12)
df_final <- new_rail_apply("new_rail",df_final,"Naples_Rome",193,2006,1)
df_final <- new_rail_apply("new_rail",df_final,"Munich_Nuernberg",90,2006,8/12)
df_final <- new_rail_apply("new_rail",df_final,"Munich_Stuttgart",61,2011,1/12)
df_final <- new_rail_apply("new_rail",df_final,"Bologna_Rome",78,2009,1/12)

#Transformning data into bilateral flow
data_city_bilateral <-  df_final %>% 
  dplyr::mutate("SPA_bilat" =o_SPA*d_SPA,"POP_bilat"=o_POP*d_POP,"ind_rail_bilat"=(ind_rail_o+ind_rail_d)/2) %>%
  group_by(connexion_city,year) %>%
  #For the tax we multiply by 2 the mean to get the right amount
  #Ex: if on route od there is a tax t on city o and no tax on city d, mean(tax)=t/2
  #We multtiply it by 2 to get the effect t of a 1-LEG tax on the bilateral flow  
  #For domestic routes, we would get mean(tax)=t
  #Then the effect of the tax is calculated twice: from o to d and from d to o, it is normal to have the 2t tax amount
  dplyr::summarise(passengers=sum(passengers),SPA_bilat=mean(SPA_bilat),new_rail=max(new_rail),POP_bilat=mean(POP_bilat),ind_rail_bilat=mean(ind_rail_bilat),
                   t_global=2*mean(t_global),t_UK=2*mean(t_UK),t_Dublin=2*mean(t_Dublin),t_NL=2*mean(t_NL),t_AT=2*mean(t_AT),t_FR=2*mean(t_FR),t_DE=2*mean(t_DE),t_NO=2*mean(t_NO),
                   dist=mean(dist),city_1=min(o_city,d_city),city_2=max(o_city,d_city),country_1=min(orig_count,dest_count),country_2=max(orig_count,dest_count)
                   #,intra_count=mean(intra_count)
                   )
data_city_bilateral$id <- paste(data_city_bilateral$connexion_city,data_city_bilateral$year,sep = "_")
# data_city_bilateral$tax_treatement <- 1
# data_city_bilateral[data_city_bilateral$orig_count %in% list$orig_count | df$dest_count %in% list$orig_count,"treated"] <- 1

#Creation of short, medium and long haul tax effects                   
data_city_bilateral$tax_0_500 <- ifelse(data_city_bilateral$dist<500,data_city_bilateral$t_global,0)
data_city_bilateral$tax_500_1000 <- ifelse(data_city_bilateral$dist>=500 &data_city_bilateral$dist < 1000,data_city_bilateral$t_global,0)
data_city_bilateral$tax_1000_inf <- ifelse(data_city_bilateral$dist >= 1000,data_city_bilateral$t_global,0)


##### Final table ###############
ols_1 = feols(log(passengers) ~ t_AT + t_DE + t_Dublin + t_FR +t_NL + t_NO + t_UK + log(SPA_bilat)|connexion_city+ year,cluster = ~ connexion_city,data = data_city_bilateral )
ols_2 = feols(log(passengers) ~ t_global + log(SPA_bilat)|connexion_city+ year,cluster = ~ connexion_city,data = data_city_bilateral)
ols_3 = feols(log(passengers) ~ t_AT + t_DE + t_Dublin + t_FR +t_NL + t_NO + t_UK + log(SPA_bilat)+new_rail|connexion_city+ year,cluster = ~ connexion_city,data = data_city_bilateral )
ols_4 = feols(log(passengers) ~ t_global +log(SPA_bilat)+new_rail|connexion_city+ year,cluster = ~ connexion_city,data = data_city_bilateral )
ols_7 = feols(log(passengers) ~ log(SPA_bilat)+tax_0_500+tax_500_1000+tax_1000_inf|connexion_city+ year,cluster = ~ connexion_city,data = data_city_bilateral )
ols_8 = feols(log(passengers) ~ log(SPA_bilat)+tax_0_500+tax_500_1000+tax_1000_inf+new_rail |connexion_city+ year,cluster = ~ connexion_city,data = data_city_bilateral )
etable(ols_1,ols_2,ols_3,ols_4,ols_7,ols_8,vcov = "twoway",fitstat=c("n","ll","r2","aic"))
etable(ols_1,ols_2,ols_3,ols_4,ols_7,ols_8,vcov = "twoway",fitstat=c("n","ll","r2","aic"),tex = TRUE)

####### Robustness checks #######
# Reference model
ols_1 = feols(log(passengers) ~ t_global +log(SPA_bilat)+new_rail|connexion_city+ year,cluster = ~ connexion_city,data = data_city_bilateral )
ols_2 = feols(log(passengers) ~ t_global + new_rail+log(SPA_bilat)+log(POP_bilat)|connexion_city+ year,cluster = ~ connexion_city,data = data_city_bilateral )
ols_3 = feols(log(passengers) ~ t_global + new_rail+log(SPA_bilat)+ind_rail_bilat|connexion_city+ year,cluster = ~ connexion_city,data = data_city_bilateral )
ols_4= feols(log(passengers) ~ t_global + new_rail+log(SPA_bilat)+ind_rail_bilat|connexion_city+ year,cluster = ~ connexion_city,data = data_city_bilateral )
etable(ols_1,ols_2,ols_3,ols_4,fitstat=c("n","ll","r2","pr2","aic","bic"))
etable(ols_1,ols_2,ols_3,ols_4,vcov = "twoway",fitstat=c("n","ll","r2","aic"),tex = TRUE)

### Event study on tax

#With AT,DE,NO & UK
list <- data.frame(
  country_1=c("AT","DE","NO","UK"),
  year_treated=c(2011,2011,2016,2007))
list2 <- c()
time_window_event_tax(data_city_bilateral,list)
event_tax_l(data_city_bilateral,list,list2,min_year = -5,max_year = 8,
            legend = "Event study: Staggered treatment (TWFE) on AT, DE, NO and UK air taxes")

#Without NO tax to get more robust post treatment observations from t=3
list <- data.frame(
  country_1=c("AT","DE","UK"),
  year_treated=c(2011,2011,2007))
list2 <- c("NO")
time_window_event_tax(data_city_bilateral,list)
event_tax_l(data_city_bilateral,list,list2,min_year = -5,max_year = 8,
            legend = "Event study: Staggered treatment (TWFE) on AT, DE and UK air taxes")

### Event study on HSR
list <- data.frame(
  connexion_city=c("Munich_Nuernberg","Munich_Stuttgart","Bologna_Rome","Paris_Strasbourg","Naples_Rome","Barcelona_Madrid","Madrid_Valencia","Lyon_Strasbourg","Madrid_Malaga","Madrid_Santiago","Milan_Rome", "Naples_Rome"),
  year_treated=c(2006,2012,2010,2007,2006,2008,2011,2011,2008,2012,2008,2006))
time_window_event(df_final,list)

event_rail_l(data_city_bilateral,list,min_year = -5,max_year =7,legend='Event study: Staggered treatment (TWFE) on HSR openings >50km' )

############## OVerall effect of the tax and HSR#############

#For taxes
sum(data_city_bilateral$passengers*exp(-0.01*data_city_bilateral$t_global),na.rm = TRUE)- sum(data_city_bilateral$passengers*data_city_bilateral$dist,na.rm = TRUE)
(sum(data_city_bilateral$passengers*exp(-0.01*data_city_bilateral$t_global),na.rm = TRUE)- sum(data_city_bilateral$passengers,na.rm = TRUE))/sum(data_city_bilateral$passengers,na.rm = TRUE)
(sum(data_city_bilateral$passengers*exp(-0.01*data_city_bilateral$t_global)*data_city_bilateral$dist,na.rm = TRUE)- sum(data_city_bilateral$passengers*data_city_bilateral$dist,na.rm = TRUE))/sum(data_city_bilateral$passengers*data_city_bilateral$dist,na.rm = TRUE)

(sum(data_city_bilateral$passengers*exp(-0.01*data_city_bilateral$t_global)*data_city_bilateral$dist,na.rm = TRUE)- sum(data_city_bilateral$passengers*data_city_bilateral$dist,na.rm = TRUE))*0.115

#For HSR
sum(data_city_bilateral$passengers*exp(-0.0024*data_city_bilateral$new_rail),na.rm = TRUE)- sum(data_city_bilateral$passengers,na.rm = TRUE)
(sum(data_city_bilateral$passengers*exp(-0.0024*data_city_bilateral$new_rail),na.rm = TRUE)- sum(data_city_bilateral$passengers,na.rm = TRUE))/sum(data_city_bilateral$passengers,na.rm = TRUE)
(sum(data_city_bilateral$passengers*exp(-0.0024*data_city_bilateral$new_rail)*data_city_bilateral$dist,na.rm = TRUE)- sum(data_city_bilateral$passengers*data_city_bilateral$dist,na.rm = TRUE))/sum(data_city_bilateral$passengers*data_city_bilateral$dist,na.rm = TRUE)

(sum(data_city_bilateral$passengers*exp(-0.0024*data_city_bilateral$new_rail)*data_city_bilateral$dist,na.rm = TRUE)- sum(data_city_bilateral$passengers*data_city_bilateral$dist,na.rm = TRUE))*0.115



