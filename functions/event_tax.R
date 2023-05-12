event_tax <- function(df,connection,year_rail,legend_place="topright"){
  df$treated <- 0
  df[df$connexion_city==connection,"treated"] <- 1
  df$year_treated <- rep(10000,nrow(df))
  df[df$connexion_city==connection,"year_treated"] <- year_rail
  df$time_to_treatment <- -1000
  df[df$treated==1,"time_to_treatment"] <- df[df$treated==1,"year"]-df[df$treated==1,"year_treated"]
  table(df$time_to_treatment)
  table(df$treated)
  
  
  mod_twfe = feols(log(passengers) ~ i(time_to_treawtment,ref=c(-1,-1000)) ## Our key interaction: time � treatment status
                   # + log(o_POP*d_POP)+
                   +log(o_SPA*d_SPA)
                   # +ind_rail_o*ind_rail_d
                   # +ind_cars_o*ind_cars_d
                   |                    ## Other controls
                     connexion_city + year,                             ## FEs
                   cluster = ~ o_city,                          ## Clustered SEs
                   data = df)
  
  mod_sunab = feols(log(passengers) ~ sunab(year_treated,year) + ## Our key interaction: time � treatment status
                      # log(o_POP*d_POP)+
                      log(o_SPA*d_SPA)
                    # + t_global
                    # +ind_rail_o*ind_rail_d
                    # +ind_cars_o*ind_cars_d
                    |                    ## Other controls
                      connexion_city + year,                             ## FEs
                    cluster = ~ o_city,                        ## Clustered SEs
                    data = df)
  
  #Plot the two TWE
  iplot(list(mod_twfe, mod_sunab), sep = 0.1,col = c(1, 4),pt.pch= c(20,15))
  abline(v=-0.5)
  legend(legend_place, col = c(1, 4), pch = c(20, 15), 
         legend = c("TWFE", "Sun & Abraham (2020) TWFE"))
}

time_window_event_tax <- function(df,list){
  # #d�but fonction
  df$treated <- 0
  df[df$country_1 %in% list$country_1 | df$country_2 %in% list$country_1,"treated"] <- 1
  # df[df$orig_count %in% list$orig_count,"treated"] <- 1
  
  df<- left_join(df,list,by="country_1",keep=FALSE)
  df[df$treated==0,"year_treated"] <- 10000
  
  df$time_to_treatment <- -1000
  df[df$treated==1,"time_to_treatment"] <- df[df$treated==1,"year"]-df[df$treated==1,"year_treated"]
  
  return(table(df$time_to_treatment))
}



event_tax_l <- function(df,list,list2,min_year=NA,max_year=NA,
                        legend_place="topright",legend='Event study: Staggered treatment (TWFE) on HSR openings >50km'){
  # #exemple
  # df=df_final
  # # list <- data.frame(
  # #   orig_count=c("UK"),
  # #   year_treated=c(2007))
  # list <- data.frame(
  #   orig_count=c("AT","DE","UK"),
  #   year_treated=c(2011,2011,2007))
  # min_year=-8
  # max_year=8
  # legend_place="topright"
  # legend='test'
  # 
  # 
  # #d�but fonction
  df$treated <- 0
  df[df$country_1 %in% list$country_1 | df$country_2 %in% list$country_1,"treated"] <- 1

  #Remove countries with tax implemented then removed 
  df <- filter(df,!(country_1 %in% list2),!(country_2 %in% list2))

  # df[df$orig_count %in% list$orig_count,"treated"] <- 1
  
  df<- left_join(df,list,by="country_1",keep=FALSE)
  df[df$treated==0,"year_treated"] <- 10000

  df$time_to_treatment <- -1000
  df[df$treated==1,"time_to_treatment"] <- df[df$treated==1,"year"]-df[df$treated==1,"year_treated"]
  
  #Filter on time_to_treatment to get the right window
  if(!is.na(min_year) & !is.na(max_year)){
    df <- filter(df,(time_to_treatment>=min_year & time_to_treatment<=max_year) |time_to_treatment==-1000)
  }
  
  
  mod_twfe = feols(log(passengers) ~ i(time_to_treatment,ref=c(-1,-1000))+ ## Our key interaction: time � treatment status             
                   log(SPA_bilat)+ new_rail
                   |                    ## Other controls
                     connexion_city + year,                             ## FEs
                   cluster = ~  connexion_city ,                        ## Clustered SEs
                   data = df)
  # summary( mod_twfe)
  
  mod_sunab = feols(log(passengers) ~ sunab(year_treated,year) + ## Our key interaction: time � treatment status
                      log(SPA_bilat) + new_rail
                    |                    ## Other controls
                      connexion_city + year,                             ## FEs
                    cluster = ~ connexion_city ,                        ## Clustered SEs
                    data = df)
  x11(width = 20, height = 14)
  iplot(list(mod_twfe, mod_sunab), sep = 0.1,col = c(1, 4),pt.pch= c(20,15),
        xlab = 'Time to treatment',
        main = legend)
  abline(v=-0.5)
  legend(legend_place, col = c(1, 4), pch = c(20, 15), 
         legend = c("TWFE", "Sun & Abraham (2020) TWFE"))
}



 
