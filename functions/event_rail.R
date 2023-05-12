event_rail <- function(df,connection,year_rail,legend_place="topright"){

  df$treated <- 0
  df[df$connexion_city==connection,"treated"] <- 1
  df$year_treated <- rep(10000,nrow(df))
  df[df$connexion_city==connection,"year_treated"] <- year_rail
  df$time_to_treatment <- -1000
  df[df$treated==1,"time_to_treatment"] <- df[df$treated==1,"year"]-df[df$treated==1,"year_treated"]
  table(df$time_to_treatment)
  table(df$treated)
  
  
  mod_twfe = feols(log(passengers) ~ i(time_to_treatment,ref=c(-1,-1000))+ ## Our key interaction: time � treatment status
                   # log(o_POP*d_POP)+
                   log(o_SPA*d_SPA)
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

time_window_event <- function(df,list){
  df$treated <- 0
  df[df$connexion_city %in% list$connexion_city,"treated"] <- 1
  
  df<- left_join(df,list,by="connexion_city",keep=FALSE)
  df[df$treated==0,"year_treated"] <- 10000
  
  
  df$time_to_treatment <- -1000
  df[df$treated==1,"time_to_treatment"] <- df[df$treated==1,"year"]-df[df$treated==1,"year_treated"]
  return(table(df$time_to_treatment))
}
  
event_rail_l <- function(df,list,min_year=NA,max_year=NA,legend_place="topright",legend='Event study: Staggered treatment (TWFE) on HSR openings >50km'){
  

#   # Exemple
#   df=df_final
#   list <- data.frame(
#     connexion_city=c("Linz_Vienna","Munich_Nuernberg","Munich_Stuttgart","Bologna_Rome","Paris_Strasbourg","Naples_Rome","Barcelona_Madrid","Madrid_Valencia","Lyon_Strasbourg","Madrid_Malaga","Madrid_Santiago","Milan_Rome", "Naples_Rome"),
#     year_treated=c(2013,2006,2012,2010,2007,2006,2008,2011,2011,2008,2012,2008,2006))
#   time_window_event(df_final,list)
#   min_year=NA
#   max_year=NA
#   legend_place="topright"
#   legend='Event study: Staggered treatment (TWFE) on HSR openings >50km'
# ##

  df$treated <- 0
  df[df$connexion_city %in% list$connexion_city,"treated"] <- 1
  
  df<- left_join(df,list,by="connexion_city",keep=FALSE)
  df[df$treated==0,"year_treated"] <- 10000
  
  
  df$time_to_treatment <- -1000
  df[df$treated==1,"time_to_treatment"] <- df[df$treated==1,"year"]-df[df$treated==1,"year_treated"]
  
  #Filter on time_to_treatment to get the right window
  if(!is.na(min_year) & !is.na(max_year)){
    df <- filter(df,time_to_treatment>=min_year & time_to_treatment<=max_year |time_to_treatment==-1000)
  }
  table(df$year_treated)
  mod_twfe = feols(log(passengers) ~ i(time_to_treatment,ref=c(-1,-1000))+ ## Our key interaction: time � treatment status
                     log(SPA_bilat)
                      + t_global
                   |                    ## Other controls
                     connexion_city + year,                             ## FEs
                   cluster = ~  connexion_city,                          ## Clustered SEs
                   data = df)
  
  mod_sunab = feols(log(passengers) ~ sunab(year_treated,year) + ## Our key interaction: time � treatment status
                      log(SPA_bilat)
                      + t_global
                    |                    ## Other controls
                      connexion_city + year,                             ##? FEs
                    cluster = ~  connexion_city,                        ## Clustered SEs
                    data = df)
  #Plot the two TWE
  x11(width = 20, height = 14)
  iplot(list(mod_twfe, mod_sunab), sep = 0.1,col = c(1, 4),pt.pch= c(20,15),
        xlab = "Time to treatment (years)",ylab="",
        main = legend)
  abline(v=-0.5)
  legend("bottomleft", col = c(1, 4), pch = c(20, 15), 
         legend = c("Two-way fixed-effects", "Sun & Abraham (2020) TWFE"))
}

