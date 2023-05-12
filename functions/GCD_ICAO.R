GCD_ICAO <- function(airp_o,airp_d){
  #airp_o and airp_d are the IATA codes of the airports
  if(airp_o==airp_d){dist <- 0}else{
    dist <- 1/1000*distCosine(c(airport_coord[airport_coord$ICAO==airp_o,8],airport_coord[airport_coord$ICAO==airp_o,7]),
                              c(airport_coord[airport_coord$ICAO==airp_d,8],airport_coord[airport_coord$ICAO==airp_d,7]),
                              r=6378137)
  }
  #According to ICAO methodology, we add a correction factor that depends on the distance
  if(dist==0){}else{
    if(dist<550){dist <- dist + 50}else{
      if(dist<5500){dist <- dist + 100}else{
        dist <- dist + 125
      }
    }
  }
  return(dist)
}

GCD_ICAO_coord <- function(o_lat_o_long_d_lat_d_long){
  o_lat <- o_lat_o_long_d_lat_d_long[1,1]
  o_long <- o_lat_o_long_d_lat_d_long[1,2]
  d_lat <- o_lat_o_long_d_lat_d_long[1,3]
  d_long <- o_lat_o_long_d_lat_d_long[1,4]
  #airp_o and airp_d are the IATA cods of the airports
  if(o_long==d_long & o_lat==d_lat){dist <- 0}else{
    dist <- 1/1000*distCosine(c(o_long,o_lat),
                              c(d_long,d_lat),
                              r=6378137)
  }

  return(dist)
}
