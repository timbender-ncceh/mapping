library(proj4) # for ptransform


proj_LonLat2LCC <- function(df_lonlat = data.frame(x = c(-82,-81), 
                                                   y = c( 36, 35))){
  require(proj4)
  out <- ptransform(df_lonlat/180*pi, 
                    src.proj = '+proj=latlong +ellps=sphere',
                    dst.proj = '+proj=lcc +lat_1=34.33333333333334 +lat_2=36.16666666666666 +lat_0=33.75 +lon_0=-79.0 +x_0=609601.22 +y_0=0')
  # transform formatting to a data.frame output
  out <- data.frame(X = out$x, 
                    Y = out$y)
  # return output
  return(out)
}

proj_LCC2LonLat <- function(df_LCC = data.frame(X = c(339162.755, 427058.740), 
                                                Y = c(253687.426, 140501.545))){
  require(proj4)
  out <- ptransform(df_LCC, 
                    dst.proj = '+proj=latlong +ellps=sphere',
                    src.proj = '+proj=lcc +lat_1=34.33333333333334 +lat_2=36.16666666666666 +lat_0=33.75 +lon_0=-79.0 +x_0=609601.22 +y_0=0')
  out <- data.frame(x = out$x, 
                    y = out$y)
  out <- out / pi * 180
  return(out)
}

