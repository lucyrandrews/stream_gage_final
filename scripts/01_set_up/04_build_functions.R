## BUILD FUNCTIONS

# This scripts builds custom functions used in this analysis.

# Functions ----

#' Identify the midpoint of an sf line object
#' 
#' @param sf_lines An sf object with geometry type LINESTRING or MULTILINESTRING
#' @return The midpoints of \code{sf_lines} as an sf object with geometry type POINT

st_line_midpoints <- function(sf_lines = NULL) {
  
  g <- st_geometry(sf_lines)
  
  g_mids <- lapply(g, function(x) {
    
    coords <- as.matrix(x)
    
    get_mids <- function (coords) {
      dist <- sqrt((diff(coords[ , 1])^2 + (diff(coords[ , 2]))^2))
      dist_mid <- sum(dist)/2
      dist_cum <- c(0, cumsum(dist))
      end_index <- which(dist_cum > dist_mid)[1]
      start_index <- end_index - 1
      start <- coords[start_index, ]
      end <- coords[end_index, ]
      dist_remaining <- dist_mid - dist_cum[start_index]
      mid <- start + (end - start) * (dist_remaining/dist[start_index])
      return(mid)
    }
    
    mids <- st_point(get_mids(coords))
    
  })
  
  geometry <- st_sfc(g_mids, crs = st_crs(sf_lines))
  geometry <- st_sf(geometry)
  
}



#' Rescale a numeric vector so that all values are between 0 and 1 without
#' performing any normalizing
#' 
#' @param vec A vector of numeric values
#' @return A vector of numeric values between 0 and 1

rescale_zero_one <- function(x) {
  
  x / max(x, na.rm = TRUE)

}



#' Function that quickly loads spatial data attributes in viewer
#' 
#' @param sf_obj An object of class sf

view_flat <- function(sf_obj) {
  sf_obj %>%
    st_drop_geometry() %>%
    View(.)
}