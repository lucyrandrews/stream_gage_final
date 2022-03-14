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
      dist <- sqrt((diff(coords[, 1])^2 + (diff(coords[, 2]))^2))
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