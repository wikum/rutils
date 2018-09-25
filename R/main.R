

#' Melt a matrix to a data.frame
#'
#' @export
#' 
util.meltMat <- function(Mat){
  meltMat(Mat)
}

#' Convert list to data.frame
#' 
#' @export
#' 
util.list_do_df <- function(l){
  list_to_df(l)
}

#' Make factor with sample counts
#' 
#' @export
#' 
utils.make_n_factor <- function(x){
  make_n_factor(x)
}

#' Make sample count factor data.frame
#' 
#' @export
#' 
utils.make_n_factor_map <- function(x){
  make_n_factor_map(x)
}

#'
#' @export
#' 
utils.lapply_c <- function(x){
  lapply_c(x)
}

#'
#' @export
#' 
utils.lapply_i <- function(x, fun, ...){
  lapply_i(x, fun, ...)
}


#' @export
gtheme.GENERIC <- function(x){
  themeGENERIC()
}


#' @export
gtheme.NO_X_LABS <- function(x){
  themeNO_X_LABS()
}

#' @export
gtheme.X_HORIZ <- function(x){
  themeX_HORIZ()
}

#' @export
gtheme.X_VERT <- function(x){
  themeX_VERT()
}





