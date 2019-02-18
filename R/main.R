

### ========================================================
### Utility functions
### ========================================================

#' Melt a matrix to a data.frame
#'
#' @export
#' 
utils.meltMat <- function(Mat, order=TRUE){
  meltMat(Mat=Mat, order=order)
}

#' Convert list to data.frame
#' 
#' @export
#' 
utils.list_do_df <- function(l){
  list_to_df(l)
}

#' Make factor with sample counts
#' 
#' @export
#' 
utils.make_n_factor <- function(x){
  make_n_factor(x)
}

#' Add factor with sample counts to current factor
#' 
#' @export
#' 
utils.add_n_factor <- function(x){
  make_n_factor.2(x)
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
utils.lapply_c <- function(x, ...){
  lapply_c(x, ...)
}

#'
#' @export
#' 
utils.lapply_i <- function(x, fun, ...){
  lapply_i(x=x, fun=fun, ...)
}

#'
#' @export
#' 
utils.sapply_c <- function(x, ...){
  sapply_c(x, ...)
}

#'
#' @export
#' 
utils.kable_vector <- function(v, j, cols=NULL, ...){
  kable_vector(v=v, j=j, cols=cols, ...)
}

#'
#' @export
#' 
utils.split_data <- function(y, classes, p=.5, samples=NULL){
  splitData(y=y, classes=classes, p=p, samples=samples)
}

#' @export
utils.get_prediction_stats <- function(predictions, truth, controlClass, caseClass, 
                                      decision_values=NULL){
  getPredictionStats(predictions=predictions, 
                     truth=truth, 
                     controlClass=controlClass, 
                     caseClass=caseClass, 
                     decision_values=decision_values)
}

#' @export
utils.calculate_confusions <- function(x, y, phenotypes){
  calculate_confusions(x=x, y=y, phenotypes=phenotypes)
}


#' @export
utils.value_match <- function(from, to, x){
  value_match(from=from, to=to, x=x)
}


#' @export
utils.print_venn <- function(x, y){
  print_venn(x=x, y=y)
}


#' @export
utils.make_pairs <- function(x){
  make_pairs(x=x)
}

#' @export
utils.print_progress <- function(i, k, m, prefix="", suffix=""){
  print_progress(i=i, k=k, m=m, prefix=prefix, suffix=suffix)
}

### ========================================================
### Hypothesis testing/feature selection
### ========================================================

#' @export
utils.Wilcoxon <- function(Mat, Groups, classes, ...){
  featureSelect.Wilcoxon(Mat=Mat, Groups=Groups, classes=classes, ...)
}

#' @export
utils.t <- function(Mat, Groups, classes, ...){
  featureSelect.t(Mat=Mat, Groups=Groups, classes=classes, ...)
}

### ========================================================
### ggplot2 themes
### ========================================================

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





