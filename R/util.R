
require(tidyr)
require(knitr)

### ========================================================
### Utility functions
### ========================================================

meltMat = function(Mat){
  Mat = as.data.frame(Mat)
  meltedMat = gather(Mat)
  colnames(meltedMat) = c("colKey", "value")
  meltedMat$rowKey = rep(rownames(Mat), ncol(Mat))
  meltedMat
}


list_to_df = function(l){
  data.frame(x=unlist(l), y=rep(names(l), sapply(l, length)))
}


# e.g. x = c("A", "A", "A", "B", "B", "B", "C", "C", "D", "D", "D", "E", "E", "F")
make_n_factor = function(x){
  
  if(! is.factor(x))
    x = as.factor(x)
  
  y = levels = levels(x)
  z = sapply(y, function(u) sprintf("n=%d", sum(x == u, na.rm=TRUE)))
  
  # add extra spaces at the beginning for groups with same number of samples
  while(sum(duplicated(z)) > 0){
    z[duplicated(z)] = paste(" ", z[duplicated(z)], sep="")
  }
  
  factor(x, levels=y, labels=z)

}

make_n_factor.2 = function(x){
  
  nf = make_n_factor(x)
  y = paste(x, " (", nf, ")", sep="")
  
  factor(y)
  
}

make_n_factor_map = function(x){
  
  if(! is.factor(x))
    x = as.factor(x)
  
  y = levels=levels(x)
  z = sapply(y, function(u) sprintf("n=%d", sum(x == u, na.rm=TRUE)))
  
  data.frame(levels=y, count=z, levelsSuffixed=paste(y, " (", z, ")", sep=""))
}

lapply_c = function(x, ...){
  
  result = lapply(x, ...)
  names(result) = x
  
  result
  
}

# fun(x, x_i, x_label)
lapply_i = function(x, fun, ...){

	out.list = lapply(1:length(x), function(xi) fun(x[[xi]], xi, names(x)[xi], ...))
	names(out.list) = names(x)
	out.list

}

# j = columns, i = rows
kable_vector = function(v, j, cols=NULL, ...){
  
  i = ceiling(length(v)/j)
  w = rep("", i * j)
  w[1:length(v)] = v
  
  m = matrix(w, nrow=i, ncol=j, byrow=TRUE)
  df = data.frame(m)
  
  if(is.null(cols))
    cols = rep("", j)
  
  kable(df, col.names=cols, ...)
  
}

sapply_c = function(x, ...){
  
  result = lapply_c(x, ...)
  Reduce(c, result)
  
}

splitData = function(y, classes, p=.5, samples=NULL){
  
  id0 = which(y == classes[1])
  id1 = which(y == classes[2])
  
  train = c( sample(id0, floor(length(id0) * p)), sample(id1, floor(length(id1) * p)) )
  
  df = data.frame(pheno=y, group="TEST")
  df$group[which(1:length(y) %in% train)] = "TRAIN"
  if(! is.null(samples))
    df$sample = samples
  
  df
  
}

