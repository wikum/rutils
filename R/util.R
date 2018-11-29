
require(tidyr)
require(knitr)
require(caret)
require(pROC)

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
  
  df = data.frame(
    pheno=y, 
    group=factor(rep("TEST", length(y)), levels=c("TRAIN", "TEST"))
  )
  df$group[which(1:length(y) %in% train)] = "TRAIN"
  if(! is.null(samples))
    df$sample = samples
  
  df
  
}

getPredictionStats = function(predictions, truth, controlClass, caseClass, decision_values=NULL){
  
  cm = confusionMatrix(data=predictions, reference=truth, positive=caseClass)
  acc = cm$overall["Accuracy"]
  sens = cm$byClass["Sensitivity"]
  spec = cm$byClass["Specificity"]
  bacc = cm$byClass["Balanced Accuracy"]
  
  auc = NA
  if(! is.null(decision_values)){
    auc = as.numeric(auc(response=truth, predictor=decision_values))
  }
  
  out = c(accuracy=acc, sensitivity=sens, specificity=spec, balanced_accuracy=bacc, auc=auc)
  names(out) = c("accuracy", "sensitivity", "specificity", "balanced_accuracy", "auc")
  
  out
  
}

calculate_confusions = function(x, y, phenotypes){
  
  p.negative = phenotypes[1]
  p.positive = phenotypes[2]
  
  z = sort(unique(x))
  dz = diff(z)/2
  
  thresholds = c(
    z[1]-dz[1],
    z[1:(length(z)-1)]+dz,
    z[length(z)]+tail(dz, 1)
  )
  
  u = t(sapply(thresholds, function(t){
    
    yt = ifelse(x <= t, p.negative, p.positive)
    
    tn = sum(yt[which(y == p.negative)] == p.negative)
    tp = sum(yt[which(y == p.positive)] == p.positive)
    fn = sum(yt[which(y != p.negative)] == p.negative)
    fp = sum(yt[which(y != p.positive)] == p.positive)
    
    sens = tp/(tp+fn)
    spec = tn/(tn+fp)
    ppv = ifelse(tp+fp == 0, 1, tp/(tp+fp))
    npv = ifelse(tn+fn == 0, 1, tn/(tn+fn))
    acc = (tp+tn)/length(y)
    bacc = (sens+spec)/2
    
    c(threshold=t,
      sensitivity=sens, specificity=spec, PPV=ppv, NPV=npv, 
      accuracy=acc, balanced_accuracy=bacc)
    
  }))
  
  data.frame(u)
}

value_match = function(from, to, x){

	if(length(from) != length(to))
		stop("from and to must be of equal length")

	u = rep(NA, length(x))
	v = intersect(unique(x), from)
	for(j in 1:length(v))
		u[which(x == v[j])] = to[which(from == v[j])[1]]

	u

}

print_venn = function(x, y){

  sprintf("%d [%d | %d | %d ] %d",
    length(x),
    sum(! x %in% y),
    length(intersect(x, y)),
    sum(! y %in% x),
    length(y)
  )

}
