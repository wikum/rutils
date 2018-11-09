

### ========================================================
### Hypothesis testing
### ========================================================

featureSelect.Wilcoxon = function(Mat, Groups, classes, ...){
  
  w = apply(Mat, 1, function(x) 
    wilcox.test(x[which(Groups == classes[1])], x[which(Groups == classes[2])], ...)
  )
  
  pvals = sapply(w, function(x) x$p.value)
  stats = sapply(w, function(x) x$statistic)
  
  pr = sort(pvals, decreasing=FALSE, index.return=TRUE, na.last=TRUE, method='radix')$ix
  
  df = data.frame(statistic=stats, 
                  pval=pvals, 
                  pbon=p.adjust(pvals, method="bonferroni"),
                  pbh=p.adjust(pvals, method="BH")
                  )
  rownames(df) = rownames(Mat)
  
  df[pr, ]
  
}

featureSelect.t = function(Mat, Groups, classes, ...){
  
  tt = apply(Mat, 1, function(x) 
    t.test(x[which(Groups == classes[1])], x[which(Groups == classes[2])], ...)
  )
  
  pvals = sapply(tt, function(x) x$p.value)
  stats = sapply(tt, function(x) x$statistic)
  
  pr = sort(pvals, decreasing=FALSE, index.return=TRUE, na.last=TRUE, method='radix')$ix
  
  df = data.frame(statistic=stats, 
                  pval=pvals, 
                  pbon=p.adjust(pvals, method="bonferroni"),
                  pbh=p.adjust(pvals, method="BH")
  )
  rownames(df) = rownames(Mat)
  
  df[pr, ]
  
}
