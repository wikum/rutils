###
### Utility functions
###

getPredictionStats = function(predictions, truth, classes=NULL, decision_values=NULL){
  
  if(length(predictions) != length(truth))
    stop("Predictions and true label vectors are not of the same length")

  # accuracy
  accuracy = NULL
  # balanced accuracy = (sensitivity + specificity) / 2
  balanced_accuracy = NULL
  # sensitivity = accuracy of class 1 (positive class)
  sensitivity = NULL
  # specificity = accuracy of class 0
  specificity = NULL

  # If classes are not supplied, take the factor levels of Groups argument; 
  # assume the alphanumeric order is class 0 followed by class 1, so reverse the factor levels
  # since we expect the classes argument to be (class 1, class 0)
  if(is.null(classes))
      classes = rev(levels(factor(truth)))
  
  # calculate the stats
  accuracy = sum(predictions == truth)/length(predictions)
  sensitivity = sum(predictions[ truth == classes[1] ] == truth[ truth == classes[1] ])/sum(truth == classes[1])
  specificity = sum(predictions[ truth == classes[2] ] == truth[ truth == classes[2] ])/sum(truth == classes[2])
  balanced_accuracy = (sensitivity + specificity) / 2
  
  if(is.null(decision_values)){
    
    c(accuracy=accuracy, sensitivity=sensitivity, specificity=specificity, balanced_accuracy=balanced_accuracy)
  
  }else{
    
    auc = NULL
    
    tryCatch({
      auc = auc(roc(truth, decision_values, levels=classes, direction=">"))
    }, error = function(e){
      #print(e)
    })

    c(accuracy=accuracy, sensitivity=sensitivity, specificity=specificity, balanced_accuracy=balanced_accuracy, auc=auc)
  }
  
}










