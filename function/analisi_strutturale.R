
analisi_strutturale<-function(object, ...){
  arguments <- list(...)
  if (sum(names(arguments)=="k")==0){k=10} else {k <- arguments$k}
  if (sum(names(arguments)=="verbose")==0){verbose <- TRUE} else {verbose <- FALSE}
  
  mainInfo= toupper("Informazioni generali sul dataset preso in esame\n\n")
  mainInfo[length(mainInfo)+1]=paste("Numero degli studenti    ",summarize(object, n()), "\n")
  mainInfo[length(mainInfo)+1]=paste("Eta'minima               ",min(object$eta), "\n")
  mainInfo[length(mainInfo)+1]=paste("Eta' massima             ",max(object$eta), "\n")
  
  if (isTRUE(verbose)){cat(mainInfo)}
  
  if (isTRUE(verbose)) cat("\nDivisi per Genere: \n")
  Y=data.frame(table(object$genere))
  names(Y)=c("Genere", "  Numero Studenti")
  if (isTRUE(verbose)) print(Y[1], row.names=FALSE)
  
  otherInfo= toupper("\nRange di valori\n")
  otherInfo[length(otherInfo)+1]=paste("GPA:                   ",min(object$gpa), "-", max(object$gpa), "\n")
  otherInfo[length(otherInfo)+1]=paste("Supporto familiare:    ",min(object$supp_famiglia), "-", max(object$supp_famiglia), "\n")
  otherInfo[length(otherInfo)+1]=paste("Stress finanziario:    ",min(object$stress_finanz), "-", max(object$stress_finanz), "\n")
  otherInfo[length(otherInfo)+1]=paste("Pressione dei pari:    ",min(object$stress_parità), "-", max(object$stress_parità), "\n")
  otherInfo[length(otherInfo)+1]=paste("Stress relazionale:    ",min(object$stress_relaz), "-", max(object$stress_relaz), "\n")
  otherInfo[length(otherInfo)+1]=paste("Stress mental:         ",min(object$stress_mental), "-", max(object$stress_mental), "\n")
  otherInfo[length(otherInfo)+1]=paste("Qualità della dieta:   ",min(object$quali_dieta), "-", max(object$quali_dieta), "\n")
  otherInfo[length(otherInfo)+1]=paste("Distorsioni cognitive: ",min(object$distorsioni_cognitive), "-", max(object$distorsioni_cognitive), "\n")
  otherInfo[length(otherInfo)+1]=paste("Uso di stostanze:      ",min(object$uso_sostanze), "-", max(object$uso_sostanze), "\n\n")
  if (isTRUE(verbose)) cat(otherInfo)
  
  cat("\nStudenti che effettuano sedute di consulenza: \n")
  X=data.frame(table(object$consulenza))
  names(X)=c("Consulenza", "  Numero Studenti")
  print(X, row.names=FALSE)
  
  cat("\nAnamnesi familiare di problemi di salute mentale: \n")
  Z=data.frame(table(object$family_mental_health))
  names(Z)=c("Condizione", "  Numero Studenti")
  print(Z, row.names=FALSE)
  
  cat("\nCondizioni mediche degli studenti: \n")
  V=data.frame(table(object$condiz_mediche))
  names(V)=c("Condizione", "  Numero Studenti")
  print(V, row.names=FALSE)
  
  
  summaryresults=list(MainInformation=mainInfo, OtherInformation=otherInfo)
  
  invisible(summaryresults)
  
}
 
