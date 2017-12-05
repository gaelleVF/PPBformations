# 0. help -----------------------------------------------------------------
#' Generates barplots for mixtures
#' 
#' @param x data frame containing the name of the populations, the median of the character, the type ("Mixture","Component",...)
#'
#' @param title title of the plot
#' 
#' @return The boxplot
#' 
#' @author Gaelle Van Frank
#' 
#' @seealso \code{\link{PPBformations::ggplot_mixture1}}
#' 

barplot.mixture1 = function(x, title, melange) {
  Gain = round((x[grep(paste("^",melange,"$",sep=""),x$entry),"median"]/x[grep("MoyenneComposantes",x$entry),"median"]-1)*100,2)
  x$type = factor(x$type, levels=c("Composante","MoyenneComposantes","Mélange non sélectionné","Mélange issu 2 années sélection 
  dans composantes (Mod1)","Mélange issu 1 année sélection 
dans composantes (Mod2)","Mélange sélectionné (Mod3)"))
  
  p = ggplot(x, aes(x = reorder(germplasm, median), y = median, fill=unlist(x$type))) + geom_bar(stat = "identity")+ theme(legend.title = element_blank())
  p = p + scale_fill_manual(drop=TRUE,values =c("Composante"="olivedrab3","MoyenneComposantes"="green4","Mélange non sélectionné"="red","Mélange issu 2 années sélection \ndans composantes (Mod1)"="springgreen3",
"Mélange issu 1 année sélection \ndans composantes (Mod2)"="gold1","Mélange sélectionné (Mod3)"="skyblue1"))
  
  # ajouter les groupes de significativité
  p = p + geom_text(data = x, aes(x = reorder(germplasm, median), y = median/2, label = groups), angle = 90, color = "white")
  if(!is.null(Gain)){title = paste(title,"
  ","Différence normalisé entre valeur du mélange et valeur moyenne des composantes :",Gain,"%",sep=" ")}
  p = p + ggtitle(title) + ylab("")
  
  # pivoter légende axe abscisses
  p = p + xlab("") + theme(axis.text.x = element_text(angle = 90)) + ylim(0, x[1,"max"])
  
  return(p)
}