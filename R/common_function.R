get_histo = function(Data,col_plot="pval",breaks=0.03,titre, language){
  
  Gain = round(mean(as.numeric(as.character(Data$overyielding)))*100,2)
  Mean=mean(as.numeric(as.character(Data$overyielding)))
  Positif = round(length(Data$overyielding[as.numeric(as.character(Data$overyielding))>0])*100/length(Data$overyielding),2)
  Negatif = round(length(Data$overyielding[as.numeric(as.character(Data$overyielding))<0])*100/length(Data$overyielding),2)
  
  B= ifelse(abs(max(as.numeric(as.character(Data$overyielding)))) > abs(min(as.numeric(as.character(Data$overyielding)))),max(as.numeric(as.character(Data$overyielding))),min(as.numeric(as.character(Data$overyielding))))
  
  
  pval= NULL
  for (i in 1:nrow(Data)){
    if (as.numeric(as.character(Data[i,"pvalue"])) <= 0.01){pval = c(pval,ifelse(language=="french","Significatif à 0.01","Significant at 0.01"))}
    if (as.numeric(as.character(Data[i,"pvalue"])) <= 0.05 & as.numeric(as.character(Data[i,"pvalue"])) > 0.01 ){pval = c(pval,ifelse(language=="french","Significatif à 0.05","Significant at 0.05"))}
    if (as.numeric(as.character(Data[i,"pvalue"])) > 0.05){pval = c(pval,ifelse(language=="french","Non significatif (pvalue >0.05)","Not significant (pvalue >0.05)"))}
  }
  
  if(col_plot == "year"){
    p =  ggplot(data=Data,aes(as.numeric(as.character(overyielding)),fill=as.factor(year))) + scale_fill_discrete(name = ifelse(language=="french","Année","Year"))
  }else if(col_plot == "melange"){
    p =  ggplot(data=Data,aes(as.numeric(as.character(overyielding)),fill=as.factor(melange))) + scale_fill_discrete(name = ifelse(language=="french","Mélange","Mixture"))
  }else if(col_plot == "location"){
    p =  ggplot(data=Data,aes(as.numeric(as.character(overyielding)),fill=as.factor(location))) + scale_fill_discrete(name = ifelse(language=="french","Paysan","Farmer"))
  }else if(col_plot == "modalite"){
    p =  ggplot(data=Data,aes(as.numeric(as.character(overyielding)),fill=as.factor(mod))) + scale_fill_discrete(name = ifelse(language=="french","Modalité","Modality"))
  }else{
    p =  ggplot(data=Data,aes(as.numeric(as.character(overyielding)),fill=as.factor(pval)))  + scale_fill_discrete(name = ifelse(language=="french","Significativité","Significance"))
  }
  
  if(shapiro.test(as.numeric(as.character(Data$overyielding)))$p.value <0.05){
    Signif = wilcox.test(as.numeric(as.character(Data$overyielding)),mu=0)$p.value
  }else{
    Signif = t.test(as.numeric(as.character(Data$overyielding)),mu=0)$p.value
  }
  sign = get_stars(Signif)
  
  Mean = round(mean(as.numeric(as.character(Data$overyielding))),3)
  p = p + geom_histogram(breaks=seq(1.5*min(as.numeric(as.character(Data$overyielding))),1.5*max(as.numeric(as.character(Data$overyielding))),breaks), alpha=0.6, color="black")
  p = p + geom_vline(xintercept = Mean, size = 1.2, color="red") + geom_vline(xintercept = 0,  linetype = "dotted")
  p = p + labs(x=paste(ifelse(language=="french","Différence normalisée entre les mélanges et 
	                         la moyenne de leurs composantes pour ","Normalized difference between mixtures and their components' mean for "),titre,sep=""), 
               y=ifelse(language=="french","Nombre de mélanges","Number of mixtures"))
  p = p + labs(title = paste(titre,":",ifelse(language=="french","Gain moyen =","Mean gain ="),Mean*100,"% (",sign,");
        ",ifelse(language=="french","Cas positifs :","Positive cases:"),Positif,"%",";",ifelse(language=="french","Cas négatifs :","Negative cases:"),Negatif,"%",sep=" "))
  
  return(list("plot"=p,"pval"=Signif))
}

get_stars = function(res) {
  stars = findInterval(res, c(0, 0.001, 0.01, 0.05, 0.1))
  stars[is.na(stars)]=5
  codes = c("***" , "**","*", ".", " ")
  return(codes[stars])
}

# get duplicated data in 1 table
dupl_table = function(tab){
  tab = lapply(tab,function(x){return(cbind(x[[1]],x[[2]]))})
  TAB = NULL
  for (i in 1:length(tab)){TAB = rbind(TAB,tab[[i]])}
  
  attributes(tab)$invert=FALSE
  return(tab)
}



# clean data 
mag = function(d){
  # tkw
  if( length(grep("^tkw---tkw$", colnames(d$data))) > 0 ) {
    a = as.numeric(d$data$"tkw---tkw")
    a[which(a>70)] = NA
    d$data$"tkw---tkw" = a
  }
  
  # protein
  if( length(grep("^protein---protein$", colnames(d$data))) > 0 ) {
    a = as.numeric(as.character(d$data$"protein---protein"))
    a[which(a>20)] = NA
    d$data$"protein---protein" = a
  }
  
  # spike_weight
  if( length(grep("^spike_weight---spike_weight$", colnames(d$data))) > 0 ) {
    a = as.numeric(as.character(d$data$"spike_weight---spike_weight"))
    a[which(a>10)] = NA
    d$data$"spike_weight---spike_weight" = a
  }
  
  # plant_height
  if( length(grep("^plant_height---plant_height$", colnames(d$data))) > 0 ) {
    a = as.numeric(as.character(d$data$"plant_height---plant_height"))
    a[which(a==0)] = NA # mesure en m
    a[which(a<2)] = a[which(a<2)] * 1000 # mesure en m
    a[which(a>2 & a<500)] = NA # valeurs bizarres
    d$data$"plant_height---plant_height" = a
  }
  
  # LLSD
  if( length(grep("^LLSD---LLSD$", colnames(d$data))) > 0 ) {
    a = as.numeric(as.character(d$data$"LLSD---LLSD"))
    a[which(a < 0)] = 0
    a[which(a > 500)] = NA
    d$data$"LLSD---LLSD" = a
  }
  
  
  # nbr_kernels---nbr.epillets
  if( length(grep("^nbr_kernels---nbr_kernels$", colnames(d$data))) > 0 ) {
    a = as.numeric(as.character(d$data$"nbr_kernels---nbr_kernels"))
    a[which(a <= 0)] = NA
    a[which(a > 60)] = NA
    d$data$"nbr_kernels---nbr_kernels" = a
  }
  
  #  spike_length---spike_length
  if( length(grep("^spike_length---spike_length$", colnames(d$data))) > 0 ) {
    a = as.numeric(as.character(d$data$"spike_length---spike_length"))
    a[which(a <= 0)] = NA
    a[which(a > 250)] = NA
    d$data$"spike_length---spike_length" = a
  }
  
  #  rdt
  if( length(grep("^rdt$", colnames(d$data))) > 0 ) {
    a = as.numeric(as.character(d$data$"rdt"))
    a[which(a <= 5)] = NA
    a[which(a > 100)] = NA
    d$data$"rdt" = a
  }
  
  if( length(grep("^rdt_parcelle---rdt_parcelle$", colnames(d$data))) > 0 ) {
    a = as.numeric(as.character(d$data$"rdt_parcelle---rdt_parcelle"))
    a[which(a <= 5)] = NA
    a[which(a > 100)] = NA
    d$data$"rdt_parcelle---rdt_parcelle" = a
  }
  
  # estimated_nbr_grain_spike
  if( length(grep("^estimated_nbr_grain_spike---estimated_nbr_grain_spike$", colnames(d$data))) > 0 ) {
    a = as.numeric(as.character(d$data$"estimated_nbr_grain_spike---estimated_nbr_grain_spike"))
    a[which(a <= 0)] = NA
    a[which(a > 100)] = NA
    d$data$"estimated_nbr_grain_spike---estimated_nbr_grain_spike" = a
  }
  
  return(d)
}

# Traduire les vec_variables
traduction = function(tab,row_or_col)	{
  
  if( !is.null(tab$duplicated_infos)) {
    for (i in 1:length(tab$duplicated_infos)) {
      if (row_or_col == "row") {rownames(tab$duplicated_infos[[i]]$duplicated_infos_variables) = gsub("^([^---]*)---.*$", "\\1",rownames(tab$duplicated_infos[[i]]$duplicated_infos_variables))
      }else{colnames(tab$duplicated_infos[[i]]$duplicated_infos_variables) = gsub("^([^---]*)---.*$", "\\1",colnames(tab$duplicated_infos[[i]]$duplicated_infos_variables)) }
    }
  }
  
  if( !is.null(tab$not_duplicated_infos)) {
    for (i in 1:length(tab$not_duplicated_infos)) {
      if (row_or_col == "row") {rownames(tab$not_duplicated_infos[[i]]) = gsub("^([^---]*)---.*$", "\\1",rownames(tab$not_duplicated_infos[[i]]))
      }else{colnames(tab$not_duplicated_infos[[i]]) = gsub("^([^---]*)---.*$", "\\1",colnames(tab$not_duplicated_infos[[i]])) }
    }
  }
  
  return(tab)
}



