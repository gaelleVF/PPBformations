# 0. help -----------------------------------------------------------------
#' Study of the intra-population selection and response to selection within the mixture project
#' 
#' @param donnees Results from \code{\link{PPBstats::MC} (list) or from \code{\link{shinemas2R::get.data} (data frame)
#' 
#' @param data_version Result from \code{\link{shinemas2R::get.data}} with query.type = "data-S" with add columns type ("Composante" or "Mélange") and modalite which is the discriminating factor when empile = TRUE
#' 
#' @param variable The variable on which the analysis is done. Must be one of the elements of donnees
#' 
#' @param person Element of the plot title
#' 
#' @param empile If TRUE, separate the graphic in x plots depending on the x levels of the data_version's "modalite" column 
#' 
#' @param nom A name to put on graphic title
#' 
#' @return A list containing the plot and the data frame containing the data
#' 
#' @author Gaelle Van Frank
#' 
#' @seealso \code{\link{}}
#' 
analyse.selection <- function(Mixtures, 
                              selection.type,
                              res_model1, 
                              vec_variables, 
                              plot.save=NULL, 
                              table.save=NULL, 
                              language="english", 
                              list_trad, 
                              year=NULL, 
                              data_mixtures=NULL,
                              data_S_all=NULL,
                              data_SR_all=NULL)
{
  Mix_tot=data_mixtures$Mix_tot

#0.1. functions
  get.data.version=function(data_S_all,language){
    data_version = format.data(data_S_all, data.on = "son", fuse_g_and_s = TRUE, format = "PPBstats")
    #  data_version$type = ifelse(unlist(lapply(as.character(data_version$group),function(x) {return(strsplit(x," ")[[1]][1])})) %in% Melanges, "Mélange","Composante")
    data_version$type = unlist(lapply(unlist(lapply(as.character(data_version$group), function(x){substr(strsplit(x,"#")[[1]][[length(strsplit(x,"#")[[1]])]],1,2)})),function(x){
      c=FALSE
      if(x %in% c("VA","VB","JA")){c=TRUE ; return("Composante")}
      if(x %in% c("BA","JB","BB")){c=TRUE ; return("Mélange")}
      if(c == FALSE){return(" ")}
    }))
    
    data_version$modalite = unlist(lapply(unlist(lapply(as.character(data_version$group), function(x){substr(strsplit(x,"#")[[1]][[length(strsplit(x,"#")[[1]])]],1,2)})),function(x){
      c=FALSE
      if(x %in% c("VA","VB")){c=TRUE ; return(ifelse(language == "english","Within components: M1","Composantes : Mod 1"))}
      if(x %in% "JA"){c=TRUE ; return(ifelse(language == "english","Within components: M2","Composantes : Mod 2"))}
      if(x %in% "JB"){c=TRUE ; return(ifelse(language == "english","Within mixture: M2","Mélanges : Mod 2"))}
      if(x %in% c("BA","BB")){c=TRUE ; return(ifelse(language == "english","Within mixtures: M3","Mélanges : Mod 3"))}
      if (c == FALSE){return("")}
    }))
    c=NULL
    for (i in 1:nrow(data_version)){
      d=data_version[i,]
      if (d$modalite %in% c("Within mixtures: M3","Mélanges : Mod 3") & d$type %in% "Composante" == T){c=c(c,i)}
    }
    if(!is.null(c)){data_version = data_version[-c,]}
    return(data_version)
  }
  
  go.graph.sel = function(res_model1, data_S_year, y, variable, person, empile, sep.by, nom,language){
    D=data_S_year$data$data 
    
    if(nrow(D) >0){
      D = D[unlist(lapply(D$father, function(x){strsplit(as.character(x),"_")[[1]][1]})) %in% unlist(lapply(y$son, function(x){strsplit(as.character(x),"_")[[1]][1]})),]
      data_S_year$data$data = D
      
      if(!is.null(data_S_year$data$data)){
        data_version = format.data(data_S_year, data.on = "son", fuse_g_and_s = TRUE, format = "PPBstats")
        colnames(y)[colnames(y) %in% "germplasm_son"]="germplasm"
        if (sep.by == "Mélange") {
          y$modalite = unlist(lapply(y$expe, function(x){return(y[y$expe %in% x & y$sl_statut %in% "son","germplasm"][1])}))
          z=y[,c("germplasm","modalite")]
          data_version = merge(z,data_version,by="germplasm")
          data_version$type = unlist(lapply(unlist(lapply(as.character(data_version$group), function(x){substr(strsplit(x,"#")[[1]][[length(strsplit(x,"#")[[1]])]],1,2)})),function(x){
            c=FALSE
            if(x %in% c("VA","VB","JA")){c=TRUE ; return("Composante")}
            if(x %in% c("BA","JB","BB")){c=TRUE ; return("Mélange")}
            if(c == FALSE){return(" ")}
          }))
          
          data_version$modalite = unlist(lapply(unlist(lapply(as.character(data_version$group), function(x){substr(strsplit(x,"#")[[1]][[length(strsplit(x,"#")[[1]])]],1,2)})),function(x){
            c=FALSE
            if(x %in% c("VA","VB")){c=TRUE ; return(ifelse(language == "english","Within components: M1","Composantes : Mod 1"))}
            if(x %in% "JA"){c=TRUE ; return(ifelse(language == "english","Within components: M2","Composantes : Mod 2"))}
            if(x %in% "JB"){c=TRUE ; return(ifelse(language == "english","Within mixture: M2","Mélanges : Mod 2"))}
            if(x %in% c("BA","BB")){c=TRUE ; return(ifelse(language == "english","Within mixtures: M3","Mélanges : Mod 3"))}
            if (c == FALSE){return("")}
          }))
        }
        if (sep.by == "Modalite") {
          data_version = get.data.version(data_S_year, language)
          data_version = data_version[unlist(lapply(as.character(data_version$version), function(x){strsplit(x,":")[[1]][2]})) %in% "vracS",]
        }
        
        
        if(variable %in% names(res_model1)){ return(go.analyse.selection(res_model1,data_version,variable,person, empile,language))
        }else{return(go.analyse.selection(D,data_version,variable,person, empile,nom,language))}
        
      } # end if
    } # end for
  } # end function do.graph
  
  Histo = function(res_model1, data_S_year, person, variable,language){
    Mix = Mixtures[Mixtures$location %in% person,]
    if(person=="RDR"){
      MIX = plyr:::splitter_d(Mix, .(expe))
      Mix=MIX[[1]]
    }
    result=list()
    # Général pour le paysan
    if(length(plyr:::splitter_d(Mix, .(expe))) > 1){
      result = c(result,list(go.graph.sel(res_model1, data_S_year, Mix, variable, person, empile=FALSE, sep.by ="Mélange", nom="",language)))
    }
    # Par mélange pour le paysan
    result = c(result,list(go.graph.sel(res_model1, data_S_year, Mix, variable, person, empile=TRUE,sep.by="Mélange",nom="",language)))
    # Par modalité, regroupe tous les mélanges
    result = c(result,list(lapply(plyr:::splitter_d(Mix, .(expe)), function(x){
      return(go.graph.sel(res_model1, data_S_year, x, variable, person, empile=TRUE,sep.by ="Modalite",nom=as.character(unique(x[!is.na(x$son_germplasm),"son_germplasm"])),language))
    })))
    # Pour le paysan, séparer par modalité
    if(length(plyr:::splitter_d(Mix, .(expe))) > 1){
      result = c(result,list(go.graph.sel(res_model1, data_S_year, Mix, variable, person, empile=TRUE,sep.by="Modalite",nom="",language)))
    }
    return(result)
  } # end function Histo
  
  go.analyse.selection = function(donnees, data_version, variable, titre, empile=FALSE, language="english"){
    
    data=unique(data_version[,c("year","location","germplasm","group","type","modalite")])
    data$vrac = paste("mu[",sapply(strsplit(as.character(data$group)," | "),function(x){return(x[[1]])}),",",data$location,":",data$year,"]",sep="")
    data$bouquet = paste("mu[",sapply(strsplit(as.character(data$group)," | "),function(x){return(x[[3]])}),",",data$location,":",data$year,"]",sep="")
    
    #1. If the data was analyzed using bayesian model -----------
    if (class(donnees) == "list"){
      if (!(variable %in% names(donnees))){warning("Variable must be one of donnees's names") ; return(list("histo" = NULL, "tab" = NULL))}
      result = apply(data,1,FUN=compare_model,donnees, variable)
      if(class(result) == "list"){Res=NULL; comp = NULL; for (i in 1:length(result)){if(!is.null(result[[i]])){Res=cbind(Res,result[[i]])}else{comp = c(comp,i)}}; result=Res; data=data[-comp,]}
    }
    
    #2. If the data was not analyzed using the bayesian model: semi-quantitative data such as awns, color, curve --> use Wilcoxon-Mann-Whitney test to compare selection vs bulk-----------
    if(class(donnees) == "data.frame"){
      if (!(variable %in% names(donnees))){warning("Variable must be one of donnees's names") ; return(list("histo" = NULL, "tab" = NULL))}
      result = apply(data,1,FUN=WMW, donnees, variable)
    }
    
    
    #3. Calculations ----------------
    if(!is.null(result)){
      result = t(result)
      colnames(result) = c("MoyenneVrac","MoyenneBouquet","pvalue")
      Data=cbind(data,result)
      if(class(donnees) == "list"){Data$overyielding = Data$MoyenneBouquet/Data$MoyenneVrac-1}
      if(class(donnees) == "data.frame"){Data$overyielding = Data$MoyenneBouquet-Data$MoyenneVrac}
      
      Data = Data[!is.na(Data$overyielding),]
      Data=Data[Data$modalite != "",]
      pval= NULL
      Data[is.na(Data$pvalue),"pvalue"] = 1
      if(language == "french"){niveaux = c("Composante, non significatif (pvalue > 0.05)","Composante, significatif à 0.05","Composante, significatif à 0.01",
                                           "Mélange, non significatif (pvalue > 0.05)","Mélange, significatif à 0.05","Mélange, significatif à 0.01")}
      if(language == "english"){niveaux=c("Component, not significant (pvalue > 0.05)","Component, significant at 0.05","Component, significant at 0.01",
                                          "Mixture, not significant (pvalue > 0.05)","Mixture, significant at 0.05","Mixture, significant at 0.01")}
      for (i in 1:nrow(Data)){
        if (Data[i,"type"] == "Composante" & as.numeric(as.character(Data[i,"pvalue"])) <= 0.01){
          if(language == "english"){pval = c(pval,"Component, significant at 0.01")}else{pval = c(pval,"Composante, significatif à 0.01")}}
        if (Data[i,"type"] == "Composante" & as.numeric(as.character(Data[i,"pvalue"])) <= 0.05 & as.numeric(as.character(Data[i,"pvalue"])) > 0.01 ){
          if(language == "english"){pval = c(pval,"Component, significant at 0.05")}else{pval = c(pval,"Composante, significatif à 0.05")}}
        if (Data[i,"type"] == "Composante" & as.numeric(as.character(Data[i,"pvalue"])) > 0.05){
          if(language == "english"){pval = c(pval,"Component, not significant (pvalue > 0.05)")}else{pval = c(pval,"Composante, non significatif (pvalue > 0.05)")}}
        if (Data[i,"type"] == "Mélange" & as.numeric(as.character(Data[i,"pvalue"])) <= 0.01){
          if(language == "english"){pval = c(pval,"Mixture, significant at 0.01")}else{pval = c(pval,"Mélange, significatif à 0.01")}}
        if (Data[i,"type"] == "Mélange" & as.numeric(as.character(Data[i,"pvalue"])) <= 0.05 & as.numeric(as.character(Data[i,"pvalue"])) > 0.01 ){
          if(language == "english"){pval = c(pval,"Mixture, significant at 0.05")}else{pval = c(pval,"Mélange, significatif à 0.05")}}
        if (Data[i,"type"] == "Mélange" & as.numeric(as.character(Data[i,"pvalue"])) > 0.05){
          if(language == "english"){pval = c(pval,"Mixture, not significant (pvalue > 0.05)")}else{pval = c(pval,"Mélange, non significatif (pvalue > 0.05)")}}
      }
      
      if(empile == "modalite"){
        Mean=unlist(lapply(levels(as.factor(Data$modalite)),function(x){mean(Data[Data$modalite %in% x,"overyielding"])}))
        names(Mean) = levels(as.factor(Data$modalite))
      }
      if(empile == "type"){
        Mean=unlist(lapply(levels(as.factor(Data$type)),function(x){mean(Data[Data$type %in% x,"overyielding"])}))
        names(Mean) = levels(as.factor(Data$type))
      }
      
      MeanGlob = mean(Data$overyielding)
      
      Data$pval = factor(pval,levels=niveaux)
      
      mel = Data[Data$type %in% "Mélange", "overyielding"]
      names(mel) = Data[Data$type %in% "Mélange", "germplasm"]
      
      #3. Test to check if overyielding is different from zero ---------
      if (empile == FALSE){
        if(shapiro.test(Data$overyielding)$p.value <= 0.05){Signif = t.test(Data$overyielding, mu=0)$p.value }else{ Signif = wilcox.test(Data$overyielding, mu=0)$p.value}
      }
      if (empile == "modalite"){
        Signif = unlist(lapply(unique(Data$modalite),function(x){
          if(length(Data[Data$modalite %in% x,"modalite"]) < 3){
            Signif=1
          }else{
            if (length(unique(Data[Data$modalite %in% x,"overyielding"])) == 1){
              if(mean(Data[Data$modalite %in% x,"overyielding"]) == 0){Signif = 1}else{Signif = 0}
            }else{
              if(shapiro.test(Data[Data$modalite %in% x,"overyielding"])$p.value <= 0.05){ 
                Signif = t.test(Data[Data$modalite %in% x,"overyielding"], mu=0)$p.value
              }else{  Signif = wilcox.test(Data[Data$modalite %in% x,"overyielding"], mu=0)$p.value}
            }
            
          }
          
          return(Signif)
        }))
        names(Signif)=unique(Data$modalite)
        Data$modalite = unlist(lapply(Data$modalite, function(x){paste(x,ifelse(language=="english","- Mean gain: "," - Gain moyen : "),round((Mean[x])*100,2)," % ",get_stars(Signif[x])," (n = ",nrow(Data[Data$modalite %in% x,])," )", sep="")}))
        
      }
      if (empile == "type"){
        Signif = unlist(lapply(unique(Data$type),function(x){
          if(length(Data[Data$type %in% x,"type"]) < 3){
            Signif=1
          }else{
            if (length(unique(Data[Data$type %in% x,"overyielding"])) == 1){
              if(mean(Data[Data$type %in% x,"overyielding"]) == 0){Signif = 1}else{Signif = 0}
            }else{
              if(shapiro.test(Data[Data$type %in% x,"overyielding"])$p.value <= 0.05){ 
                Signif = t.test(Data[Data$type %in% x,"overyielding"], mu=0)$p.value
              }else{  Signif = wilcox.test(Data[Data$type %in% x,"overyielding"], mu=0)$p.value}
            }
            
          }
          
          return(Signif)
        }))
        names(Signif)=unique(Data$type)
        Data$type = unlist(lapply(Data$type, function(x){paste(x,ifelse(language=="english","- Mean gain: "," - Différentiel moyen : "),round((Mean[x])*100,2)," % ",get_stars(Signif[x])," (n = ",nrow(Data[Data$type %in% x,])," )", sep="")}))
        
      }
      
      
      #4. Histogram ---------
      From= min(Data$overyielding)-0.2*abs(min(Data$overyielding))
      To = max(Data$overyielding) + 0.2*max(Data$overyielding)
      By = (max(Data$overyielding)-min(Data$overyielding))/12
      if (language == "english"){factoL = c("Component, not significant (pvalue > 0.05)","Component, significant at 0.05","Component, significant at 0.01",
                                            "Mixture, not significant (pvalue > 0.05)","Mixture, significant at 0.05","Mixture, significant at 0.01")
      }else{factoL = c("Composante, non significatif (pvalue > 0.05)","Composante, significatif à 0.05","Composante, significatif à 0.01",
                       "Mélange, non significatif (pvalue > 0.05)","Mélange, significatif à 0.05","Mélange, significatif à 0.01")}
      if(nrow(Data) ==1){p = ggplot(ylim = c(0,1))
      
      }else{ 
        p =  ggplot(data=Data,aes(as.numeric(as.character(overyielding)),fill=pval)) +
          geom_histogram(breaks = seq(From,To,By), color = "black")
        p = p +  scale_fill_manual(values = c("lightpink1","tomato1","goldenrod","skyblue1","seagreen2","mediumorchid2"), name=" ")
      }
      
      if (empile == "modalite"){ p = p + facet_wrap( ~ modalite, ncol =1, scales="free_y") + theme(strip.text.x = element_text(size=9))}
      if (empile == "type"){ p = p + facet_wrap( ~ type, ncol =1, scales="free_y") + theme(strip.text.x = element_text(size=9))} 
      p = p + ggtitle(titre)
      if (empile == FALSE) {
        p = p + geom_vline(xintercept = MeanGlob, size = 1.2, color="red")
      }
      if(class(donnees) == "list"){
        xlabel = ifelse(language == "english",paste("Normalised difference between selected and non-selected bulk, ",variable,sep=""),paste("Différence normalisée entre bouquet de sélection et vrac, ",variable,sep=""))
      }
      if(class(donnees) == "data.frame"){
        xlabel = ifelse(language == "english",paste("Difference between selected and non-selected bulk, ",variable,sep=""),paste("Différence entre bouquet de sélection et vrac, ",variable,sep=""))
      }
      p = p + labs(x=xlabel,
                   y=ifelse(language == "english","Number of comparisons selected vs non-selected bulk","Nombre de couples Bouquet - Vrac")) 
      p = p + geom_vline(xintercept = 0,  linetype = "dotted")
      # p = p + scale_fill_discrete(name = "")
      p = p + theme(legend.text = element_text(size = 7), axis.title = element_text(size = 10))
      
      if (empile==F){p = p + annotate("text",label = c(paste("n :",nrow(Data),sep=" "),
                                                       paste(ifelse(language == "english","Mean gain = ","Gain moyen = "),round(MeanGlob*100,2),"% (",get_stars(Signif),")",sep="")),x=(max(Data$overyielding)-0.2*max(Data$overyielding)),y=c(nrow(Data)/5,(nrow(Data)/5-nrow(Data)/50)),
                                      size=3.5)
      }
      
      
      return(list("histo" = p, "tab" = Data))
    }else{return(list("histo" = NULL, "tab" = NULL))}
    
  }
  
  get_selection_differential = function(donnees,data_S,vec_variables,language="english",year, table.save){
    
    data_version = get.data.version(data_S,language)
    data_version = data_version[unlist(lapply(as.character(data_version$version), function(x){strsplit(x,":")[[1]][2]})) %in% "vracS",]
    data_version = data_version[data_version$year %in% year,]
    
    if(language == "french"){i=2}
    if(language == "english"){i=3}
    SelectionTot_type = lapply(vec_variables,function(variable){go.analyse.selection(donnees,data_version,variable,titre=paste(list_trad[[grep("Réseau",list_trad)]][i]," (",paste(year,collapse=", "),") : ",list_trad[[grep(variable,list_trad)]][i],sep=""), 
                                                                                     empile="type",language)})
    
    SelectionTot_moda = lapply(vec_variables,function(variable){go.analyse.selection(donnees,data_version,variable,titre=paste(list_trad[[grep("Réseau",list_trad)]][i]," (",paste(year,collapse=", "),") : ",list_trad[[grep(variable,list_trad)]][i],sep=""), 
                                                                                     empile="modalite",language)})
   
    SelectionTot = lapply(vec_variables,function(variable){go.analyse.selection(donnees,data_version,variable,titre=paste("Total : ",list_trad[[grep(variable,list_trad)]][i],sep=""), empile=FALSE,language="french")})
    if(!is.null(table.save)){lapply(1:length(vec_variables),function(x){
      if(!is.null(SelectionTot[[x]]$tab)){write.table(SelectionTot[[x]]$tab, file = paste(table.save,"/DifferentielSelection_",vec_variables[x],"_",paste(year,collapse="-"),".csv",sep=""),
                                                                                    sep=";",dec=".")}})}
    
    if(FALSE){
      # Pour les mélanges
      data_mel = data_version[data_version$type %in% "Mélange",]
      Selection3 = lapply(vec_variables,function(variable){go.analyse.selection(donnees,data_mel,variable,person="Mélange", nom = "", empile=FALSE,language)})
      lapply(1:length(vec_variables),function(x){write.table(Selection3[[x]]$tab, file = paste("./donnees brutes/DifferentielSelectionMelange_",vec_variables[x],"_",paste(year,collapse="-"),".csv",sep=""))})
      
      # Pour les composantes : Modalité 1
      data_mod1 = data_version[data_version$modalite %in% c("Composantes : Mod 1","Within components: M1"),]
      Selection1 = lapply(vec_variables,function(variable){go.analyse.selection(donnees,data_mod1,variable,person="Modalité 1", nom = "", empile=FALSE,language)})
      lapply(1:length(vec_variables),function(x){write.table(Selection1[[x]]$tab, file = paste("./donnees brutes/DifferentielSelectionModalite1_",vec_variables[x],"_",paste(year,collapse="-"),".csv",sep=""))})
      
      # Pour les composantes : Modalité 2
      data_mod2 = data_version[data_version$modalite %in% c("Composantes : Mod 2","Within components: M2"),]
      Selection2 = lapply(vec_variables,function(variable){go.analyse.selection(donnees,data_mod2,variable,person="Modalité 2", nom = "", empile=FALSE,language)})
      lapply(1:length(vec_variables),function(x){write.table(Selection2[[x]]$tab, file = paste("./donnees brutes/DifferentielSelectionModalite2_",vec_variables[x],"_",paste(year,collapse="-"),".csv",sep=""))})
      
      return(list("Tot1"=SelectionTot1,"Tot"=SelectionTot,"Mod3"=Selection3,"Mod1"=Selection1,"Mod2"=Selection2))
    }
    return(list("Tot_type"=SelectionTot_type,"Tot_moda"=SelectionTot_moda,"Tot"=SelectionTot))
  }
  
#0.2. 
  if(is.null(year)){year = seq("2016",strsplit(as.character(Sys.Date()),"-")[[1]][1],1)}
  if(!is.null(data_mixtures$Mix_tot)){Melanges = unique(data_mixtures$Mix_tot$data$data$son_germplasm)}
  
#1. analysis
  paysans = unique(Mixtures$location)
  paysans = paysans[which(!(paysans %in% c("ADP","HEL")))]
 
if(FALSE){  # à faire ...
  #1.1. Par mélange ----------------
  if(type=="mixtures"){
    
    bp = lapply(vec_variables,function(variable){
      lapply(paysans,function(x){Histo(x,variable,language)})
    })
    names(bp) = vec_variables
    
    for (i in 1:length(bp)){
      var = names(bp)[i]

##faire une boucle sur les variables !
      pdf(paste(plot.save,"/DifferentielSelectionPaysans&melanges_",var,"_",language,".pdf",sep=""))
          lapply(list(bp[[1]]),function(x){return(
            lapply(x,function(y){return(
              lapply(y,function(z){
                if(names(z)[1] == "histo"){ if(is.data.frame(z$histo$data)){return(z$histo)}}else{return(lapply(z,function(zz){
                  if(is.data.frame(zz$histo$data)){return(zz$histo)}}))}
              }))}))})
          dev.off()
   
  }
  
  }
}
  
#1.2 Selection differential on the network
if(selection.type == "sel.diff.network" | selection.type == "diff.and.rep"){
  if(!is.null(table.save) & !dir.exists(file.path(table.save, "Diff_Sel"))){system(paste("mkdir ",table.save,"/Diff_Sel",sep="")) ; message("dir Diff_Sel have been created, tables dealing with selection differential will be saved there")}
  DS = lapply(vec_variables, function(variable){
    if(variable %in% names(res_model1)){
      get_selection_differential(donnees = res_model1, data_S = data_S_all, vec_variables = variable , language, year, table.save = paste(table.save,"Diff_Sel",sep="/"))
    }else{
      get_selection_differential(donnees = data_S_all$data$data, data_S = data_S_all, vec_variables = variable , language, year, table.save = paste(table.save,"Diff_Sel",sep="/"))
    }
  })
  names(DS)=vec_variables
  
  
  if(!is.null(plot.save)){
    pdf(file=paste(plot.save,"/DifferentielSelectionReseau-",language,"_",paste(year,collapse="-"),".pdf",sep=""))
    print(DS)
    dev.off()
    
    lapply(1:length(DS),function(i){
      y=DS[[i]]
      png(file=paste(plot.save,"/png/DifferentielSelectionReseau-",language,paste(year,collapse="-"),"_type_",names(DS)[i],".png",sep=""))
      print(y[[1]])
      dev.off()
      
    })
  }
  if(selection.type == "sel.diff.network"){return(DS)}
}

 
if(selection.type == "response.sel.mixture" | selection.type == "diff.and.rep"){
  variables_mod1 = vec_variables[which(vec_variables %in% names(res_model1))]
  variables_semiquanti = setdiff(vec_variables,names(res_model1))
  RS=list()
  
  if(!is.null(data_SR_all)){
    D = data_SR_all$data$data
    D=D[grep("(R)",D$expe_name_2),]
    D=D[D$sl_statut %in% "2017:bouquetR",]
    D$sel_name = unlist(lapply(as.character(D$son),function(x){
      strsplit(strsplit(x,"_")[[1]][1],"#")[[1]][2]
    }))
    D=D[grep("JA|VA",D$expe_name_2),]  # on perd le vrac...!
    
    # fonction compare_model
    D$vrac = unlist(lapply(as.character(D$expe_name_2),function(x){strsplit(x," | ")[[1]][1]}))
    D$vrac = unlist(lapply(as.character(D$vrac),function(x){
      a = strsplit(x,"_")[[1]]
      return(paste("mu[",a[[1]],",",a[2],":",a[3],"]",sep=""))
    }))
    
    D$bouquet = unlist(lapply(as.character(D$expe_name_2),function(x){strsplit(x," | ")[[1]][3]}))
    D$bouquet = unlist(lapply(as.character(D$bouquet),function(x){
      a = strsplit(x,"_")[[1]]
      return(paste("mu[",a[[1]],",",a[2],":",a[3],"]",sep=""))
    }))
    data_SR = D[,c("vrac","bouquet","sl_statut","expe_name")]
    data_SR = unique(data_SR)
    colnames(data_SR)[grep("expe_name",colnames(data_SR))] = "group"
  }else{data_SR=NULL}

  
  if(length(variables_mod1)>0){
    if(!is.null(table.save) & !dir.exists(file.path(table.save, "Rep_Sel"))){system(paste("mkdir ",table.save,"/Rep_Sel",sep="")) ; message("dir Rep_Sel have been created, tables dealing with response to selection will be saved there")}
    person=as.character(na.omit(unique(data_mixtures$Mixtures_all$data$son_person)))
    RS_mod1=lapply(variables_mod1,function(variable){
      p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = data_mixtures$Mixtures_all, data_S = data_mixtures$Mixtures_selection, melanges_tot = data_mixtures$Mix_tot, variable, 
                                   year=year, model = "model_1", plot.type = "comp.mod.network", person=NULL, nb_parameters_per_plot = 20, save=NULL, language=language)$Tab
      p_melanges = cbind(p_melanges,rep("melange",nrow(p_melanges)))
      colnames(p_melanges)[ncol(p_melanges)]="type"
      if(!is.null(data_SR)){
        result = apply(data_SR,1,FUN=compare_model,donnees=res_model1, variable=variable)
        if(class(result) == "list"){Res=NULL; comp = NULL; for (i in 1:length(result)){if(!is.null(result[[i]])){Res=cbind(Res,result[[i]])}else{comp = c(comp,i)}}; result=Res; data_SR=data_SR[-comp,]}
        result = t(result)
        colnames(result) = c("MoyenneVrac","MoyenneBouquet","pvalue")
        Data = cbind(paste(data_SR$vrac,data_SR$bouquet,sep="-"),result)
        Data=as.data.frame(Data)
        Data$overyielding = as.numeric(as.character(Data$MoyenneBouquet))/as.numeric(as.character(Data$MoyenneVrac))-1
        if(!is.null(table.save) & !is.null(Data)){write.table(Data, file=paste(table.save,"/Rep_Sel/Mod1_",variable,"-2017.csv",sep=""),sep=";")}
        Data = Data[!is.na(Data$overyielding),]
        Data$type="composante"
        Data = cbind(Data$overyielding,rep(NA,nrow(Data)),rep(NA,nrow(Data)),rep(NA,nrow(Data)),Data$type)
        colnames(Data)=colnames(p_melanges)
        rownames(Data) = paste(
          unlist(lapply(as.character(data_SR$group),function(x){strsplit(x," ")[[1]][1]})),
          " | ",
          unlist(lapply(as.character(data_SR$group),function(x){strsplit(x," ")[[1]][3]})),sep=""
        )
        p_melanges = rbind(p_melanges,Data)
      }
      
      if(!is.null(table.save) & !is.null(p_melanges)){write.table(p_melanges,file=paste(table.save,"/Rep_Sel/sel_response_",variable,"_",paste(year,collapse="-"),".csv",sep=""),sep=";",dec=".")}
      return(p_melanges)
    })
    names(RS_mod1)=variables_mod1
    RS=c(RS,RS_mod1)
  }
  if(length(variables_semiquanti)>0){
    if(!is.null(table.save) & !dir.exists(file.path(table.save, "Rep_Sel"))){system(paste("mkdir ",table.save,"/Rep_Sel",sep="")) ; message("dir Rep_Sel have been created, tables dealing with response to selection will be saved there")}
    RSQ = lapply(variables_semiquanti,function(variable){
      d_env =  plyr:::splitter_d(Mixtures_all$data, .(location))
      d_env_b = lapply(d_env,function(pers){
        x = plyr:::splitter_d(pers, .(expe_melange))
        x = x[names(x)%in%Melanges]
        lapply(x,function(y){
          M = unique(data_mixtures$Mix_tot$data$data[data_mixtures$Mix_tot$data$data$son_germplasm %in% unique(y$son_germplasm),c("son","son_year","son_germplasm","father","father_germplasm","selection_id","block","X","Y")])
          if(nrow(M)>0){
            M = M[is.na(M$selection_id) & M$son_year %in% year,]
            M$modalite = unlist(lapply(1:nrow(M),function(i){
              d=M[i,]
              comp=0
              if(length(grep("[.]3",d$son))>0){return("Mod1") ; comp=1}
              if(length(grep("[.]2",d$son))>0){return("Mod2") ; comp=1}
              if(length(grep("#B",d$son))>0){return("Mod3") ; comp=1}
              if(comp==0){return("Mod4")}
            }))
            M$germplasm = unlist(lapply(as.character(M$son),function(x){strsplit(x,"_")[[1]][1]}))
            M = M[!duplicated(M[c("germplasm","son_year")]),]
            M =  plyr:::splitter_d(M, .(son_year))
            
            y = lapply(year,function(yr){
              m = M[yr][[1]]
              if(!is.null(m)){
                A = unlist(lapply(1:nrow(m),function(i){
                  d=m[i,]
                  Mod = d$modalite
                  if(d$modalite != "Mod4"){
                    a = strsplit(as.character(d$son),"_")[[1]]
                    if(length(grep("Mod4",m$modalite))>0){
                      b = strsplit(as.character(m[grep("Mod4",m$modalite),"son"]),"_")[[1]]
                      sel = data_mixtures$Mix_tot$data$data[grep(paste(a[1],"_",a[2],"_",yr,sep=""), data_mixtures$Mix_tot$data$data$son),list_trad[[grep(variable,list_trad)]][1]]  ;  sel=sel[!is.na(sel)]
                      no_sel = data_mixtures$Mix_tot$data$data[grep(paste(b[1],"_",b[2],"_",yr,sep=""), data_mixtures$Mix_tot$data$data$son),list_trad[[grep(variable,list_trad)]][1]] ;  no_sel=no_sel[!is.na(no_sel)]
                      overY = (mean(as.numeric(as.character(sel))) - mean(as.numeric(as.character(no_sel)))) / mean(as.numeric(as.character(no_sel)))
                    }else{overY = NA}
                    
                    names(overY) = Mod
                    if(Mod == "Mod2"){
                      if(length(grep("Mod3",m$modalite))>0){
                        b = strsplit(as.character(m[grep("Mod3",m$modalite),"son"]),"_")[[1]]
                        m2 = data_mixtures$Mix_tot$data$data[grep(paste(a[1],"_",a[2],"_",yr,sep=""), data_mixtures$Mix_tot$data$data$son),list_trad[[grep(variable,list_trad)]][1]]  ;  m2=m2[!is.na(m2)]
                        m3 = data_mixtures$Mix_tot$data$data[grep(paste(b[1],"_",b[2],"_",yr,sep=""), data_mixtures$Mix_tot$data$data$son),list_trad[[grep(variable,list_trad)]][1]] ;  m3=m3[!is.na(m3)]
                        overY = c(overY,(mean(as.numeric(as.character(m3))) - mean(as.numeric(as.character(m2)))) / mean(as.numeric(as.character(m2))))
                        names(overY)[2]="Mod3vsMod2"
                      }
                    }
                    return(overY)
                  }else{return(NA)}
                }))
                A=A[!is.na(A)]
                if(length(A)>0){
                  if(length(grep("Mod4",m$modalite))>0){A=c(m[grep("Mod4",m$modalite),"germplasm"],A)
                  }else{A=c(strsplit(as.character(m[grep("Mod3",m$modalite),"germplasm"]),"#")[[1]][1],A)}
                  names(A)[1]="melange"
                  if(!("Mod1"%in%names(A))){A=c(A,"Mod1"=NA)}
                  if(!("Mod2"%in%names(A))){A=c(A,"Mod2"=NA)}
                  if(!("Mod3"%in%names(A))){A=c(A,"Mod3"=NA)}
                  if(!("Mod3vsMod2"%in%names(A))){A=c(A,"Mod3vsMod2"=NA)}
                }else{A=NULL}
                return(A)
              }else{return(NULL)}
            })
            names(y) = year
            a = unlist(lapply(y,function(x){length(x)>0}))
            y=y[a]
            if(length(y)>0){
              A=NULL
              for(i in 1:length(y)){A = rbind(A,y[[i]])}
            }else{A=NULL}
          }else{A=NULL}
          return(A)
        })
      })
      Tab=NULL
      for(pers in d_env_b){
        for(mel in pers){
          if(!is.null(mel)){Tab = rbind(Tab,mel)}
        }
      }
      if(!is.null(Tab)){
        rownames(Tab) = Tab[,"melange"]
        Tab=Tab[,-grep("melange",colnames(Tab))]
        Tab = cbind(Tab,"melange")
        colnames(Tab)[ncol(Tab)] = "type"
        
        result = apply(data_SR,1,FUN=WMW, donnees=data_SR_all$data$data, list_trad[[grep(variable,list_trad)]][1])
        result = t(result)
        colnames(result) = c("MoyenneVrac","MoyenneBouquet","pvalue")
        Data=result
        Data=as.data.frame(Data)
        rownames(Data) = paste(
          unlist(lapply(as.character(data_SR$group),function(x){strsplit(x," ")[[1]][1]})),
          " | ",
          unlist(lapply(as.character(data_SR$group),function(x){strsplit(x," ")[[1]][3]})),sep=""
        )
        Data$overyielding = as.numeric(as.character(Data$MoyenneBouquet)) - as.numeric(as.character(Data$MoyenneVrac))
        
        if(!is.null(table.save) & !is.null(Data)){write.table(Data, file=paste(table.save,"/Rep_Sel/Mod1_",variable,"-2017.csv",sep=""),sep=";")}
        Data = Data[!is.na(Data$overyielding),]
        Data$type="composante"
        name = rownames(Data)
        Data = cbind(rep(NA,nrow(Data)),rep(NA,nrow(Data)),rep(NA,nrow(Data)),Data$overyielding,Data$type)
        colnames(Data)=colnames(Tab)
        rownames(Data) = name
        
        Tab = rbind(Tab,Data)
        
        if(!is.null(table.save) & !is.null(Tab)){write.table(Tab,file=paste(table.save,"/Rep_Sel/sel_response_",variable,"_",paste(year,collapse="-"),".csv",sep=""),sep=";")}
        return(Tab)
      }else{return(NULL)}
    
    })
    names(RSQ) = variables_semiquanti
    RS=c(RS,RSQ)
  }
  if(selection.type == "response.sel.mixture"){return(RS)}
}
 
  
if(selection.type == "diff.and.rep"){return(list(DS,RS))}
  

}
