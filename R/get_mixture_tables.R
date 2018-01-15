# 0. help -----------------------------------------------------------------
#' Function to get tables containing results from mixture experiment
#' 
#' @param res_model results from PPBstats models
#' 
#' @param table.type type of table to be returned
#' \itemize{
#'  \item \code{global.info} to get a table containing 
#'  \item \code{distribution} to get a table containing :
#'  \itemize{
#'     \item the number of mixtures tested
#'     \iem the proportion of mixtures which value exceeds the predicted value based on the mean of components' values
#'     \item the number of mixtures for which the mixture significantly exceeds the predicted value (alpha = 0.05)
#'     \item the proportion of mixtures which value is lower than its lowest component
#'     \item the proportion of mixtures which value is higher than its highest component
#'     \item mean value of all components
#'     \item mean value of all mixtures
#'     \item mean overyielding over all the mixtures tested. The overyielding is calculated as : 
#'     \deqn{\frac{1}{N} \sum\limits_{i=1}^N \frac{\overline{Y_{Mixture_{i}}} - \overline{Y_{Mean of components_{i}}}}{\overline{Y_{Mean of components_{i}}}} }
#'     \item standard deviation of overyieldings
#'     \item pvalue of the comparison test mean overyielding vs 0
#'  }
#'  \item \code{correlations} to get a table containing:
#'  \itemize{
#'    \item the correlation of the overyielding with: the number of components in the mixture and the variance observed among components
#'    \item the pvalue of the correlation
#'  }
#'  \item \code{selection.modalities} (focusing on phenotypic mean) and \code{varIntra} (focusing on phenotypic variability) to get tables containing
#'  the comparison of the selected mixture and the non-selected mixture for all selection modalities, and the comparison between the different selection modalities:
#'    \itemize{
#'       \item mean value of the differences over all the mixtures tested
#'       \item standard deviation of differences
#'       \item statistic value for the comparison test (t.test if normal distribution of values, wilcox.test otherwise)
#'       \item pvalue of the comparison test
#'       \item significance symbol
#'       \item test used for the comparison
#'    }
#'  
#' }
#' 
#' @param res_model_varintra if 
#' 
#' @param year for \code{table.type = 'distribution'} and \code{table.type = 'correlations'}, a character vector containing the years to be analyzed. If NULL, all years from 2015 will be analyzed.
#' 
#' @param year_DS for \code{table.type = 'selection.modalities'), a character vector containing the years for which the selection differential has to be analyzed. If NULL, all years from 2015 will be analyzed.
#' 
#' @param year_RS for \code{table.type = 'selection.modalities'), a character vector containing the years for which the response to selection has to be analyzed. If NULL, all years from 2015 will be analyzed.
#' 
#' @param mix_to_delete names of the mixtures that should not be analyzed (default is NULL).
#' 
#' @param language \code{"french"} or \code{"english"}: the language in which the tables must be returned
#' 
#' @param data_mixtures result from \code{\link{analyse_feedback_folder_1}
#' 
#' @param vec_variables the variables to be analyzed
#' 
#' @param data_S_all result from \code{\link{analyse_feedback_folder_1}
#' 
#' @param data_SR_all result from \code{\link{analyse_feedback_folder_1}
#' 
#' @param path_to_tables the path to were the tables should be saved (current directory as default)
#' 
#' @param tab_proportions if \code{table.type = 'correlations'}, a table containing the proportion of each component in each mixtures for the calculation of weighted variability among components.
#' If NULL, components will be considered as equally abondant in mixtures.
#' 

#
#' @details=
#' 
#' @return Generate tex and pdf files
#' 
#' @author Pierre Rivière, Gaelle Van Frank
#' 



get_mixture_tables <- function(res_model,
                               res_model_varintra = NULL,
                               year=NULL,
                               year_DS=NULL,
                               year_RS=NULL,
                               mix_to_delete=NULL,
                               language="english",
                               data_mixtures,
                               vec_variables, 
                               data_S_all=NULL, 
                               data_SR_all=NULL, 
                               path_to_tables = ".",
                               list_trad=NULL,
                               table.type="distribution",
                               tab_proportions = NULL
                               )
{

  library(Hmisc)
  Mixtures = data_mixtures$Mixtures_all$data
  melanges_tot=data_mixtures$Mix_tot$data$data
  if(!is.null(data_S_all)){  
    data_S = data_S_all$data$data
    data_S = unique(data_S[,c("son","expe","sl_statut","expe_name","expe_name_2","son_germplasm","father","father_germplasm","son_person")])
    data_S = data_S[grep("bouquet",data_S$sl_statut),]
  }
  Melanges = unique(melanges_tot$son_germplasm)

#0.1 If no year, take all years
  if(is.null(year)){year = as.character(seq("2016",strsplit(as.character(Sys.Date()),"-")[[1]][1],1))}
  if(is.null(year_DS)){year_DS = as.character(seq("2016",strsplit(as.character(Sys.Date()),"-")[[1]][1],1))}
  if(is.null(year_RS)){year_RS = as.character(seq("2016",strsplit(as.character(Sys.Date()),"-")[[1]][1],1))}
  
#0. functions
  get.data.version <- function(data_S_all,language){
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
  
  go.graph.sel <- function(res_model1, data_S_year, y, variable, person, empile, sep.by, nom,language){
    D=data_S_year$data$data 
    
    if(nrow(D) >0){
      D = D[unlist(lapply(D$father, function(x){strsplit(as.character(x),"_")[[1]][1]})) %in% unlist(lapply(y$son, function(x){strsplit(as.character(x),"_")[[1]][1]})),]
      data_S_year$data$data = D
      
      if(!is.null(data_S_year$data)){
        data_version = format.data(data_S_year, data.on = "son", fuse_g_and_s = TRUE, format = "PPBstats")
        colnames(y)[colnames(y) %in% "germplasm_son"]="germplasm"
        if (sep.by == "Mélange") {
          y$modalite = unlist(lapply(y$expe, function(x){return(y[y$expe %in% x & y$sl_statut %in% "son","germplasm"][1])}))
          z=y[,c("germplasm","modalite")]
          data_version = merge(z,data_version,by="germplasm")
        }
        if (sep.by == "Modalite") {
          data_version = get.data.version(data_S_year, language)
          data_version = data_version[unlist(lapply(as.character(data_version$version), function(x){strsplit(x,":")[[1]][2]})) %in% "vracS",]
        }
        
        data_version$type = ifelse(unlist(lapply(as.character(data_version$group),function(x) {return(strsplit(x," ")[[1]][1])})) %in% y$son_germplasm, "Mélange","Composante")
        
        if(variable %in% names(res_model1)){ return(analyse.selection(res_model1,data_version,variable,person, empile,nom,language))
        }else{return(analyse.selection(D,data_version,variable,person, empile,nom,language))}
        
      } # end if
    } # end for
  } # end function do.graph
  
  Histo <- function(res_model1, data_S_year, person, variable,language){
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
  
  get.gain <- function(tab,to_split=NULL,col=NULL){
    tab = as.data.frame(tab)
    if(length(na.omit(tab[,col]))==0){
      return(rep(NA,7))
    }else{
      if(!is.null(to_split)){
        tab$split = as.numeric(as.factor(tab[,to_split]))
      }else{
        tab$split = 1
      }
      tab = plyr:::splitter_d(tab, .(split))
      
      a = lapply(tab,function(d){
        if(length(na.omit(d[,col]))>2){
          if(shapiro.test(d[,col])$p.value<=0.05){
            a = t.test(d[,col],mu=0)
            type = "t.test"
          }else{
            a = wilcox.test(d[,col],mu=0)
            type="wilcox.test"
          }
          return(c("mean"=round(100*mean(na.omit(d[,col])),2),"sd"=round(100*sd(na.omit(d[,col])),2),"statistic" = a$statistic,"pvalue"=a$p.value, "stars"=get_stars(a$p.value), "test"=type, "n"=length(na.omit(d[,col]))))
        }else{return(rep(NA,7))}
        })
      Res=NULL
      for (i in a){Res=rbind(Res,i)}
      rownames(Res)=unlist(lapply(tab,function(x){unique(x[,to_split])}))
      return(Res)
    }
  }
  

# 1.Global information on mixtures
if(table.type == "global.info"){
 a = melanges_tot[!is.na(melanges_tot[,grep(vec_variables,colnames(melanges_tot))]),]
 Mix = Mixtures[Mixtures$son_germplasm %in% unique(a$son_germplasm),]
 M1 =  plyr:::splitter_d(Mix, .(expe_melange))  
 # Number of components for each mixture
 Nb_comp = unlist(lapply(M1,function(M){
   M=M[M$germplasm_father != M$germplasm_son,]
   M=M[M$germplasm_son == M$expe_melange,]
   return(nrow(M))
 }))
 
 # Number of locations per year
 M2 =  plyr:::splitter_d(Mix, .(year))
 Nb_location = lapply(M2,function(yr){
   return(unique(yr$location))
 })
 
 # Nombre de mélanges par ferme
 a = unique(Mix[,c("location","expe_melange")])
 a$expe_melange = 1
 b = by(a$expe_melange, a$location, sum)
 b = do.call(rbind, list(b))
 rownames(b) = "Nombre_melanges"
 return(list("Nb_comp"=Nb_comp,"Nb_fermes"=Nb_location,"Nb_mel"=b))
}
  
# 1. Distribution-------
if(table.type == "distribution"){
  D=list()
  for (i in vec_variables){
    if(!file.exists(paste(path_to_tables,"/Distrib_",i,".csv",sep=""))){
      p = ggplot_mixture1(res_model = res_model, melanges_PPB_mixture = data_mixtures$Mixtures_all, data_S = data_mixtures$Mixtures_selection, melanges_tot = data_mixtures$Mix_tot, i, 
                                   year=year, model="model_1", plot.type = "mix.gain.distribution", person=NULL, nb_parameters_per_plot = 15,
                                   save=path_to_tables)
    }
    d = read.table(paste(path_to_tables,"/Distrib_",i,".csv",sep=""),header=T,sep=";",comment.char="")
    if(!is.null(mix_to_delete)){if(length(grep(paste(mix_to_delete,collapse="|"),d$true_melange))>0){d = d[-grep(paste(mix_to_delete,collapse="|"),d$true_melange),]}}
    tmp=cbind(as.numeric(as.character(d[grep("Worst",d$parameter),"Moyenne"])),
              as.numeric(as.character(d[grep("MoyenneComposantes",d$parameter),"Moyenne"])),
              as.numeric(as.character(d[grep("Mélange",d$parameter),"Moyenne"])),
              as.numeric(as.character(d[grep("Best",d$parameter),"Moyenne"])),
              as.numeric(as.character(d[grep("MoyenneComposantes",d$parameter),"pval"])))
    
    
    tmp = cbind(tmp,unlist(as.character(d$melange[seq(1,nrow(d),4)])),unlist(as.character(d$year[seq(1,nrow(d),4)])),unlist(as.character(d$location[seq(1,nrow(d),4)])))
    tmp=data.frame(tmp,stringsAsFactors = F)
    colnames(tmp)=c("1.moins bonne","2.moyenne composantes","3.mélange","4.meilleure","pvalue_overY","melange","year","location")
    D=c(D,list(tmp))
  }
  names(D)=vec_variables
  
  Tab = NULL
  for(i in 1:length(D)){
    x=D[[i]]
    x$overyielding = (as.numeric(x$`3.mélange`) - as.numeric(x$`2.moyenne composantes`))/as.numeric(x$`2.moyenne composantes`)
    if(shapiro.test(as.numeric(as.character(x$overyielding)))$p.value <0.05){
      Signif = wilcox.test(as.numeric(as.character(x$overyielding)),mu=0)$p.value
    }else{
      Signif = t.test(as.numeric(as.character(x$overyielding)),mu=0)$p.value
    }
    a = c(nrow(x),round(sum(as.numeric(as.character(x$'3.mélange'))>as.numeric(as.character(x$'2.moyenne composantes')))*100/nrow(x),3), sum(as.numeric(as.character(x$pvalue_overY))<=0.05 & as.numeric(as.character(x$overyielding))>0),
          round(sum(as.numeric(as.character(x$'3.mélange'))<as.numeric(as.character(x$'1.moins bonne')))*100/nrow(x),3),
          round(sum(as.numeric(as.character(x$'3.mélange'))>as.numeric(as.character(x$'4.meilleure')))*100/nrow(x),3),
          mean(as.numeric(as.character(x$'2.moyenne composantes'))),mean(as.numeric(as.character(x$'3.mélange'))), mean(as.numeric(x$overyielding))*100,100*sd(as.numeric(x$overyielding)),Signif,get_stars(Signif))
    Tab=cbind(Tab,a)
  }
  
  colnames(Tab) = names(D)
  if(language=="english"){
    rownames(Tab) = c("Number of blends","Proportion blend > components' mean","number significant overyieldings","Proportion blend < lowest component","Proportion blend > highest component","Mean components","Mean blends","Mean overyielding","Std.Dev. overyielding","pvalue overyielding","stars")
  }else{
    rownames(Tab) = c("Nombre de mélanges","Proportion mélanges > moyenne des composantes","nombre de gains significatifs","Proportion mélanges < composante la plus basse","Proportion mélanges > composante la plus haute",
                      "Moyenne des composantes","Moyenne des mélanges","Gain moyen","Ec.Type gain","pvalue overyielding","stars")
    
  }
  return(Tab)

}

#2. corrélations ----------
if(table.type == "correlations"){

  a = lapply(vec_variables,function(variable){
    d_env = plyr:::splitter_d(Mixtures, .(location))
    d_env_b = lapply(d_env, function(x){
      # une table par mélange
      mix = plyr:::splitter_d(x, .(mixture_id))
      if(length(mix)==1){
        MIX=mix
      }else{
        MIX = list()
        for (i in 1:(length(mix)-1)) {
          Mel = mix[[i]]
          Comp = mix[[length(mix)]] 
          MIX = c(MIX,list(rbind(Mel, Comp[Comp$expe %in% Mel$mixture_id,])))
        }
      }
      paysan = unique(mix[[1]]$location)
      
      # récupérer les données (MCMC) pour chaque mixture et les splitter
      mix_split = lapply(MIX , function(y) {
        noms = as.data.frame(unique(y[,c("son","son_year","son_germplasm","father","father_germplasm","selection_id","block","X","Y","expe_melange")]),stringsAsFactors = FALSE)
        
        M = unique(melanges_tot[melanges_tot$son_germplasm %in% unique(x$son_germplasm),c("son","son_year","son_germplasm","father","father_germplasm","selection_id","block","X","Y")])
        M = M[is.na(M$selection_id) & M$son_year %in% year,]
        M$expe_melange = unlist(lapply(as.character(M$father_germplasm),function(x){
          a = strsplit(x,"-")[[1]]
          b = grep("[.]",a) ; c = grep("#",a)
          if(length(b)>0){ a[b] = strsplit(a[b],".",fixed=TRUE)[[1]][1]}
          if(length(c)>0){ a[c] = strsplit(a[c],".",fixed=TRUE)[[1]][1]}
          return(paste(a,collapse="-"))
        }))
        M = M[M$expe_melange %in% noms$expe_melange,]
        M = plyr:::splitter_d(M, .(son_year))
        M = lapply(M, function(m){
          a = noms[which(noms$son %in% m$father),]
          a$son = a$father
          return(rbind(m,a))
        })
        
        Donnees = lapply(M,function(M_year){
          M_year$Type = unlist(lapply(1:nrow(M_year),function(i){
            a = M_year[i,]
            if(as.character(a$son_germplasm)== as.character(a$father_germplasm)){return("Mélange")}else{return("Composante")}
          }))
          if(length(unique(M_year$son_germplasm == M_year$expe_melange))>1 | length(grep(FALSE,(unique(M_year$son_germplasm == M_year$expe_melange))))>0){
            nom_melange=data.frame(M_year[M_year$Type %in% "Mélange",],stringsAsFactors = FALSE)
            nom_melange$germplasm_2 = nom_melange[,1]
            
            noms=data.frame(M_year[M_year$Type %in% "Composante",],stringsAsFactors = FALSE)
            colnames(noms)[1] =  colnames(nom_melange)[1] ="germplasm"
            #       mel_year = strsplit(as.character(nom_melange$germplasm),"_")[[1]][3]
            mel_year = as.character(as.numeric(as.character(unique(nom_melange$son_year)))-1)
            noms$germplasm_2 = lapply(as.character(noms$germplasm),function(x){
              d = data_S[grep(strsplit(x,"#")[[1]][1],data_S$father),]
              d = d[d$son_person %in% paysan,]
              d$year = unlist(lapply(as.character(d$sl_statut),function(y){strsplit(y,":")[[1]][1]}))
              d=d[d$year %in% mel_year,]
              germ = d$son
              if(length(germ)>0){return(as.character(germ[grep("VA",germ)]))}else{return(x)}
            })
            noms = rbind(as.matrix(nom_melange),as.matrix(noms))
            noms = as.data.frame(noms)
          }else{
            noms=M_year
            colnames(noms)[1] = "germplasm"
            noms$germplasm_2 = noms$germplasm
          }
          
          if(length(unlist(noms$germplasm_2)) < nrow(noms)){noms$germplasm_2 = noms$germplasm}
          if(length(unlist(noms$germplasm_2)) == nrow(noms)){
            M_year$son = unlist(lapply(as.character(M_year$son),function(x){return(noms[which(noms$germplasm ==x),"germplasm_2"])}))
            
          }
          melange = unlist(unique(noms[noms$Type%in%"Composante","expe_melange"]))
          noms$son_germplasm = unlist(lapply(as.character(noms$germplasm_2),function(x){strsplit(x,"_")[[1]][1]}))
          noms$son_year = max(as.numeric(as.character(noms$son_year)))
          noms$son = paste(noms$son_germplasm, paysan, noms$son_year,"0001",sep="_")
          noms=noms[noms$expe_melange %in% melange,]
          return(noms)
        })
        
        res_year = lapply(year,function(yr){
          noms = Donnees[yr][[1]]
          if(!is.null(noms)){
            if(nrow(noms)>0){
              melange = noms$son_germplasm[1]
              mcmc = get_result_model(res_model, noms, type_result = "MCMC", variable, model="model_1",param = "mu", year = yr)
              Mel = mcmc[,unlist(rm_between(colnames(mcmc), "[", ",", extract=TRUE)) %in% noms[which(noms$Type == "Mélange"),"son_germplasm"]]
              
              if (length(Mel) > 0) {
                Comp = mcmc[,unlist(rm_between(colnames(mcmc), "[", ",", extract=TRUE)) %in% noms[which(noms$Type == "Composante"),"son_germplasm"]]
                if(!is.null(ncol(Comp))){if (ncol(Comp) < length(unique(noms[noms$Type == "Composante","son_germplasm"])) | length(noms[noms$Type == "Composante","Type"])==0){
                  missingComp = TRUE}else{missingComp=FALSE}}else{missingComp=TRUE}
                
                if(!missingComp){
                  MeanComp = apply(Comp, 1, mean)
                  M = cbind(Mel, MeanComp, Comp)
                  colnames(M)[colnames(M) %in% "MeanComp"] = paste("mu[","MoyenneComposantes",",",paysan,":",yr,"]",sep="")
                }else{return(NULL)}
                
                colnames(M)[colnames(M) %in% "Mel"] = paste("mu[", unique(noms[noms$Type %in% "Mélange","son_germplasm"]),",",paysan,":",yr,"]",sep="")
                M = apply(M,2,mean)
                return(list("Tab"=M,"Comp"=names(Comp)))
              }else{
                warning("No data for the mixture")
                return(list("Tab"=NULL,"Comp"=NULL))
              }
            }else{
              warning(paste("No data for ",yr,sep=""))
              return(list("Tab"=NULL,"Comp"=NULL))
            }
          }else{
            warning(paste("No data for ",yr,sep=""))
            return(list("Tab"=NULL,"Comp"=NULL))
          }
          
        }) # end lapply(yr)
        # ne garder l'overyielding que de la première année
        comp=NULL
        for (yr in 1:length(res_year)){if(!is.null(res_year[[yr]]$Tab) & is.null(comp)){comp=yr}}
        if(!is.null(comp)){if(comp < length(res_year)){for(i in (comp+1):length(res_year)){res_year[[i]]$Tab=NULL}}}
        return(res_year)
      }) # end lapply(y)
      return(mix_split)
    }) # end lapply(x)
    Tab = NULL
    for (i in 1:length(d_env_b)){
      d=d_env_b[[i]]
      for(j in 1:length(d)){
        dd=d[[j]]
        for(k in dd){
          if(!is.null(k$Tab)){
            Comp = k$Comp
            Comp = unlist(lapply(as.character(Comp),function(x){strsplit(strsplit(x,"[[]")[[1]][2],",")[[1]][1]}))
            k = k$Tab
            mod = unique(unlist(lapply(as.character(Comp),function(x){substr(strsplit(x,"#")[[1]][2],1,2)})))
            comp=NULL ; for(l in 1:length(mod)){if(!(mod[l] %in% c("VA","VB","JA"))){comp=c(comp,l)}} ; if(length(comp)>0){mod=mod[-comp]} ; if(length(mod)==0){mod=NA}
            nb_comp = length(Comp)
            k = cbind(k,unlist(lapply(as.character(names(k)),function(x){strsplit(unlist(lapply(as.character(x),function(y){strsplit(y,",")[[1]][1]})),"[[]")[[1]][2]})))
            colnames(k) = c("value","variete")
            t = k[k[,"variete"]%in%Comp,]
            
            mel = as.character(Melanges[Melanges %in% k[,"variete"]])
            mel=mel[!(mel %in% t[,"variete"])]
            if(is.na(mod)){a = grep("[.]2|[.]3",mel) ; if(length(a)>0){mel = mel[-a]}; mel_base=mel
            }else if(mod%in%c("JA","VA")){mel = mel[grep("[.]2",mel)] ;  mel_base=gsub("[.]2","",mel)
            }else if(mod%in%c("VB")){mel = mel[grep("[.]3",mel)];  mel_base=gsub("[.]3","",mel)
            }
          
            overY = k[(k[,"variete"]%in%c(mel,"MoyenneComposantes")),]
            OverY = 100*(as.numeric(as.character(overY[overY[,"variete"] %in% mel,"value"])) - as.numeric(as.character(overY[overY[,"variete"] %in% "MoyenneComposantes","value"])))/as.numeric(as.character(overY[overY[,"variete"] %in% "MoyenneComposantes","value"]))
            if(!is.null(tab_proportions)){
              t[,"variete"] = unlist(lapply(t[,"variete"],function(x){strsplit(x,"#")[[1]][1]}))
              comp=NULL
              if(!(mel %in% tab_proportions$melange)){
                if(mel_base %in% tab_proportions$melange){tab = tab_proportions[tab_proportions$melange %in% mel_base,]
                }else{t = cbind(t,rep(1/nrow(t),nrow(t))) ; comp=1 ; colnames(t)[ncol(t)] = "proportion"}
              }else{ tab = (tab_proportions[tab_proportions$melange %in% k[,"variete"],])}
            if(is.null(comp)){
              tab[,"germplasm"] = unlist(lapply(as.character(tab[,"germplasm"]),function(x){strsplit(x,"#")[[1]][1]}))
              t = merge(t,tab,by.x = "variete",by.y = "germplasm")
              t=t[t[,"melange"] %in% c(mel,mel_base),]
            }
            }else{
              t = cbind(t,rep(1/nrow(t),nrow(t)))
              colnames(t)[ncol(t)] = "proportion"
            }
            Wvar = weightedvar(as.numeric(as.character(t[,"value"])),  as.numeric(as.character(t[,"proportion"])), na.rm = FALSE)
            Tab = rbind(Tab,c(mel,OverY,nb_comp,Wvar))
          }
        }
      }
    }
    colnames(Tab) = c("melange","overyielding","NbComp","WeightedVar")
    Tab=as.data.frame(Tab)
    correl1 = rcorr(as.numeric(as.character(Tab$overyielding)),as.numeric(as.character(Tab$NbComp)))
    correl2 = rcorr(as.numeric(as.character(Tab$overyielding)),as.numeric(as.character(Tab$WeightedVar)))
    return(list("Tab"=Tab,"Correl" = c(correl1$r[1,2],correl1$P[2],correl2$r[1,2],correl2$P[2])))
  })
  names(a)=vec_variables
  Correl = NULL ; Tab = a[[1]]$Tab[,1:2]
  for (i in 1:length(a)){
    Correl = cbind(Correl,a[[i]]$Correl)
    if(i>1){
      Tab = join(Tab,a[[i]]$Tab[1:2],by="melange")
    }
  }
  colnames(Correl) = colnames(Tab)[-1] = vec_variables
  if(language == "french"){rownames(Correl)=c("Rcorr overyielding~Nb composantes", "pvalue overyielding~Nb composantes","Rcorr overyielding~Variance composantes", "pvalue overyielding~Variance composantes")}
  if(language == "english"){rownames(Correl)=c("Rcorr overyielding~Nb components", "pvalue overyielding~Nb components","Rcorr overyielding~Variance components", "pvalue overyielding~Variance components")}
  
  return(list("overyielding" = Tab, "Correlations"=Correl))
}#end correlations
  
  
  
#3. modalités de sélection ---------- 
if(table.type %in% c("varIntra","selection.modalities")){
  Mixtures$expe_melange = unlist(lapply(1:nrow(Mixtures),function(i){
    m = Mixtures[i,]
    if(m$sl_statut == "son"){return(m$expe_melange)}
    if(m$sl_statut == "father"){
      if(length(unique(Mixtures[grep(m$expe,Mixtures$expe),"sl_statut"])) == 1){return(NA)}else{return(unique(Mixtures[grep(m$expe,Mixtures$mixture_id),"expe_melange"]))}
    }
  }))
  
  VI = lapply(vec_variables,function(variable){
    if(variable %in% names(res_model_varintra)){
      if(!file.exists(paste(path_to_tables,"/Varintra/Rep_Sel_",variable,".csv",sep=""))){
        Tab = ggplot_mixture1(res_model = res_model_varintra, melanges_PPB_mixture = data_mixtures$Mixtures_all, data_S = data_mixtures$Mixtures_selection, 
                              melanges_tot = data_mixtures$Mix_tot, variable, year=c("2016","2017"), model = "model_varintra", 
                              plot.type = "comp.mod.network", person=NULL, nb_parameters_per_plot = 20, save=NULL)$Tab
        write.table(Tab,paste(path_to_tables,"/Varintra/Rep_Sel_",variable,".csv",sep=""),sep=";")
      }else{
        Tab = read.table(paste(path_to_tables,"/Varintra/Rep_Sel_",variable,".csv",sep=""),sep=";")
      }
    
      colnames(Tab) = c("Mod1","Mod2","Mod3","Mod3vsMod2")
      
      a = lapply(c("Mod1","Mod2","Mod3","Mod3vsMod2"),function(x){get.gain(Tab,to_split=NULL,col=x)})
      Res=NULL
      for(i in a){i=as.vector(i) ; Res=cbind(Res,i)}
    }else{Res=matrix(NA,nrow=7,ncol=4)}
    rownames(Res) = c("mean_gain","sd_gain","statistic","pvalue","stars","test","n")
    colnames(Res) = c("M1","M2","M3","M3vsM2")
    return(Res)
  })
  names(VI) = vec_variables
  if(table.type == "varIntra"){return(VI)}
}
  
if(table.type == "selection.modalities"){
  Melanges=unique(Mixtures[Mixtures$sl_statut %in% "son" & unlist(lapply(as.character(Mixtures$son), function(x){return(strsplit(x,"_")[[1]][1])})) != unlist(lapply(as.character(Mixtures$father), function(x){return(strsplit(x,"_")[[1]][1])})), "son_germplasm"])
  
  RS = lapply(vec_variables,function(variable){
    print(variable)
    if(!file.exists(paste(path_to_tables,"/Rep_Sel/sel_response_",variable,"_",paste(year,collapse="-"),".csv",sep=""))){
      Tab = analyse.selection(Mixtures, res_model, vec_variables = variable, plot.save=NULL, table.save=path_to_tables, language=language, list_trad=list_trad, 
                              year=year_RS, data_mixtures=data_mixtures, selection.type = "response.sel.mixture", data_SR_all=data_SR_all)
    }else{
      Tab = read.table(paste(path_to_tables,"/Rep_Sel/sel_response_",variable,"_",paste(year,collapse="-"),".csv",sep=""),sep=";",header=T)
    }
    
    if(class(Tab) == "list"){Tab=Tab[[1]]}
    if(!is.null(Tab)){
      Tab=as.data.frame(Tab)
      Tab[,1:(ncol(Tab)-1)] = apply(Tab[,1:(ncol(Tab)-1)],2,function(x){as.numeric(as.character(x))})
      Tab$type = as.character(Tab$type)
      a=lapply(colnames(Tab)[-ncol(Tab)],function(x){
        t = get.gain(Tab,to_split="type",col=x)
        if(length(grep("1",x))==0){t = t[grep("melange",rownames(t)),]}
        return(t)
      })
      names(a) = colnames(Tab)[-ncol(Tab)]
      Res=NULL
      for(i in a){
        if(class(i)=="character"){i=as.vector(i)} 
        Res=rbind(Res,i)
      }
      colnames(Res) = c("mean_gain","sd_gain","statistic","pvalue","stars","test","n")
      rownames(Res)[grep("composante",rownames(Res))] = "M1 Composantes"
      rownames(Res)[grep("melange",rownames(Res))] = "M1 Melanges"
      rownames(Res)[grep("i",rownames(Res))] =  colnames(Tab)[2:4]
      Res=t(Res)
      Res = Res[,order(colnames(Res))]
      colnames(Res) = c("M1 Composantes","M1 Melanges","M2","M3","M3vsM2")
      return(list("Res"=Res,"Tab"=Tab))
    }else{
      Res = matrix(NA,ncol=4,nrow=7)
      rownames(Res) = c("mean_gain","sd_gain","statistic","pvalue","stars","test","n")
      colnames(Res) = c("M1","M2","M3","M3vsM2")
      return(list("Res"=Res, "Tab"=NULL))
    }
    
  })
  names(RS)=vec_variables
  
  DS = lapply(vec_variables,function(variable){
    mel_to_get = rownames(RS[[variable]]$Tab[grep("melange", RS[[variable]]$Tab$type),])
    comp_to_get_Mod1 = rownames(RS[[variable]]$Tab[grep("composante", RS[[variable]]$Tab$type),])
    comp_to_get = Mixtures[Mixtures$sl_statut %in% "son",]
    comp_to_get = comp_to_get[comp_to_get$expe_melange %in% mel_to_get,c("father_germplasm","location","year")]
    if(file.exists(paste(path_to_tables,"/Diff_Sel/DifferentielSelection_",variable,"_",paste(year_DS,collapse="-"),".csv",sep=""))){
      Tab = read.table(paste(path_to_tables,"/Diff_Sel/DifferentielSelection_",variable,"_",paste(year_DS,collapse="-"),".csv",sep=""),sep=";",header=T)
    }else{
      Tab = analyse.selection(Mixtures_all, res_model, vec_variables = variable, plot.save=NULL, table.save=path_to_tables, language=language, list_trad=list_trad, 
                            year=year_DS, data_mixtures=data_mixtures, selection.type = "sel.diff.network", data_S_all=data_S_all, data_SR_all=data_SR_all)[[1]]$Tot[[1]]$tab
    }

    if(!is.null(Tab)){
      #get only DS for populations we have a RS
      Tab_melange = Tab[Tab$germplasm %in% mel_to_get,]
      Tab_composantes_Mod1 = Tab[Tab$group %in% comp_to_get_Mod1,]
      Tab_composantes = Tab[Tab$germplasm %in% comp_to_get$father_germplasm & Tab$location %in% comp_to_get$location,]
      Tab=rbind(Tab_melange, Tab_composantes, Tab_composantes_Mod1)
      DS = get.gain(Tab,to_split="modalite",col="overyielding")
      ds =  get.gain(Tab,to_split=NULL,col="overyielding")
      DS=rbind(DS,ds)
      rownames(DS)[4]="Total"
      return(DS)
    }else{
      DS = matrix(NA,ncol = 7,nrow=4)
      colnames(DS) = c("mean","sd","statistic.t","pvalue","stars","test","n")
      rownames(DS) = c("Composantes : Mod 1","Composantes : Mod 2","Mélanges : Mod 3","Total")
      return(DS)
    }
  })
  names(DS) = vec_variables
  

  
  M = lapply(vec_variables,function(variable){ return(list("DS"=DS[[variable]],"RS"=RS[[variable]],"varIntra"=VI[[variable]]))  })
  names(M) = vec_variables
  return(M)
}

}#end function
