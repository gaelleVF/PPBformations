# 0. help -----------------------------------------------------------------
#' Function to get tables containing results from mixture experiment
#' 
#' @param pathway pathway to files needed
#
#' @details 
#' 
#' @return Generate tex and pdf files
#' 
#' @author Pierre Rivière, Gaelle Van Frank
#' 
#' 
#' 
get_mixture_tables <- function(res_model,
                               res_model_varintra = NULL,
                               table.type,
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
                               list_trad=NULL)
{
  
  Mixtures = data_mixtures$Mixtures_all$data
#0.1 If no year, take all years
  if(is.null(year)){year = seq("2016",strsplit(as.character(Sys.Date()),"-")[[1]][1],1)}
  if(is.null(year_DS)){year_DS = seq("2016",strsplit(as.character(Sys.Date()),"-")[[1]][1],1)}
  if(is.null(year_RS)){year_RS = seq("2016",strsplit(as.character(Sys.Date()),"-")[[1]][1],1)}
  
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
      return(rep(NA,6))
    }else{
      if(!is.null(to_split)){
        tab$split = as.numeric(as.factor(tab[,to_split]))
      }else{
        tab$split = 1
      }
      tab = plyr:::splitter_d(tab, .(split))
      
      a = lapply(tab,function(d){
        if(shapiro.test(d[,col])$p.value<=0.05){
          a = t.test(d[,col],mu=0)
          type = "t.test"
        }else{
          a = wilcox.test(d[,col],mu=0)
          type="wilcox.test"
        }
        return(c("mean"=round(100*mean(na.omit(d[,col])),3),"sd"=round(100*sd(na.omit(d[,col])),3),"statistic" = a$statistic,"pvalue"=a$p.value, "stars"=get_stars(a$p.value), "test"=type))
      })
      Res=NULL
      for (i in a){Res=rbind(Res,i)}
      rownames(Res)=unlist(lapply(tab,function(x){unique(x[,to_split])}))
      return(Res)
    }
  }
  

# 1. Distribution-------
if(table.type == "distribution"){
  D=list()
  for (i in vec_variables){
    if(!file.exists(paste(path_to_tables,"/Distrib_",i,".csv",sep=""))){
      p = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = data_mixtures$Mixtures_all, data_S = data_mixtures$Mixtures_selection, melanges_tot = data_mixtures$Mix_tot, i, 
                                   year=year, model="model_1", plot.type = "mix.gain.distribution", person=NULL, nb_parameters_per_plot = 15,
                                   save=path_to_tables)
    }
    d = read.table(paste(path_to_tables,"/Distrib_",i,".csv",sep=""),header=T,sep=";",comment.char="")
    d = d[-grep(paste(mix_to_delete,collapse="|"),d$melange),]
    tmp=cbind(as.numeric(as.character(d[seq(1,nrow(d),4),"Moyenne"])),as.numeric(as.character(d[seq(2,nrow(d),4),"Moyenne"])),as.numeric(as.character(d[seq(3,nrow(d),4),"Moyenne"])),as.numeric(as.character(d[seq(4,nrow(d),4),"Moyenne"])))
    tmp = cbind(tmp,unlist(as.character(d$melange[seq(1,nrow(d),4)])),unlist(as.character(d$year[seq(1,nrow(d),4)])),unlist(as.character(d$location[seq(1,nrow(d),4)])))
    tmp=data.frame(tmp,stringsAsFactors = F)
    colnames(tmp)=c(as.character(d[1:4,"Type"]),"melange","year","location")
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
    a = c(nrow(x),round(sum(as.numeric(as.character(x$'3.mélange'))>as.numeric(as.character(x$'2.moyenne composantes')))*100/nrow(x),3),round(sum(as.numeric(as.character(x$'3.mélange'))<as.numeric(as.character(x$'1.moins bonne')))*100/nrow(x),3),
          round(sum(as.numeric(as.character(x$'3.mélange'))>as.numeric(as.character(x$'4.meilleure')))*100/nrow(x),3),
          mean(as.numeric(as.character(x$'2.moyenne composantes'))),mean(as.numeric(as.character(x$'3.mélange'))), mean(as.numeric(x$overyielding))*100,Signif,get_stars(Signif))
    Tab=cbind(Tab,a)
  }
  
  colnames(Tab) = names(D)
  if(language=="english"){
    rownames(Tab) = c("number blends","Blend > Components' mean","Blend < Lowest component","Blend > Highest component","Mean components","Mean blends","Overyielding","pvalue overyielding","stars pval")
  }else{
    rownames(Tab) = c("Nombre de mélanges","Proportion mélanges > moyenne des composantes","Proportion mélanges < composante la plus basse","Proportion mélanges > composante la plus haute",
                      "Moyenne des composantes","Moyenne des mélanges","Gain moyen","pvalue overyielding","stars")
    
  }
  return(Tab)

}
  
if(table.type == "correlations"){
  
  
}
  
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
      Tab = ggplot_mixture1(res_model = res_model_varintra, melanges_PPB_mixture = data_mixtures$Mixtures_all, data_S = data_mixtures$Mixtures_selection, 
                            melanges_tot = data_mixtures$Mix_tot, variable, year=c("2016","2017"), model = "model_varintra", 
                            plot.type = "comp.mod.network", person=NULL, nb_parameters_per_plot = 20, save=NULL)$Tab
      colnames(Tab) = c("Mod1","Mod2","Mod3","Mod3vsMod2")
      
      a = lapply(c("Mod1","Mod2","Mod3","Mod3vsMod2"),function(x){get.gain(Tab,to_split=NULL,col=x)})
      Res=NULL
      for(i in a){i=as.vector(i) ; Res=cbind(Res,i)}
    }else{Res=matrix(NA,nrow=6,ncol=4)}
    rownames(Res) = c("mean_gain","sd_gain","statistic","pvalue","stars","test")
    colnames(Res) = c("M1","M2","M3","M3vsM2")
    return(Res)
  })
  names(VI) = vec_variables
  if(table.type == "varIntra"){return(VI)}
}
  
if(table.type == "selection.modalities"){
  Melanges=unique(Mixtures[Mixtures$sl_statut %in% "son" & unlist(lapply(as.character(Mixtures$son), function(x){return(strsplit(x,"_")[[1]][1])})) != unlist(lapply(as.character(Mixtures$father), function(x){return(strsplit(x,"_")[[1]][1])})), "son_germplasm"])
  
  DS = lapply(vec_variables,function(variable){
    if(file.exists(paste(path_to_tables,"/Diff_Sel/DifferentielSelection_",variable,"_",paste(year_DS,collapse="-"),".csv",sep=""))){
      Tab = read.table(paste(path_to_tables,"/Diff_Sel/DifferentielSelection_",variable,"_",paste(year_DS,collapse="-"),".csv",sep=""),sep=";",header=T)
    }else{
      Tab = analyse.selection(Mixtures_all, res_model1, vec_variables = variable, plot.save=NULL, table.save=path_to_tables, language=language, list_trad=list_trad, 
                            year=year_DS, data_mixtures=data_mixtures, selection.type = "sel.diff.network")[[1]]$Tot[[1]]$tab
    }
    
    DS = get.gain(Tab,to_split="modalite",col="overyielding")
    ds =  get.gain(Tab,to_split=NULL,col="overyielding")
    DS=rbind(DS,ds)
    rownames(DS)[4]="Total"
    return(DS)
  })
  names(DS) = vec_variables
  
  RS = lapply(vec_variables,function(variable){
    if(file.exists(paste(path_to_tables,"/Rep_Sel/sel_response_",variable,"_",paste(year_RS,collapse="-"),".csv",sep=""))){
      Tab = read.table(paste(path_to_tables,"/Rep_Sel/sel_response_",variable,"_",paste(year_RS,collapse="-"),".csv",sep=""),sep=";",header=T)
    }else{
      Tab = analyse.selection(Mixtures_all, res_model1, vec_variables = variable, plot.save=NULL, table.save=path_to_tables, language=language, list_trad=list_trad, 
                            year=year_RS, data_mixtures=data_mixtures, selection.type = "response.sel.mixture")
    }
    
    if(!is.null(Tab)){
      a=lapply(c("mod1","Mod2","Mod3","Mod3vsMod2"),function(x){get.gain(Tab,to_split=NULL,col=x)})
      Res=NULL
      for(i in a){i=as.vector(i) ; Res=cbind(Res,i)}
      rownames(Res) = c("mean_gain","sd_gain","statistic","pvalue","stars","test")
      colnames(Res) = c("M1","M2","M3","M3vsM2")
      return(Res)
    }else{
      Res = matrix(NA,ncol=4,nrow=6)
      rownames(Res) = c("mean_gain","sd_gain","statistic","pvalue","stars","test")
      colnames(Res) = c("M1","M2","M3","M3vsM2")
      return(Res)
    }

  })
  names(RS)=vec_variables

  
  M = lapply(vec_variables,function(variable){ return(list("DS"=DS[[variable]],"RS"=RS[[variable]],"varIntra"=VI[[variable]]))   })
  names(M) = vec_variables
  return(M)
}

}#end function