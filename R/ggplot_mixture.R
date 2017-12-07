# 0. help -----------------------------------------------------------------
#' Barplot for the comparison of the mixture vs its components & the mean of its components
#' 
#' @param res_model output from the \code{PPBstats::analyse.outputs} function or from \code{shinemas2R::get.data} function
#' 
#' @param farmers_data output from the \code{shinemas2R::get.data} function, with argument query type == "data.mixture.1"
#' 
#' @param variable the variable to study
#' 
#' @param plot.type the type of plot wanted. 
#' \itemize{
#'  \item "comp.in.farm" to compare mixtures to each of their components and the mean of their components in a farm
#'  \item "mix.comp.distribution" to get the histogram of the overyieldings, 
#'  the overyielding for each mixture being computed as the value of the mixture minus the mean value of its components, divided by the mean value of the mixture's component 
#'  \item "mix.gain.distribution" to get, for each mixture, the value of the lower component, the highest component and the mean of the components
#'  \item "comp.mod" to compare the selection modalities of a mixture
#'  \item "comp.mod.network" to get, for all mixtures, the comparison of the different selection modalities with the non selected mixture and the comparison of the different selection modalities
#' }
#'
#' @param model the model from which the results comes from. If NULL res_model must come from \code{shinemas2R::get.data} function
#'
#' @param person if plot.type = "comp.in.farm" or "comp.mod", the farmers you want the analysis done to
#' 
#' @param nb_parameters_per_plot the number of parameters per plot
#
#' @return A list containing, for each environment and mixture, the barplots ("bp") and the tables ("Tab")
#' 
#' @author Gaelle Van Frank
#' 
#' @seealso \code{\link{shinemas2R::get.data}}
#' 

ggplot_mixture1 = function(res_model, 
                           melanges_PPB_mixture,
                           melanges_tot,
                           data_S, 
                           variable, 
                           year, 
                           model, 
                           language,
                           plot.type = "comp.in.farm", 
                           person=NULL,
                           nb_parameters_per_plot = 8,
                           save=NULL,
                           col_plot = "pval") 
{
  if(class(melanges_PPB_mixture)=="list"){melanges_PPB_mixture=melanges_PPB_mixture$data}
  melanges_tot=melanges_tot$data$data
  add_split_col = function(x, each){ rep(c(1:nrow(x)), each = each)[1:nrow(x)] } 
  data_S = data_S$data$data
  data_S = unique(data_S[,c("son","expe","sl_statut","expe_name","expe_name_2","son_germplasm","father","father_germplasm","son_person")])
  data_S = data_S[grep("bouquet",data_S$sl_statut),]
  
  if(!is.null(model)){
    if(model=="model_1"){param = "mu"}
    if(model=="model_varintra"){param = "sigma"}
  }else{
    if(class(res_model) == "list"){
      res_model=res_model$data
      if(class(res_model) == "list"){
        res_model=res_model$data
      }
    }
  }
  
  #0. functions -----------
  compare_pop = function(x,donnees){
    data = donnees[donnees$parameter %in% x,c(variable,"parameter")]
    if(length(unique(data$parameter))>1){
      mean = by(as.numeric(as.character(data[,variable])),data$parameter,function(x){mean(na.omit(x))})
      OverY = round(100*(mean[1]-mean[2])/mean[2],1)
      test1 = by(data[,variable],data$parameter,function(x){length(na.omit(x))})
      if(length(unique(test1>1)) == 1){
        test2 = by(as.numeric(as.character(data[,variable])),data$parameter,function(x){var(na.omit(x))})
        if(test2[1]==0 & test2[2]==0){
          # la variance est nulle
          test3 = by(as.numeric(as.character(data[,variable])),data$parameter,function(x){mean(na.omit(x))})
          if(test3[1] == test3[2]){pval = 1}else{pval=0}
        }else{
          # Test non paramétrique U de Wilcoxon-Mann-Whitney pour données semi_quantitatives
          data[,variable] = as.numeric(as.character(data[,variable]))
          data = data[!is.na(data[,variable]),]
          pval = wilcox.test(data[,variable] ~ data[,"parameter"])$p.value
        }
        return(c(mean,pval,OverY))
      }else{pval=NA ;       return(c(mean,pval,OverY))}

    }else{return(NULL)}
    
  }
  
  plot_comp_mod <- function(Data,save){
    Data = arrange(Data, median)
    Data$max = max(Data$median, na.rm = TRUE)
    if(length(grep("Mod4",Data$mod))>1){
      Data$gain = Data$median/Data[grep("Mod4",Data$mod),"median"]-1
    }else{Data$gain = NA}
    year=paste(unique(Data$year),sep=",")
    
    
    p = ggplot(Data, aes(x = reorder(germplasm, median), y = median, fill=unlist(Data$mod))) + geom_bar(stat = "identity")+ theme(legend.title = element_blank())
    p = p + scale_fill_manual("legend",values=c("Mélange issu 1 année sélection 
  dans composantes (Mod2)"="gold","Mélange sélectionné (Mod3)"="steelblue3","Mélange issu 2 années sélection 
  dans composantes (Mod1)"="chartreuse3","Mélange non sélectionné (Mod4)"="red"))
    
    # ajouter les groupes de significativité
    p = p + geom_text(data = Data, aes(x = reorder(germplasm, median), y = median/2, label = groups), angle = 90, color = "white")
    p = p + ggtitle(paste("Comparaison modalités de sélection",", données ",year,sep="")) + ylab(variable)
    
    # pivoter légende axe abscisses
    p = p + xlab("") + theme(axis.text.x = element_text(angle = 90)) + ylim(0, Data[1,"max"])
    
    if(!is.null(save)){write.table(Data,file=paste(save,"/Selection_mod_",variable,".csv",sep=""),sep=";")}
    
    return(list("Tab"=Data,"plot"=p))
  }
  
  # 1. Compare, for each mixture in each farm, the mixture to its components and the mean of these components ----
  if ( plot.type == "comp.in.farm" | plot.type == "mix.comp.distribution"| plot.type == "mix.gain.distribution") {
    # Séparer par environnement
    d_env = plyr:::splitter_d(melanges_PPB_mixture, .(location))
    if (plot.type == "comp.in.farm"){d_env=list(d_env[[grep(person,names(d_env))]])}
    
    # Par environnement, on sépare par mélange pour ensuite faire les graphs
    d_env_b = lapply(d_env, function(x){
      # une table par mélange
      if (plot.type %in% c("mix.gain.distribution","mix.comp.distribution")){mix = plyr:::splitter_d(x, .(mixture_id))}
      if (plot.type == "comp.in.farm"){mix = plyr:::splitter_d(x, .(expe_melange))}
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
              if(!is.null(model)){
                # Data from model results
                mcmc = get_result_model(res_model, noms, type_result = "MCMC", variable, model,param = param, year = yr)
                Mel = mcmc[,unlist(rm_between(colnames(mcmc), "[", ",", extract=TRUE)) %in% noms[which(noms$Type == "Mélange"),"son_germplasm"]]
                
                if (length(Mel) > 0) {
                  Comp = mcmc[,unlist(rm_between(colnames(mcmc), "[", ",", extract=TRUE)) %in% noms[which(noms$Type == "Composante"),"son_germplasm"]]
                  if(!is.null(ncol(Comp))){if (ncol(Comp) < length(unique(noms[noms$Type == "Composante","son_germplasm"])) | length(noms[noms$Type == "Composante","Type"])==0){missingComp = TRUE}else{missingComp=FALSE}}else{missingComp=TRUE}
                  
                  if(!missingComp){
                    MeanComp = apply(Comp, 1, mean)
                    M = cbind(Mel, MeanComp, Comp)
                    colnames(M)[colnames(M) %in% "MeanComp"] = paste(param,"[","MoyenneComposantes",",",paysan,":",yr,"]",sep="")
                  }else{M = cbind(Mel, Comp)}
                  
                  colnames(M)[colnames(M) %in% "Mel"] = paste(param,"[", unique(noms[noms$Type %in% "Mélange","son_germplasm"]),",",paysan,":",yr,"]",sep="")
                  M=list("MCMC"=M)
                  #         attributes(M)$PPBstats.object = "check_model_model_1"
                  comp.mu = mean_comparisons.check_model_1(M, param, get.at.least.X.groups = 1)
                  
                  C=comp.mu$data_mean_comparisons[[1]]$Mpvalue
                  
                  if(!missingComp){
                    A = cbind(C[which(rownames(C) == paste(param,"[",melange,",",paysan,":",yr,"]",sep="")),], C[,which(colnames(C) == paste(param,"[",melange,",",paysan,":",yr,"]",sep=""))]) 
                    A = apply(A,1,sum)
                  }else{A=NA}
                  
                  comp.mu=comp.mu$data_mean_comparisons[[1]]$mean.comparisons
                  comp.mu$germplasm = unlist(rm_between(comp.mu$parameter, "[", ",", extract=TRUE))
                  comp.mu$pval=A[as.character(comp.mu$parameter)]
                }else{comp.mu=NULL}
              }else{ # Data from row table (shinemas2R::get.data)
                comp.mu = res_model[res_model$son %in% noms$son,c("son",variable)]
                comp.mu = comp.mu[!is.na(comp.mu[,variable]),]
                if(nrow(comp.mu) >0){
                  comp.mu = aggregate(as.numeric(as.character(comp.mu[,variable])), by=list(comp.mu$son), FUN=mean)
                  colnames(comp.mu) = c("son","median")
                  comp.mu$germplasm = unlist(lapply(as.character(comp.mu$son),function(x){strsplit(x,"_")[[1]][1]}))
                  comp.mu$pval=NA
                  comp.mu$entry = comp.mu$germplasm
                }else{comp.mu=NULL}
              }
             if(!is.null(comp.mu)){
                  type = NULL
                  for (i in 1:nrow(comp.mu)) { 
                    a = unique(unlist(noms[noms$son_germplasm %in% comp.mu[i,"germplasm"],"Type"]))
                    if (length(a)>0) {
                      if(a == "Mélange"){
                        if(length(grep("[.]2",comp.mu[i,"entry"]))>0){ type = c(type, "Mélange issu 1 année sélection 
dans composantes (Mod2)")
                        }else if(length(grep("#B",comp.mu[i,"entry"]))>0){ type = c(type, "Mélange sélectionné (Mod3)")
                        }else if(length(grep("[.]3",comp.mu[i,"entry"]))>0){ type = c(type, "Mélange issu 2 années sélection 
dans composantes (Mod1)")
                        }else{ type = c(type, "Mélange non sélectionné") 
                        }
                      }
                      if(a == "Composante"){type = c(type, a)}
                    }
                    if (comp.mu[i,"germplasm"] == "MoyenneComposantes") { type = c(type, "MoyenneComposantes")}
                  }
                  
                  Data = cbind(comp.mu, type)
                  Data = arrange(Data, median)
                  Data$max = max(Data$median, na.rm = TRUE)
                  d = Data[grep("^Mélange non sélectionné$",Data$type),"germplasm"]
                  if(length(d)>0){
                    Data$melange = d
                  }else if(length( Data[grep("Mod2",Data$type),"germplasm"])>0){
                    Data$melange = gsub("[.]2", "",Data[grep("Mod2",Data$type),"germplasm"])
                  }else if(length( Data[grep("Mod3",Data$type),"germplasm"])>0){
                    to_delete = strsplit(Data[grep("Mod3",Data$type),"germplasm"],"#")[[1]][2]
                    Data$melange = gsub(paste("#",to_delete,sep=""), "",Data[grep("Mod3",Data$type),"germplasm"])
                  }else if(length( Data[grep("Mod1",Data$type),"germplasm"])>0){
                    Data$melange = gsub("[.]3", "",Data[grep("Mod2",Data$type),"germplasm"])
                  }
                  Data$true_melange = melange
                  
                  if(!is.null(save)){write.table(Data,file=paste(save,"/Par_paysan/Mel_et_comp_",unique(Data$melange),"_",unique(Data$environment),"_",variable,".csv",sep=""),sep=";",dec=",")}
                  if (plot.type == "comp.in.farm") {
                    Data$split = add_split_col(Data, nb_parameters_per_plot)
                    Data_split = plyr:::splitter_d(Data, .(split))
                    
                    # faire le graph pour chaque split
                    bp = lapply(Data_split , function(z){return(barplot.mixture1(z,title = paste(person, " : ",strsplit(as.character(variable),"---")[[1]][1],", ","données ",yr, sep=""),melange = melange))})
                    
                    return(list("Tab" = Data,"plot"= bp))
                  }
                  if ((plot.type == "mix.comp.distribution" | plot.type == "mix.gain.distribution") & missingComp == FALSE) {
                    return(list("Tab" = Data,"plot" = NULL))
                  }
              }else{  # a retirer comp.mu=NULL
                warning("No data for the mixture")
                return(list("Tab" = NULL,"plot"= NULL))
              }
            }else{
              warning(paste("No data for ",yr,sep=""))
              return(list("Tab" = NULL,"plot"= NULL))
            }
          }else{
            warning(paste("No data for ",yr,sep=""))
            return(list("Tab" = NULL,"plot"= NULL))
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
    names(d_env_b) = names(d_env)
    Nul = TRUE
    for (i in 1:length(d_env_b)){
      for (j in 1:length(d_env_b[[i]])){
        for (k in 1:length(d_env_b[[i]][[j]])){
          if(!is.null(d_env_b[[i]][[j]][[k]])){Nul = FALSE}
        }
      }
    }
    if(plot.type == "comp.in.farm") {return(d_env_b)}
    
    Mat = NULL
    for (i in 1:length(d_env_b)){
      for (j in 1:length(d_env_b[[i]])){
        for(yr in 1:length(year)){
          if (!is.null(d_env_b[[i]][[j]][[yr]]$Tab)){
            M=d_env_b[[i]][[j]][[yr]]$Tab
            Mat = rbind(Mat,M)
          }
        }
      }
    }
    colnames(Mat)[2] = variable
 
  }
  
  
  # 2. Compare the effect of being a mixture vs the effect of being a component -----
  # Normalement ça marche
  if ( plot.type == "mixVScomp") {
    
    melange = grep("son",melanges_PPB_mixture$sl_statut)
    
    Mélanges = melanges_PPB_mixture[melange,]
    Composantes = melanges_PPB_mixture[-melange,]
    
    # Récupérer les résultats du modèle
    Result = lapply(list(Mélanges,Composantes), function(x){
      mcmc = get_result_model(res_model, x, type_result = "MCMC", variable, model="model_1", param = param, year = year)
      
      #concaténuer les chaines
      if (ncol(mcmc)>1) {
        MCMC = NULL
        for( i in 1:nrow(mcmc)) { MCMC = c(MCMC, mean(as.matrix(mcmc[i,])))}
      }else{
        MCMC = mcmc
      }
      return(MCMC)
    })
    
    Result = as.data.frame(cbind(Result[[1]],Result[[2]]))
    colnames(Result) = c(paste(param,"[Mélanges]",sep=""),paste(param,"[Composantes]",sep=""))
    
    # Comparer les MCMC des mélanges et des composantes
    Mpvalue = comp.parameters(Result, parameter = param, type = 1)
    Comparison = get.significant.groups(Mpvalue,Result, alpha = 0.05)
    
    Data = arrange(Comparison, median)
    Data$max = max(Data$median, na.rm = TRUE)
    Data$type = ex_bracket(Data$parameter)
    Data$germplasm = unlist(ex_bracket(Data$parameter))
    Data$max = max(Data$median, na.rm = TRUE)
    # graphique mélanges vs composantes
    bp = barplot.mixture1(Data,variable)
    return(list("Tab" = Data,"plot"=bp))
  }
  
  # 3. Sur le réseau, comparer la distribution des mélanges à celles de la moins bonne et la meilleure composante pour chaque mélange ----------
  # Normalement ça marche, pas testé sur données
  if (plot.type == "mix.comp.distribution" | plot.type == "mix.gain.distribution") {
    if(Nul == FALSE){
      Distrib = lapply(d_env_b, function(x){
        Mat = lapply(x, function(y) {
          mat = lapply(y,function(yr){
            Tab=yr$Tab
            Data = Tab[Tab$type %in% "Composante",]
            nb_comp=nrow(Data)
            if(!is.null(Data)){
              Data = Data[order(Data$median),]
              Data=Data[c(1,nrow(Data)),] ; Data$parameter = c("Worst","Best") 
              Data$Type = c("1.mois bonne","4.meilleure")
              Mel = Tab[grep("Mélange|Moyenne",Tab$type),] ; Mel$parameter = unlist(lapply(as.character(Mel$type),function(x){strsplit(x," ")[[1]][1]}))
              if(length(grep("Mod1",Mel$type))>0){
                Mel = Mel[grep("Mod1|MoyenneComposantes",Mel$type),]
              }else if(length(grep("Mod2",Mel$type))>0){
                Mel = Mel[grep("Mod2|MoyenneComposantes",Mel$type),]
              }
              Mel$Type = unlist(lapply(as.character(Mel$type),function(x){ifelse(length(grep("Mélange",x))>0,"3.mélange","2.moyenne composantes")}))
              M = rbind(Data,Mel)
              M = M[order(M$Type),]
            }else{
              M = NULL
            }
            return(list("plot" = NULL, "tab"= M, "nbComp" =  nb_comp))
          })
          names(mat)=year
          return(mat)
        })
        return(Mat)
      })
      
      if (plot.type == "mix.comp.distribution" | plot.type =="mix.gain.distribution"){
        toPlot=NULL
        D = lapply(Distrib,function(x) {
          return(lapply(x,function(y){return(lapply(y,function(yr){return(yr$tab)}))}))
        })
        for ( i in 1:length(D)) {
          for (j in 1:length(D[[i]])){
            for(yr in 1:length(D[[i]][[j]])){
              if(!is.null(D[[i]][[j]][[yr]])){toPlot=rbind(toPlot,cbind(as.matrix(D[[i]][[j]][[yr]]),rep(names(D)[i], 4),rep(paste("Paysan",i," Mélange",j,sep=""),4)))}
            }
          }
        }
        
        #         D = cbind(toPlot[toPlot$Type %in% "1.moins bonne","Moyenne"],toPlot[toPlot$Type %in% "2.moyenne composantes","Moyenne"],
        #                   toPlot[toPlot$Type %in% "3.mélange","Moyenne"],toPlot[toPlot$Type %in% "4.meilleure","Moyenne"])
        #         colnames(D) = c("1.moins bonne","2.moyenne composantes","3.mélange","4.meilleure")
        #         
        #         plot(D)
        
        colnames(toPlot)[(ncol(toPlot)-1):ncol(toPlot)] = c("Paysan","Group")
        rownames(toPlot)=NULL
        toPlot=as.data.frame(toPlot)
        toPlot$Moyenne = as.numeric(as.character(toPlot$median))
    
        if(plot.type =="mix.comp.distribution"){
          p = ggplot(toPlot, aes(x=Type,y=Moyenne,color = Group, shape=Group), xlab=variable)
          p = p +labs(x="", y=paste("Valeur du ",variable,sep=""))
          p = p + stat_summary(fun.y=mean,geom="point",color="black",shape="x",size=4.5)
          p = p + geom_jitter(position=position_jitter(0), cex=3) 
          p = p + scale_shape_manual(values = seq(1,nrow(toPlot)/4,1))
          p = p + geom_line()
          p = p + theme(legend.position="none")
          return(list("Tab"=toPlot,"plot"=p))
        }
        if(!is.null(save)){write.table(toPlot,file=paste(save,"/Distrib_",variable,".csv",sep=""),sep=";")}

      }

    }else{return(NULL)}
  }
  
  
  # 4. Sur le réseau, distribution du gain des mélanges par rapport aux composantes ----------
  if (plot.type == "mix.gain.distribution") {
    if(Nul==FALSE){
      Histo = lapply(Distrib,function(x){
        return(lapply(x, function(y){
          return(lapply(y,function(yr){
            z=yr$tab
            if(!is.null(z)){
              diff = as.numeric(as.character(z[grep("Mélange",z$parameter),"median"]))/as.numeric(as.character(z[grep("MoyenneComposantes",z$parameter),"median"]))-1
              return(c(unlist(diff),as.numeric(as.character(z[grep("MoyenneComposantes",z$parameter),"pval"])),yr$nbComp,unique(z$melange),unique(z$true_melange),unique(z$year),unique(z$location)))
          }}))
          }))
      })
      
      
      Data = cbind(
        unlist(lapply(Histo,function(paysan){return(lapply(paysan, function(melange){return(lapply(melange,function(yr){return(yr[[1]])}))}))})),
        unlist(lapply(Histo,function(paysan){return(lapply(paysan, function(melange){return(lapply(melange,function(yr){return(yr[[2]])}))}))})),
        unlist(lapply(Histo,function(paysan){return(lapply(paysan, function(melange){return(lapply(melange,function(yr){return(yr[[3]])}))}))})),
    #    unlist(lapply(Histo,function(paysan){return(lapply(paysan, function(melange){return(lapply(melange,function(yr){return(yr[[4]])}))}))})),
        unlist(lapply(Histo,function(paysan){return(lapply(paysan, function(melange){return(lapply(melange,function(yr){return(yr[[5]])}))}))})),
        unlist(lapply(Histo,function(paysan){return(lapply(paysan, function(melange){return(lapply(melange,function(yr){return(yr[[6]])}))}))})),
        unlist(lapply(Histo,function(paysan){return(lapply(paysan, function(melange){return(lapply(melange,function(yr){return(yr[[7]])}))}))}))
      )
      
      Data=cbind(rownames(Data),Data)
      rownames(Data)=seq(1,nrow(Data),1)
      Data=as.data.frame(Data)
      colnames(Data) = c("Paysan","overyielding","pvalue","nbComp","melange","year","location")
      Data$mod = unlist(lapply(as.character(Data$melange),function(x){
        if(length(grep("[.]2",x))>0){return("mod2")
        }else if(length(grep("[.]3",x))>0){return("mod1")
        }else{return("mod4")}
      }))

      p = get_histo(Data,col_plot,breaks=0.03, titre=variable,language=language)
      
      if(!is.null(save)){write.table(Data,file=paste(save,"/Histo_",variable,".csv",sep=""),sep=";")}
  
      return(list("Tab"=Data,"plot"=p))
    }else{
      return(NULL)
    }
    
  }
  
  
  # 5. Compare, for each mixture, the different selection practices -----
  if ( plot.type == "comp.mod" | plot.type=="comp.mod.network" ){
    d_env = plyr:::splitter_d(melanges_PPB_mixture, .(location))
    if(!is.null(person)){d_env = list(d_env[[grep(person,names(d_env))]])}
    d_env_b = lapply(d_env,function(D){
      D = D[D$sl_statut %in% "son" & !is.na(D$expe_melange),]
      D = plyr:::splitter_d(D, .(expe_melange))
      
      bp = lapply(D,function(x){
        M = unique(melanges_tot[melanges_tot$son_germplasm %in% unique(x$son_germplasm),c("son","son_year","son_germplasm","father","father_germplasm","selection_id","block","X","Y")])
        M = M[is.na(M$selection_id) & M$son_year %in% year,]
        if(nrow(M)>1){
          
          if(!is.null(model)){
            mcmc = lapply(year,function(yr){
              a = get_result_model(res_model, M, type_result = "MCMC", variable, model=model, param = param, year = yr)
              # Do not keep if year's selection
              data_S$parameter = paste(param,"[",unlist(lapply(as.character(data_S$son),function(x){strsplit(x,"_")[[1]][1]})),",",
                                       data_S$son_person, ":", unlist(lapply(as.character(data_S$son),function(x){strsplit(x,"_")[[1]][3]})),"]",sep="")
              a=a[!(names(a) %in% data_S$parameter)]
            })
            mcmc = do.call(cbind,mcmc)
            
            if(ncol(mcmc) > 1){
              a = unlist(lapply(year,function(yr){return(length(grep(yr,names(mcmc))))}))
              year_to_delete = c(year[a==1],year[a==0])

              if(length(year_to_delete)>0){
                b = grep(paste(year_to_delete,collapse="|"),names(mcmc))
                if(length(b)>0){mcmc = mcmc[,-b]}
                year = year[-grep(year_to_delete,year)]
                }

              comp.mu = lapply(year, function(yr){
                x = mcmc[,grep(yr,names(mcmc))]
                if(model=="model_1"){comp.mu = mean_comparisons.check_model_1(list("MCMC"=mcmc), param, get.at.least.X.groups = 1)}
                if(model=="model_varintra"){comp.mu =mean_comparisons.check_model_variance_intra(list("MCMC"=mcmc), param, get.at.least.X.groups = 1)}
                comp.mu=comp.mu$data_mean_comparisons[[1]]$mean.comparisons
                comp.mu$germplasm = unlist(rm_between(comp.mu$parameter, "[", ",", extract=TRUE))
                
                comp.mu$mod = unlist(lapply(as.character(comp.mu$germplasm),function(y){
                  if(length(grep("[.]2",y)) == 1){return("Mélange issu 1 année sélection 
  dans composantes (Mod2)")}
                  if(length(grep("#B",y)) == 1){return("Mélange sélectionné (Mod3)")}
                  if(length(grep("[.]3",y)) == 1){return("Mélange issu 2 années sélection 
  dans composantes (Mod1)")}
                  if(length(grep("[.]2",y)) == 0 & length(grep("#B",y)) == 0 &  length(grep(".3",y)) == 0){return("Mélange non sélectionné (Mod4)")}
                }))
                return(comp.mu)
              })
              }else{return(NULL)}
              
          }else{
            donnees = melanges_tot[,c(1:40,grep(paste("^",variable,"$",sep=""),colnames(melanges_tot)))]
            donnees = donnees[donnees$son %in% M$son,]
            donnees$parameter = paste("mu[",
                    unlist(lapply(as.character(donnees$son),function(x){strsplit(x,"_")[[1]][1]})),
                    ",",donnees$son_person,":",donnees$son_year,"]",sep=""
            )
            donnees = donnees[!is.na(donnees[,variable]),]
            Moy = by(donnees[,variable],donnees$parameter, function(x){mean(as.numeric(as.character(x)))})
            M$parameter =  paste("mu[",
                                 unlist(lapply(as.character(M$son),function(x){strsplit(x,"_")[[1]][1]})),",",
                                 unlist(lapply(as.character(M$son),function(x){strsplit(x,"_")[[1]][2]})),":",
                                 M$son_year,"]",sep=""
            )
            if(length(unique(M$parameter))>1){
              M$median = Moy[M$parameter]
              M$mod = unlist(lapply(as.character(M$parameter),function(y){
                if(length(grep("[.]2",y)) == 1){return("Mélange issu 1 année sélection 
  dans composantes (Mod2)")}
                if(length(grep("#B",y)) == 1){return("Mélange sélectionné (Mod3)")}
                if(length(grep("[.]3",y)) == 1){return("Mélange issu 2 années sélection 
  dans composantes (Mod1)")}
                if(length(grep("[.]2",y)) == 0 & length(grep("#B",y)) == 0 &  length(grep(".3",y)) == 0){return("Mélange non sélectionné (Mod4)")}
              }))
              M$germplasm = unlist(lapply(as.character(M$son),function(x){strsplit(x,"_")[[1]][1]}))
              if(length(grep("Mod4",M$mod))>0){
                m1 = unique(M[grep("Mod1|Mod4",M$mod),"parameter"])
                m2 = unique(M[grep("Mod2|Mod4",M$mod),"parameter"])
                m3 = unique(M[grep("Mod3|Mod4",M$mod),"parameter"])
                a = lapply(list(m1,m2,m3),function(x){return(compare_pop(x,donnees))})
                pval=NULL
                for (i in 1:length(a)){if(!is.null(a[[i]])){pval = rbind(pval,c(names(a[[i]])[1],a[[i]][3]))}}
                M$pval = unlist(lapply(1:nrow(M),function(i){
                  b = pval[which(pval[,1] == M[i,"parameter"]),2] 
                  if(length(b)>0){return(b)}else{return(NA)}
                }))
              }else{M$pval = NA}
              M$median = as.numeric(as.character(M$median))
              comp.mu=unique(M[,c("parameter","median","pval","germplasm","mod","son_year")])
              comp.mu$groups = get_stars(comp.mu$pval)
              comp.mu = plyr:::splitter_d(comp.mu, .(son_year))
            }else{comp.mu=NULL}
          }
          
        if(!is.null(comp.mu)){p = lapply(comp.mu,function(yr){plot_comp_mod(yr,save=save)})}else{p=NULL}
        }else{return(NULL)}
      })
      return(bp)
    })
    if(plot.type =="comp.mod"){ return(d_env_b)}
  }

  
  # 6. Compare selection practices on the network
  if(plot.type == "comp.mod.network"){
    Nul = TRUE
    for (i in 1:length(d_env_b)){
      for (j in 1:length(d_env_b[[i]])){
        for (k in 1:length(d_env_b[[i]][[j]])){
          if(!is.null(d_env_b[[i]][[j]][[k]])){Nul = FALSE}
        }
      }
    }
    
    if(is.null(person) & Nul == FALSE){
      Gain_sel = lapply(d_env_b, function(pers){
        return(lapply(pers,function(mel){return(lapply(mel,function(yr){return(yr$Tab)}))}))})
      Gain_sel = delete.NULLs(Gain_sel)
      Mat=NULL
      noms = NULL
      for (pers in Gain_sel){
       for(mel in pers){
         if(length(mel)>0){
           for (y in mel){
             m1 = (y[grep("Mod1",y$mod),"median"] - y[grep("Mod4",y$mod),"median"])/ y[grep("Mod4",y$mod),"median"] ; if(length(m1)==0){m1=NA}
             m2 = (y[grep("Mod2",y$mod),"median"] - y[grep("Mod4",y$mod),"median"])/ y[grep("Mod4",y$mod),"median"] ; if(length(m2)==0){m2=NA}
             m3 = (y[grep("Mod3",y$mod),"median"] - y[grep("Mod4",y$mod),"median"])/ y[grep("Mod4",y$mod),"median"] ; if(length(m3)==0){m3=NA}
             m32 = (y[grep("Mod3",y$mod),"median"] - y[grep("Mod2",y$mod),"median"])/ y[grep("Mod2",y$mod),"median"] ; if(length(m32)==0){m32=NA}
             if(!is.na(m3)){
               melange = y[grep("Mod4",y$mod),"germplasm"]
             }else{
                melange = strsplit(y[grep("Mod3",y$mod),"germplasm"][1],"#")[[1]][1]}
             d=c(m1,m2,m3,m32)
             Mat=rbind(Mat,d)
             noms=c(noms,melange)
           }
         }
       }
      }
      to_delete = NULL
      for (i in 1:nrow(Mat)){m = Mat[i,] ; if(length(m[!is.na(m)])==0){to_delete=c(to_delete,i)}}
      if(length(to_delete)>0){Mat = Mat[-to_delete,]}
      colnames(Mat) = c("mod1","Mod2","Mod3","Mod3vsMod2")
      rownames(Mat)=noms
      if(!is.null(save)){write.table(Mat,file=paste(save,"Rep_Sel_",variable,".csv",sep=""),dec=",",sep=";")}
      return(list("Tab" = Mat))
    }else(return(NA))}

  
} # end function
