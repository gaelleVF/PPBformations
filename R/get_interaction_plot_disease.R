get_interaction_plot_disease <- function(tab, nb_parameter_per_plot,vec_variables,type="year")
{
  
add_split_col= function(x, each){ rep(c(1:nrow(x)), each = each)[1:nrow(x)] } 
  
format_date <- function(char){
  date = "not_date"
  if(length(strsplit(char,"-")[[1]])>1){
    a = strsplit(char,"-")[[1]]
    if(length(which(nchar(a)==4))>0){
      if(which(nchar(a)==4)==1){return(paste(a[3],a[2],a[1],sep="/"))}
      if(which(nchar(a)==4)==3){return(paste(a[1],a[2],a[3],sep="/"))}
    }
    date="date"
    if(length(which(nchar(a)==4))==0){return(paste(a[1],a[2],paste("20",a[3],sep=""),sep="/"))}
  }
  
  if(length(strsplit(char,"/")[[1]])>1){
    a = strsplit(char,"/")[[1]]
    if(length(which(nchar(a)==4))>0){
      if(which(nchar(a)==4)==1){return(paste(a[3],a[2],a[1],sep="/"))}
      if(which(nchar(a)==4)==3){return(paste(a[1],a[2],a[3],sep="/"))}
    }
    date="date"
    if(length(which(nchar(a)==4))==0){return(paste(a[1],a[2],paste("20",a[3],sep=""),sep="/"))}
  }
  
  if(date == "not_date"){return("NA")}
}



if(type=="year"){
 
  if(!is.null(tab$duplicated_infos)){ 
    dup = lapply(tab$duplicated_infos,function(x){return(cbind(x[[1]],"block"=1,x[[2]]))})
    Dupl=NULL
    Data_duplicated_groups = NULL
    for (i in 1:length(dup)){
      Data_duplicated_groups = rbind(Data_duplicated_groups,c(paste("Groupe",i,sep=" "),unlist(as.character(dup[[i]][,1]))))
      dup[[i]][,1] = paste("Groupe",i,sep=" ")
      Dupl = rbind(Dupl,dup[[i]])
    }
    colnames(Dupl)[1] = "germplasm"
  }else{Dupl=NULL; Data_duplicated_groups=NULL}
  no_Dupl=NULL
  for (i in 1:length(tab$not_duplicated_infos)){no_Dupl = rbind(no_Dupl,tab$not_duplicated_infos[[i]])}
  
  Tab = rbind(Dupl,no_Dupl)
  
  dates = unique(unlist(sapply(Tab[,3:ncol(Tab)],function(x){unique(x[!is.na(x)])})))
  lev = dates[sort.list(as.POSIXct(strptime(dates,"%d/%m/%Y")))]
  
  Tab$split = add_split_col(Tab,nb_parameter_per_plot)
  Tab = plyr:::splitter_d(Tab, .(split))
  
  p <- lapply(Tab,function(x){
    x = melt(x,id.vars="germplasm",measure.vars = vec_variables[-grep(paste(setdiff(vec_variables,colnames(x)[3:ncol(x)]),collapse="|"),vec_variables)])
    x=x[!is.na(x$value),]
    x$variable = unlist(lapply(as.character(x$variable),function(x){strsplit(x,"_")[[1]][1]}))
    x$variable=unlist(lapply(x$variable,function(y){
      if(y=="sain"){return(1)}
      if(y=="malade"){return(2)}
      if(y=="tres"){return(3)}
    }))
    x$value = factor(x$value,levels=lev)
    
    p = ggplot(data=x,aes(x=value,y=variable, colour = germplasm, group=germplasm)) 
    p = p + labs(x="date",y="note maladie")
    p = p + scale_y_continuous(breaks = c(1,2,3))
    
    if(length(unique(x$value))>1){
      p = p + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line") 
    }else{p = p + geom_jitter(inherit.aes=TRUE,width=0.1,height=0)}
    
    return(p)
  })
  return(list("plot" = p, "Data_groups"=Data_duplicated_groups))
  
}  
  
if(type=="all_year"){
  Tab=tab$not_duplicated_infos$`set-1`
  Tab=cbind(Tab,Tab[,ncol(Tab)])
  Tab = cbind(Tab[,1:2],apply(Tab[,3:ncol(Tab)],2,function(x){
    return(unlist(lapply(x,function(y){
      if(!is.na(y)){
        return(format_date(as.character(y)))
       }else{return(y)}
    })))
  }))
  Tab=Tab[,-ncol(Tab)]
  yr = as.character(unique(unlist(Tab))[grep("/", unique(unlist(Tab)))])
  yr = unique(unlist(lapply(yr,function(x){strsplit(x,"/")[[1]][3]})))
  
  if(length(yr)>1){
    dates = unique(unlist(sapply(Tab[,3:ncol(Tab)],function(x){unique(x[!is.na(x)])})))
    lev = dates[sort.list(as.POSIXct(strptime(dates,"%d/%m/%Y")))]
    
    Tab$split = add_split_col(Tab,nb_parameter_per_plot)
    Tab = lapply(unique(Tab$split),function(nb){
      return(Tab[Tab$split %in% nb,])
    })
    
    p <- lapply(Tab,function(TAB){
      TAB = melt(TAB,id.vars="germplasm",measure.vars = vec_variables[-grep(paste(setdiff(vec_variables,colnames(TAB)[3:ncol(TAB)]),collapse="|"),vec_variables)])
      TAB=TAB[!is.na(TAB$value),]
      TAB$variable = unlist(lapply(as.character(TAB$variable),function(TAB){strsplit(TAB,"_")[[1]][1]}))
      TAB$variable=unlist(lapply(TAB$variable,function(y){
        if(y=="sain"){return(1)}
        if(y=="malade"){return(2)}
        if(y=="tres"){return(3)}
      }))
      TAB$value = factor(TAB$value,levels=lev)
      TAB$year = unlist(lapply(as.character(TAB$value),function(x){strsplit(x,"/")[[1]][3]}))
      TAB$BY = paste(TAB$germplasm,TAB$year,sep=":")
      M = aggregate(TAB$variable,by=list(TAB$BY),mean)
      M$year = unlist(lapply(as.character(M[,1]),function(x){strsplit(x,":")[[1]][2]}))
      M$germplasm = unlist(lapply(as.character(M[,1]),function(x){strsplit(x,":")[[1]][1]}))
      
      p = ggplot(data=M,aes(x=year,y=x, colour = germplasm, group=germplasm)) + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line") 
      p = p + labs(x="année",y="note maladie")
      p = p + scale_y_continuous(breaks = c(1,2,3))
      return(p)
    })
    return("plot"=p)
  }else{return(NULL)}

}

}
