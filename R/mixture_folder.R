# 0. help -----------------------------------------------------------------
#' Function to get the "feedback file" containing the results of the mixtures trial for each farmer based on the analysis coming from \code{analyse_feedback_folder_1}
#' 
#' @param dir Directory where folder for each person is created
#' 
#' @param person The farmer's name
#' 
#' @param out_analyse_feedback_folder_1 The outputs from \code{analyse_feedback_folder_1}
#
#' @details 
#' The function creates two folders :
#' \itemize{
#'  \item "tex_files" with the tex files used to create the pdf
#'  \item "mixture_folder" with, for each person, a folder with information coming from shinemas2R::get.pdf() see ?get.pdf for more details.
#' }
#' 
#' @return Generate tex and pdf files
#' 
#' @author Pierre Rivière, Gaelle Van Frank
#' 
#' 
mixture_folder = function(
  dir = ".",
  person,
  pathway = ".",
  path_to_tables=".",
  vec_variables,
  out_analyse_feedback_folder_1,
  mix_to_delete)
{
# Set the right folder and create folders tex_files and feedback_folder ----------
  a = dir(dir)
  if( !file.exists(dir) ){ stop("directory ", dir, " does not exist.") }

  setwd(dir)
  we_are_here = getwd()
  if( !is.element("tex_files", dir()) ) { system("mkdir tex_files") ; message("The folder tex_files has been created") }
  if( !is.element("feedback_folder", dir()) ) { system("mkdir mixture_folder") ; message("The folder mixture_folder has been created") }
  
# Add info useful for feedback_folder_1
  p = system.file("extdata", "feedback_folder_1", package = "PPBformations")
  system(paste("cp ",p , "/* ", " ",we_are_here,"/tex_files", sep = ""))
  p = system.file("extdata", "mixture_folder", package = "PPBformations")
  system(paste("cp ",p , "/* ", " ",we_are_here,"/tex_files", sep = ""))
  message("Several files used in the tex document have been copied to tex_files folder")
  
  language="french"

    # get info from out_analyse_feedback_folder_1
    year = out_analyse_feedback_folder_1$year
    res_model1 = out_analyse_feedback_folder_1$res_model1
    res_model2 = out_analyse_feedback_folder_1$res_model2
    res_model_varintra = out_analyse_feedback_folder_1$res_model_varintra
    data_network_year = out_analyse_feedback_folder_1$data_network_year
    data_all =  out_analyse_feedback_folder_1$out_farmers_data[[person]]$data_all
    data_S_all = out_analyse_feedback_folder_1$data_all$data_S_all
    data_SR_all = out_analyse_feedback_folder_1$data_all$data_SR_all
    data_year =  out_analyse_feedback_folder_1$out_farmers_data[[person]]$data_year
    data_S_year =  out_analyse_feedback_folder_1$out_farmers_data[[person]]$data_S_year
    data_SR_year =  out_analyse_feedback_folder_1$out_farmers_data[[person]]$data_SR_year
    data_PPB_mixture = out_analyse_feedback_folder_1$out_farmers_data[[person]]$data_PPB_mixture
    Mixtures_all = out_analyse_feedback_folder_1$data_mixtures$Mixtures_all
    Mixtures_S = out_analyse_feedback_folder_1$data_mixtures$Mixtures_selection
    Mix_tot = out_analyse_feedback_folder_1$data_mixtures$Mix_tot
    data_mixtures = out_analyse_feedback_folder_1$data_mixtures
  
    

if(FALSE){
  year = get(load(paste(pathway,"out_year.RData",sep="/")))
  out_farmers_data = get(load(paste(pathway,"out_out_farmers_data.RData",sep="/")))
  data_all =  out_farmers_data[[person]]$data_all
  data_year =  out_farmers_data[[person]]$data_year
  data_S_year =  out_farmers_data[[person]]$data_S_year
  data_SR_year =  out_farmers_data[[person]]$data_SR_year
  data_PPB_mixture = out_farmers_data[[person]]$data_PPB_mixture
  res_model1 = get(load(paste(pathway,"out_res_model1.RData",sep="/")))
  res_model_varintra = get(load(paste(pathway,"res_varintra.RData",sep="/")))
  data_mixtures = get(load(paste(pathway,"out_data_mixtures.RData",sep="/")))
  Mixtures_all = data_mixtures$Mixtures_all
  Mixtures_S = data_mixtures$Mixtures_selection
  Mix_tot = data_mixtures$Mix_tot
  data_S_all =  get(load(paste(pathway,"data_S_all.RData",sep="/")))
  data_SR_all =   get(load(paste(pathway,"data_SR_all.RData",sep="/")))
}
    
  if(file.exists(paste(pathway,"mix_to_delete.RData",sep="/"))){mix_to_delete = get(load(paste(pathway,"mix_to_delete.RData",sep="/")))}else{mix_to_delete=FALSE}
  vec_variables_mod1 = names(res_model1)

  list_trad = list(
    c("poids.de.mille.grains","Poids de mille grains","Thousand kernel weight","tkw---tkw"),
    c("poids.de.l.epi","Poids de l'épi","Spike weight","spike_weight---spike_weight"),
    c("longueur.de.l.epi","Longueur de l'épi","Spike lenght","spike_lenght---spike_lenght"),
    c("LLSD","Distance dernière feuille-base de l'épi","LLSD","LLSD---LLSD"),
    c("hauteur","hauteur","plant height","plant_height---plant_height"),
    c("nbr.estime.grain.par.epi","Nombre moyen de grains par épi","Mean number of grain per spike"),
    c("taux.de.proteine","Taux de proteine","Protein content","protein---protein"),
    c("couleur---couleur_M","Couleur","Color","color---color_M"),
    c("barbe---barbe_M","Barbe","Awns","awns---awns_M"),
    c("courbure---courbure_M","Courbure","Curve","curve---curve_M"),
    c("rendement","Rendement","Yield","rendement"),
    c("verse","Verse","Lodging","verse"),
    c("Réseau","Sur le réseau","On the network")
  )
  
  # Verse et rendement
  if(length(grep("verse",vec_variables))>0){
    D=data_all$data$data
    if(!is.null(D$"verse---verse_2")){
      D$"verse" = unlist(lapply(D$"verse---verse_2",function(x){
        b=NULL
        if(!is.null(x) & x %in% c("à plat","a plat","à  plat","plat","100% a plat","100 a plat")){a=1 ; b=1}
        if(!is.null(x) & x %in% c("couché","couche","100% couché")){a=2 ; b=1}
        if(!is.null(x) & x %in% c("intermediaire","intermédiaire","intermédiaire- presque droit","100 intermédiaire","100% intermédiaire")){a=3 ; b=1}
        if(!is.null(x) & x %in% c("presque droit","presque droits","presque droit- droit")){a=4 ; b=1}
        if(!is.null(x) & x %in% c("droit","100 droit","100% droit")){a=5 ; b=1}
        if(is.null(x) | is.na(x)){a=NA}
        if(is.null(b)){a=NA}
        return(a)
      }))
    }
    data_all$data$data = D
  }
  
  if(length(grep("rendement",vec_variables))>0){
    data_all$data$data$"rendement" = data_all$data$data$"rdt_parcelle---rdt_parcelle"
    data_all$data$data$"rendement" = gsub(",",".",data_all$data$data$"rendement")
  }

  
# Créer title page --------
  a = paste(
    "	\\begin{titlepage}
    \\pagecolor{color1}
    \\noindent
    \\begin{center}
    \\begin{tabular}{cccc}
    \\includegraphics[width=.25\\textwidth, height=.25\\textwidth]{", we_are_here, "/tex_files/sp4} &
    \\includegraphics[width=.25\\textwidth, height=.25\\textwidth]{", we_are_here, "/tex_files/sp6} &
    \\includegraphics[width=.25\\textwidth, height=.25\\textwidth]{", we_are_here, "/tex_files/sp7} &
    \\includegraphics[width=.25\\textwidth, height=.25\\textwidth]{", we_are_here, "/tex_files/sp2}
    \\end{tabular}
    \\end{center}
    %\\[-1em]
    \\color{color2}
    \\makebox[0pt][l]{\\rule{1.3\\textwidth}{1pt}}
    \\par
    \\noindent
    \\textbf{\\textsf{Réseau Semences Paysannes}} \\\\ 
    \\textbf{\\textsf{Equipe DEAP, INRA Le Moulon}} \\\\
    \\textbf{\\textsf{CETAB}} \\\\
    \\textbf{\\textsf{Triptolème}} \\\\
    \\textbf{\\textsf{Pétanielle}} \\\\
    \\textbf{\\textsf{Touzelle}} \\\\
    \\textbf{\\textsf{ARDEAR Rhone-Alpes}} \\\\
    \\textbf{\\textsf{ARDEAR Centre}} \\\\
    \\textbf{\\textsf{Bergerie de Villarceaux}} \\\\
    \\textbf{\\textsf{Graines de Noé}} \\\\
    \\textbf{\\textsf{Li mestère}} \\\\
    \\textbf{\\textsf{ADEAR 32}} \\\\
    \\textbf{\\textsf{ARDEAR NPDC}} \\\\
    \\textbf{\\textsf{GAB 65}} \\\\
    
    
    \\vfill
    \\noindent
    {\\Huge \\textbf{\\textsf{Programme de sélection décentralisée \\\\ et collaborative sur le blé tendre}}} \\\\
    ~\\\\
    \\noindent{\\Large \\textbf{\\textsf{Résultats de l'essai de sélection pour les mélanges, saison ",as.numeric(year)-1,"-",year,"}}}
    \\vfill
    
    \\begin{flushright}
    \\noindent
    \\textbf{\\textsf{", person,"}}
    \\end{flushright}
    
    \\end{titlepage}
    
    \\nopagecolor % Use this to restore the color pages to white
    \\pagestyle{plain}
    ",
    sep=""
  )
  
  
  p = paste(we_are_here, "/tex_files/titlepage_", person,".tex", sep = "")
  sink(p);	cat(a);	sink()
  
  OUT = list()
  
  # 0. Page de garde, contacts, table des matières ----------
  
  out = list("input" = paste("../tex_files/titlepage_", person, ".tex", sep = "")); OUT = c(OUT, out)
  
  
  # Contacts --------
  a=paste("	\\chapter*{Qui contacter ?}
          
          \\vfill
          
          \\noindent\\textbf{\\textsf{Correspondants nationaux :}} \\\\
          
          \\begin{wrapfigure}{l}{.20\\textwidth}
          \\begin{center} \\vspace{-20pt}
          \\includegraphics[width=.20\\textwidth]{",we_are_here,"tex_files/Logo-RSP.png}
          \\end{center} \\vspace{-20pt}
          \\end{wrapfigure}
          \\noindent
          ~\\\\
          Pierre Rivière \\href{mailto:pierre@semencespaysannes.org}{pierre@semencespaysannes.org} \\\\
          Patrick de Kochko \\href{mailto:patrick@semencespaysannes.org}{patrick@semencespaysannes.org} \\\\
          Réseau Semences Paysannes \\\\
          3, avenue de la Gare 47190 Aiguillon \\\\
          ~\\\\
          
          \\vfill
          
         % \\begin{wrapfigure}{l}{.20\\textwidth}
          \\begin{center} \\vspace{-20pt}
          \\includegraphics[width=.20\\textwidth]{",we_are_here,"tex_files/Logo-UMRGV.jpg}
          \\end{center} \\vspace{-20pt}
         % \\end{wrapfigure}
          \\noindent
          %~\\\\
          Isabelle Goldringer \\href{mailto:isabelle.goldringer@inra.fr}{isabelle.goldringer@inra.fr} \\\\
          Gaëlle van Frank \\href{mailto:gaelle.van-frank@inra.fr}{gaelle.van-frank@inra.fr} \\\\
          Equipe DEAP, INRA Le Moulon  \\\\
          Ferme du Moulon, 91190 Gif sur Yvette \\\\
         % ~\\\\

          \\vfill
          
          \\newpage
          
          \\noindent\\textbf{\\textsf{Correspondants régionaux :}} \\\\
          
          \\begin{longtable}{p{.5\\textwidth}p{.5\\textwidth}}
          
          \\textbf{ARDEAR Rhône-Alpes}	& \\textbf{Graines de Noé} \\\\
          Alexandre HYACINTHE & Hélène MONTAZ \\\\
          \\href{mailto:ardear.semences@wanadoo.fr}{ardear.semences@wanadoo.fr} & \\href{mailto:technique.grainesdenoe@gmail.com}{technique.grainesdenoe@gmail.com} \\\\
          58 rue Raulin	69007	Lyon & Technopole Agro-Environnement \\\\
          04 72 41 79 22	& Agronov RD-31, 21110	Bretenière \\\\
          & 03 80 56 37 07 - 07 70 45 43 12 \\\\
          & \\\\
          & \\\\
          
          \\textbf{ARDEAR Centre}	& \\textbf{ARDEAR Nord} \\\\
          Sophie WOEHLING	& Clémentine HEITZ \\\\	
          \\href{mailto:ardearcentre.semencespaysannes@gmail.com}{ardearcentre.semencespaysannes@gmail.com} & \\href{mailto:semencespaysannes@adearn.fr}{semencespaysannes@adearn.fr} \\\\
          Village d'entreprises de l'Arrou, 87A Route de Château-Renault,	41000	Blois & 40 avenue Salengro	62223	Saint Laurent Blangy	\\\\
          02 54 43 32 94 - 06 79 29 13 95 & 09 77 95 56 78	
          & \\\\
          & \\\\
          
          \\textbf{ADEAR 32} & \\textbf{Li mestère}	\\\\
          Charleyne BARTHOMEUF & 	Sofia Costa Santos BALTAZAR \\\\	
          \\href{mailto:adear32@free.fr}{adear32@free.fr} & \\href{mailto:sofia.baltazar@unamur.be}{sofia.baltazar@unamur.be} \\\\
          1 rue Dupont de l'Eure,	32000	Auch	& 48 Rue Albert Billy	5370	Porcheresse	Belgique	\\\\
          05 62 05 30 86 - 06 87 58 35 95 & \\\\
           & \\\\
           & \\\\
          
          
          \\textbf{GAB 65}	& \\\\
          Frédéric FURET	& \\\\
          frederic.furet.gab65@gmail.com		& \\\\
          Chemin de Lalette, BP449	65004	Tarbes Cedex & \\\\
          05 62 35 27 73 - 06 80 18 26 29 & \\\\
          
          \\end{longtable}
          
          \\vfill
          
          %\\centering\\textsf{Fait à Aiguillon le~\\today}",sep="/")
  
  p = paste(we_are_here, "/tex_files/contacts", ".tex", sep = "")
  sink(p);	cat(a);	sink()
  
  out=list("input" = "../tex_files/contacts.tex")
  OUT=c(OUT,out)
  
  # Table of contents
  out = list("tableofcontents" = TRUE); OUT = c(OUT, out)
  
  
  # 1. Intro ----------------------------------------------------------------------------------------------------------------------------------------------------
  # 1.1 Pourquoi ce dossier -----
  a=paste("	\\chapter{Pourquoi ce dossier?}
      Cet essai, mis en place depuis l'automne 2015, vise à comparer les effets de différentes pratiques de sélection des mélanges sur leur comportement. 
      Chaque paysan choisi les populations qu'il souhaite assembler, c'est à partir de ce(s) mélange(s) que sont testées différentes pratiques de sélection. \\\\
  Trois pratiques sont étudiées (voir schéma ci-dessous) : 
            \\begin{enumerate}
             \\item deux années de sélection massale dans les composantes avant de mélanger ces sélections (Modalité 1 sur le schéma) ; 
             \\item une année de sélection massale dans les composantes avant de les mélanger puis une sélection massale dans le mélange (Modalité 2) ; 
             \\item à partir du mélange créé sans sélection dans les composantes, deux années de sélection massale dans le mélange (Modalité 3).
           \\end{enumerate}
             Ces pratiques de sélection sont comparées au mélange évoluant sans sélection massale (Modalité 4). \\\\

            Le dispositif expérimental est adapté à partir de celui utilisé en routine, avec le(s) témoin(s) répété(s) deux fois, le(s) mélange(s) répété(s) deux fois et les composantes non répétées.
            Cet essai peut tout à fait être semé dans la même parcelle que des populations que vous voudriez tester qui ne seraient pas intégrées dans le mélange, car les témoins utilisés sont les mêmes.
             Ce dossier présente les résultats des premières années d'essai. Si vous voulez mettre en place cet essai chez vous, c'est tout à fait possible,
          contactez votre animateur ou l'équipe de recherche ! \\\\

          Ce dossier présente les résultats des essais depuis la saison 2015-2016. N'hésitez pas à nous faire part de vos réflexions à la lecture de ce dossier (choses à améliorer, élements
          qui vous semblent cohérent par rapport à ce que vous observez chez vous ou au contraire qui s'en éloignent, si ces résultats soulèvent des interrogations, ...).
          \\begin{enumerate}
          \\item \\textbf{Une première partie traite des résultats dans votre ferme.}
          Si vous avez mis en place un essai mélange chez vous la première partie reprend l'ensemble des résultats sur votre ferme : 
          \\begin{itemize}
          \\item la comparaison du comportement du mélange non sélectionné par rapport aux comportement des composantes
          \\item la comparaison des différentes modalités de sélection des mélanges à partir de la 2ème année d'essai
          \\end{itemize}
          \\item \\textbf{Une deuxième partie traite des résultats dans le réseau de fermes.}
         La seconde partie présente les résultats sur l'ensemble du réseau de fermes participant à l'essai :
         \\begin{itemize}
              \\item  Quel est globalement le comportement des mélanges par rapport à celui des composantes cultivées \"en pur\" ?
              \\item Dans ce dispositif quels caractères ont été sélectionnés par les paysans ?
              \\item Quel sont les impacts des patiques de sélection des mélanges sur les caractères mesurés ?
          \\end{itemize}
          \\end{enumerate}

          \\begin{figure}[!h]
          \\begin{center} 
          \\includegraphics[width=.80\\textwidth]{",we_are_here,"tex_files/dispositifMelanges.png}
          \\caption{Schéma du dispositif sur 3 ans : les rectangles représentent les parcelles (plein : mélange ; vide : composante) 
          et les flèches une sélection (trait plein) ou une multiplication sans sélection massale (trait pointillé). 
          Sur les flèches sont indiqués les noms données aux lots de graines sélectionnés en fonction de la modalité de sélection (#VA,JA...). 
          Les fermes satellites mettent en places les modalités 2, 3 et 4 tandis que les fermes régionales mettent en place l'ensemble des modalités.}
          \\end{center} 
          \\label{SchemaRecap}
          \\end{figure}

  \\newpage",sep="/")
  
  p = paste(we_are_here, "/tex_files/intro_melanges", ".tex", sep = "")
  sink(p);	cat(a);	sink()
  
  out=list("input" = "../tex_files/intro_melanges.tex")
  OUT=c(OUT,out)
  

  # 2. Essai Mélanges --------------------------------------------------------------------------------------------------------------------------------------
  out = list("chapter" = list("text"="Résultats de l'essai mélanges")); OUT = c(OUT, out)

  # 2.1. Résultats sur la ferme -----
  out = list("section" = list("text"="Résultats sur la ferme")); OUT = c(OUT, out)
  if (is.null(data_PPB_mixture$data)) { 
    out = list("text" = "Vous n'avez pas mis en place l'essai de sélection pour les mélanges sur votre ferme."); OUT=c(OUT,out)
  }else{
    cat("Résultats sur la ferme -----------------------------------------------------   \n")
    out=list("text"="Les graphiques suivants présentent, pour chaque caractère mesuré et pour chaque mélange testé, les valeurs des différentes modalités de sélection des mélanges 
    ainsi que des composantes et la valeur moyenne des composantes si celles-ci ont été semées. 
             Les populations qui appartiennent au même groupe (représenté par une même lettre) ne sont pas significativement différentes.") ; OUT=c(OUT,out)
    
    ## par mélange, donner d'abord la comparaison mélange/composantes, puis différentiel de sélection, puis réponse à la sélection pour chaque mélange et chaque caractère
    Mixtures_all_person = Mixtures_all$data[Mixtures_all$data$location %in% person,]
    Mixtures_all_person$expe_melange = unlist(lapply(1:nrow(Mixtures_all_person),function(i){
      m = Mixtures_all_person[i,]
      if(m$sl_statut == "son"){return(m$expe_melange)}
      if(m$sl_statut == "father"){
        return(unique(Mixtures_all_person[grep(m$expe,Mixtures_all_person$mixture_id),"expe_melange"]))
      }
    }))

    mix = plyr:::splitter_d(Mixtures_all_person, .(expe_melange))

    graphs_ferme_melanges = function(OUT,variable,titre,mel){
   #   out = list("subsection" = titre); OUT = c(OUT, out)
      
      # Comparaison mélange vs composantes
      if(variable %in% vec_variables_mod1){res_model = res_model1 ; model="model_1"}else{res_model=data_all ; model=NULL}
      p_melanges = ggplot_mixture1(res_model = res_model, melanges_PPB_mixture = mel, data_S = Mixtures_S, melanges_tot = Mix_tot, variable, year=c("2016","2017"), model = model, 
                                   plot.type = "comp.in.farm", person, nb_parameters_per_plot = 20, save=NULL, language=language, tab_proportions = tab_proportions)
      
      p = lapply(p_melanges, function(x){
        lapply(x,function(y){
          lapply(y,function(z){return(z$plot)})
        })
      })
      a = do.call(c,unlist(p,recursive = FALSE))
      if(length(a)>1){a = do.call(c,a)}
  #   for (i in 1:length(p_melanges[[1]])){
   #     for (yr in 1:length(p_melanges[[1]][[i]])){
   #       if(!is.null(p_melanges[[1]][[i]][[yr]]$plot)){
            i = ifelse(language=="french",2,3)
            out = list("figure" = list("caption" = paste("Comparaison du \\textbf{",list_trad[[grep(variable,list_trad)]][i],"} du(des) mélange(s) et de ses(leurs) composantes (si les composantes ont été semées) et comparaison
                                                          des différents modalités de sélection du(des) mélange(s) (si ces modalités ont été semées). 
                                                       Les populations qui appartiennent le même groupe (représenté par une même lettre) ne sont pas significativement différentes (risque de se tromper de 5%).
                                                       ",sep=""), "content" = a, "layout" = matrix(c(1,2), ncol = 1), "width" = 1, "landscape" = FALSE))
            OUT = c(OUT, out)#}
          
      #  }
    #  }
     
      # Différentiel de sélection
      # Correspondance data_S et mix
      
      
      
      # Comparaison modalités de sélection
      if(length(a)<2){
        if(variable %in% vec_variables_mod1){res_model = res_model1 ; model="model_1"}else{res_model=data_all ; model=NULL}
        p_melanges = ggplot_mixture1(res_model = res_model, melanges_PPB_mixture = mel, data_S = Mixtures_S, melanges_tot = Mix_tot, variable, year=c("2016","2017"), model = model,
                                     plot.type = "comp.mod", person, nb_parameters_per_plot = 20, save=NULL,language=language,tab_proportions = tab_proportions)
        p = lapply(p_melanges, function(x){
          lapply(x,function(y){
            lapply(y,function(z){return(z$plot)})
          })
        })
        a = do.call(c,unlist(p,recursive = FALSE))
        if(length(a)>1){a = do.call(c,a)}
        if(length(a)>0){
          out = list("figure" = list("caption" = paste("Comparaison du \\textbf{",variable,"} des différentes modalités de sélection des mélanges.
                                                       Les modalités qui partagent le même groupe (représenté par une même lettre) ne sont pas significativement différentes.
                                                       ",sep=""), "content" = a, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)}
        
        }
      
      return(OUT)
    }
    
    for(i in 1:length(mix)){
      print(i)
      x=mix[[i]]
      out = list("subsection" = list("text"=unique(x$expe_melange))); OUT=c(OUT,out)
      for (variable in vec_variables){
        print(variable)
        if(variable %in% vec_variables_mod1  |  variable %in% colnames(data_all$data$data))
        OUT = graphs_ferme_melanges(OUT,variable, titre=variable, mel = x)
      }
    }
    
    
    # Ajouter les différentiels de sélection !
    
} #end resultats sur la ferme
  
  # 2.2. Résultats sur le réseau de fermes -----
  cat("Résultats sur le réseau ---------------------------------------   \n")
  out = list("section" = list("text"="Résultats sur le réseau de fermes")); OUT = c(OUT, out)
  out = list("text" = "Dans cette partie sont présentés les résultats de l'essai mélange sur le réseau de fermes. 
             Dans un premier temps on s'intéresse au comportement des mélanges comparé à la moyenne de leurs composantes : \\textbf{y a-t-il globalement un bénéfice à cultiver des mélanges ?} \\\\
             Dans un second temps on étudie le différentiel de sélection moyen sur le réseau : \\textbf{quels caractères sont sélectionnés globalement par les paysans ?} \\\\
             Enfin on compare les différentes modalités de sélection des mélanges : \\textbf{y a-t-il une méthode qui permette d'obtenir des réponses plus intéressantes ? 
             Quel est l'impact des pratiques de sélection des mélange sur la diversité phénotypique ?}"
             ); OUT=c(OUT,out)
  
  # 2.2.0. Explication des symboles de significativité --------
  out = list("subsection" = list("text" = "Explication des notations utilisées pour rendre compte de la significativité des tests de comparaison",
                                 "cross_ref" = "SymbolesSignif")); OUT = c(OUT, out)
  out = list("text" = "Dans les tableaux et gaphiques suivant seront comparés différentes populations/mélanges : si ces populations/mélanges sont significativement différents, c'est à dire que l'on a un faible risque de
se tromper en déclarant qu'ils sont différents, cela sera indiqué par des symboles ou des couleurs qui seront différents selon le niveau de risque que l'on prend. Par exemple, * signifie que l'on a entre 1% et 5% de chances
de se tromper, tandis que *** signifie que l'on a moins de 0.1% de chances de se tromper. Le tableau suivant récapitule la signification des différents symboles. 
Concernant les graphiques, ce niveau de risque sera indiqué par des couleurs différentes dont la signification sera précisée dans la légende. Par exemple, s'il est indiqué \"significatif à 0.01\" cela signifie qu'on a 
un risque de se tromper entre 0.1% et 1%.
  "); OUT=c(OUT,out)
  
  out = list("text" = "
\\begin{table}[ht]
\\centering
\\begin{tabular}{ll}
  \\hline
  Symbole & Signification \\\\ 
  \\hline
     & Différence non significative \\\\
   . & Différence significative à 0.1 : risque de se tromper entre 5 et 10% \\\\ 
   \\** & Différence significative à 0.05 : risque de se tromper entre 1 et 5% \\\\ 
   \\*** & Différence significative à 0.01 : risque de se tromper entre 0.1 et 1% \\\\ 
   \\**** & Différence significative à 0.001 : risque de se tromper inférieur à 0.1% \\\\ 
   \\hline
\\end{tabular}
\\caption{Explication des symboles utilisés pour caractériser la significativité des tests de comparaison.}
\\label{Signif}
\\end{table}
             "); OUT=c(OUT,out)
  
  
  # 2.2.1. Tableau récapitulatif -----
  cat("tableaux ------------------------------------------------------ \n")
  out=list("subsection" = list("text"="Résultats globaux sur le réseau")); OUT = c(OUT, out)
  out=list("text"="Les tableaux suivant présentent les résultats sur l'ensemble des mélanges testés. Y sont reportés : 
\\begin{itemize}
\\item le nombre de mélanges testés pour chaque caractère (tableau \\ref{Compmel}), 
\\item pour chaque caractère mesuré, la proportion de mélanges pour lesquels la valeur du mélange est supérieure à la valeur moyenne de ses composantes ainsi 
que la proportion de mélanges pour lesquels la valeur du mélange est supérieure à sa composante la plus haute et sa composante la plus basse (tableau \\ref{Compmel}),
\\item  les valeurs moyennes des mélanges et des composantes, ainsi que le gain (ou la perte) moyen.ne des mélanges par rapport à la moyenne de leurs composantes respectives 
(tableau \\ref{OverY}), calculé ainsi : \\\\
Moyenne sur l'ensemble des mélanges de $\\frac{Valeur mélange - Valeur moyenne des composantes}{Valeur moyenne des composantes}$ 
\\end{itemize}
\\\\
Enfin, on cherche à savoir s'il y a des corrélations entre les différences observés (mélange vs moyenne des composantes) et le nombre de composantes dans le mélange et la variabilité du caractère au sein des différentes composantes (tableau \\ref{OverY}) et
si on détecte des corrélations entre ces différences pour les différents caractères mesurés (tableau \\ref{CorrelOverY}). \\\\

\\emph{On remarque en particulier :}

\\begin{itemize}
\\item On a peu de cas pour lesquels le mélange est inférieur à la composante la plus basse : on prend donc moins de risques à semer un mélange qu'à miser sur une variété 
semée en pur sans connaître les conditions de culture de l'année (Table \\ref{Compmel}).
\\item Pour la majorité des caractères, 2/3 à 3/4 des mélanges sont supérieurs aux moyennes des composantes sauf pour le PMG et la teneur en protéine pour lesquels seulement 40% des mélanges sont supérieurs.
\\item Pour la plupart des caractères on a une différence positive et significatif des mélanges par rapport à la moyenne de leurs composantes respectives. On observe une  faible perte en PMG globalement
des mélanges par rapport à la moyenne des composantes respectives : en compétition avec des génotypes différents les plantes produisent plus de grains plus petits (Table \\ref{OverY}).
\\item Il n'y a qu'une seule corrélation significative et importante : la différence en hauteur du mélange et de la moyenne de ses composantes par rapport à la variabilité de hauteur des composantes. 
On peut émettre l'hypothèse que lorsqu'on a une grande variabilité de hauteur les composantes les plus hautes prennent le dessus sur les composantes les plus basses, 
donnant alors une hauteur moyenne du mélange plus importante que la hauteur moyenne des composantes (Table \\ref{OverY}).
\\item On ne constate pas de corrélation négative entre les gains en poids de mille grains et taux de protéine (Table \\ref{CorrelOverY}), 
ou entre le nombre de grains par épi et le taux de protéine : un gain de pmg ou de nombre de grains par épi du mélange n'est pas nécessairement associé à une perte en taux de protéine.
\\item On observe des corrélations entre le poids de l'épi, la longueur de l'épi et le nombre de grains par épis mais pas entre le poids de l'épi et le PMG : l'augmentation du poids des épis des mélanges se fait grâce à une augmentation de longueur des épis et de nombre de grains par épi
\\end{itemize}
           ")
  OUT=c(OUT,out)
  
# 2.2.1.1 Résultats globaux : comparaisons mélanges / composantes 

    Table = get_mixture_tables(res_model1, year=NULL, year_DS=NULL, year_RS=NULL,
                           mix_to_delete=mix_to_delete,
                           language=language,
                           data_mixtures=data_mixtures,
                           vec_variables = intersect(vec_variables,vec_variables_mod1), 
                           data_S_all = NULL, 
                           data_SR_all = NULL, 
                           path_to_tables = path_to_tables,
                           list_trad,
                           table.type="distribution")

  if(language == "french"){
    Table[c("Proportion mélanges > moyenne des composantes","Proportion mélanges < composante la plus basse","Proportion mélanges > composante la plus haute"),] = apply(Table,2,function(x){
      p1 = paste(round(as.numeric(as.character(x["Proportion mélanges > moyenne des composantes"]))*as.numeric(as.character(x["Nombre de mélanges"]))/100,1)," - ", 
                 round(as.numeric(as.character(x["Proportion mélanges > moyenne des composantes"])),1), "%"," (", x["nombre de gains significatifs"],")",sep="")
      p2 = paste(round(as.numeric(as.character(x["Proportion mélanges < composante la plus basse"]))*as.numeric(as.character(x["Nombre de mélanges"]))/100,1)," - ", 
                 round(as.numeric(as.character(x["Proportion mélanges < composante la plus basse"])),1), "%",sep="")
      p3 = paste(round(as.numeric(as.character(x["Proportion mélanges > composante la plus haute"]))*as.numeric(as.character(x["Nombre de mélanges"]))/100,1)," - ", 
                 round(as.numeric(as.character(x["Proportion mélanges > composante la plus haute"])),1), "%",sep="")
      return(c(p1,p2,p3))})
    Table = Table[-grep("nombre de gains significatifs",rownames(Table)),]
    if(!is.null(dim(Table))){
      Table[c("Moyenne des composantes","Moyenne des mélanges","Gain moyen"),] = t(apply(Table[c("Moyenne des composantes","Moyenne des mélanges","Gain moyen"),],1,function(x){return(round(as.numeric(as.character(x)),2))}))
      Table["Gain moyen",] = apply(Table,2,function(x){paste(x["Gain moyen"],"% ",x["stars"]," (+/-",round(as.numeric(as.character(x["Ec.Type gain"])),1),"% ",")",sep="") })
      Table=Table[-grep("pvalue|stars|Type",rownames(Table)),]
      rownames(Table)[grep("Proportion",rownames(Table))] = gsub("Proportion","",rownames(Table)[grep("Proportion",rownames(Table))])
      Table=cbind(rownames(Table),Table)
      colnames(Table)[1]=" "
    }else{
      Table[c("Moyenne des composantes","Moyenne des mélanges","Gain moyen")] = round(as.numeric(as.character(Table[c("Moyenne des composantes","Moyenne des mélanges","Gain moyen")])),2)
      Table["Gain moyen"] = paste(Table["Gain moyen"],"% ",Table["stars"]," (+/-",round(as.numeric(as.character(Table["Ec.Type gain"])),1),"% ",")",sep="")
      Table=Table[-grep("pvalue|stars",names(Table))]
      names(Table)[grep("Proportion",names(Table))] = gsub("Proportion","",names(Table)[grep("Proportion",names(Table))])
      Table=cbind(names(Table),Table)
      colnames(Table)[2] = intersect(vec_variables,vec_variables_mod1)
      colnames(Table)[1]=" "
    }
    
    Table1 = Table[grep("Nombre|>|<",rownames(Table)),]
    Table2 = Table[-grep("Nombre|>|<",rownames(Table)),]
  }else if(language == "english"){
    Table[c("Proportion blend > components' mean","Proportion blend < lowest component","Proportion blend > highest component"),] = apply(Table,2,function(x){
      p1 = paste(round(as.numeric(as.character(x["Proportion blend > components' mean"]))*as.numeric(as.character(x["Number of blends"]))/100,1)," - ", 
                 round(as.numeric(as.character(x["Proportion blend > components' mean"])),1), "%"," (", x["number significant overyieldings"],")",sep="")
      p2 = paste(round(as.numeric(as.character(x["Proportion blend < lowest component"]))*as.numeric(as.character(x["Number of blends "]))/100,1)," - ", 
                 round(as.numeric(as.character(x["Proportion blend < lowest component"])),1), "%",sep="")
      p3 = paste(round(as.numeric(as.character(x["Proportion blend > highest component"]))*as.numeric(as.character(x["Number of blends "]))/100,1)," - ", 
                 round(as.numeric(as.character(x["Proportion blend > highest component"])),1), "%",sep="")
      return(c(p1,p2,p3))})
    Table = Table[-grep("number significant overyieldings",rownames(Table)),]
    if(!is.null(dim(Table))){
      Table[c("Mean components","Mean blends","Mean overyielding"),] = t(apply(Table[c("Mean components","Mean blends","Mean overyielding"),],1,function(x){return(round(as.numeric(as.character(x)),2))}))
      Table["Mean overyielding",] = apply(Table,2,function(x){paste(x["Mean overyielding"],"% ",x["stars"]," (+/-",round(as.numeric(as.character(x["Ec.Type gain"])),1),"% ",")",sep="") })
      Table=Table[-grep("pvalue|stars",rownames(Table)),]
      rownames(Table)[grep("Proportion",rownames(Table))] = gsub("Proportion","",rownames(Table)[grep("Proportion",rownames(Table))])
      Table=cbind(rownames(Table),Table)
      colnames(Table)[1]=" "
    }else{
      Table[c("Mean components","Mean blends","Mean overyielding")] = round(as.numeric(as.character(Table[c("Mean components","Mean blends","Mean overyielding")])),2)
      Table["Mean overyielding"] = paste(Table["Mean overyielding"],"% ",Table["stars"]," (+/-",round(as.numeric(as.character(x["Ec.Type gain"])),1),"% ",")",sep="")
      Table=Table[-grep("pvalue|stars",names(Table))]
      names(Table)[grep("Proportion",names(Table))] = gsub("Proportion","",names(Table)[grep("Proportion",names(Table))])
      Table=cbind(names(Table),Table)
      colnames(Table)[2] = intersect(vec_variables,vec_variables_mod1)
      colnames(Table)[1]=" "
    }
    
    Table1 = Table[grep("Nombre|>|<",rownames(Table)),]
    Table2 = Table[-grep("Nombre|>|<",rownames(Table)),]
  }

  
  attributes(Table1)$invert =FALSE
  out = list("table" = list("caption" = "Comparaison des mélanges avec leurs composantes respectives par caractère sur le réseau. 
Pour les comparaisons sont indiqués en premier lieu le nombre de mélanges concernés ainsi que la proportion que cela représente par rapport au nombre de mélanges testés.
Pour la comparaison du mélange à la moyenne des composantes est indiqué entre parenthèse le nombre de cas pour lesquels la valeur du mélange est significativement supérieure
à la valeur moyenne des composantes (risque de se tromper inférieur à 5%). \\\\
\\emph{Interprétation :} on a peu de cas pour lesquels le mélange est inférieur à la composante la plus basse : on prend donc moins de risques à semer un mélange qu'à miser sur une variété 
semée en pur sans connaître les conditions de culture de l'année. Pour la majorité des caractères, 2/3 à 3/4 des mélanges sont supérieurs aux moyennes des composantes sauf pour le PMG 
et la teneur en protéine pour lesquels seulement 40% des mélanges sont supérieurs.
                            ", "content" = list(Table1),"landscape"=TRUE,"tab.lab"="Compmel")) ; OUT=c(OUT,out)
  
# 2.2.1.2. Résultats globaux : Overyieldings et corrélations
  Table = get_mixture_tables(res_model=res_model1,
                             res_model_varintra = NULL,
                             table.type="correlations",
                             year=NULL,
                             year_DS=NULL,
                             year_RS=NULL,
                             mix_to_delete=mix_to_delete,
                             language="english",
                             data_mixtures,
                             vec_variables = intersect(vec_variables,vec_variables_mod1), 
                             data_S_all=data_S_all, 
                             data_SR_all=NULL, 
                             path_to_tables = ".",
                             list_trad=list_trad,
                             tab_proportions = tab_proportions
                             )
  
  Tab = apply(Table$Correlations,2,function(x){
    return(c(paste(round(x[grep("Rcorr overyielding~Nb",names(x))],2), get_stars(x[grep("pvalue overyielding~Nb",names(x))])),
             paste(round(x[grep("Rcorr overyielding~Variance",names(x))],2), get_stars(x[grep("pvalue overyielding~Variance",names(x))]))))
  })
  if(language=="french"){rownames(Tab) = c("Corrélation gain ~ Nombre de composantes","Corrélation gain ~ variabilité dans les composantes")}
  if(language=="english"){rownames(Tab) = c("Correlation overyielding ~ Number of components","Correlation overyielding ~ variability across components")}
  Tab = cbind(rownames(Tab),Tab)
  Table2 = rbind(Table2,Tab)
  attributes(Table2)$invert =FALSE
  out = list("table" = list("caption" = "Résultats globaux par caractère sur l'ensemble des mélanges. La ligne du milieu présente la différence moyenne des mélanges
comparé à leurs composantes respectives (une valeur positive indique un gain, une valeur négative une perte). Entre parenthèses est indiqué l'écart-type des différences
sur l'ensemble des mélanges testés. Les deux dernière lignes présentent les corrélations entre la différence mélange/moyenne des composantes et le nombre de composantes dans le mélange 
ou la variabilité du caractère au sein des composantes : une valeur proche de 0 indique qu'il n'existe pas de corrélation, tandis qu'une valeur proche de 1 indique une forte corrélation. 
Les symboles indiquent si le gain moyen et les corrélations sont significatifs (voir tableau \\ref{Signif} pour l'explication des symboles utilisés). 
\\\\
\\emph{Interprétation :} Pour la plupart des caractères on a un différence positive significative des mélanges par rapport à la moyenne de leurs composantes respectives. 
Une seule corrélation significative : le gain en hauteur du mélange lié à une plus grande variabilité de hauteur des composantes. 
Hypothèse : lorsqu'on a une grande variabilité de hauteur les composantes les plus hautes prennent le dessus sur les composantes les plus basses.
                            ", "content" = list(Table2),"landscape"=TRUE,"tab.lab"="OverY")) ; OUT=c(OUT,out)
  
# 2.2.1.3. Corrélations entre overyieldings
  Table = Table$overyielding
  Tab=matrix(NA,ncol=ncol(Table)-1,nrow=ncol(Table)-1)
  if(nrow(Tab)>1){
    for(i in 2:(ncol(Table)-1)){
      for(j in (i+1):ncol(Table)){
        t=Table[,c(i,j)]
        R = rcorr(as.numeric(as.character(t[,1])),as.numeric(as.character(t[,2])))
        Tab[(i-1),(j-1)] = paste(round(R$r[1,2],3),get_stars(R$P[2]),sep=" ")
      }
    }
    Tab[,1] = gsub("[.]"," ",colnames(Table)[-1]) ; colnames(Tab)[2:ncol(Tab)]=colnames(Table)[-c(1,2)]
    Tab = Tab[-nrow(Tab),]
    
    attributes(Tab)$invert =FALSE
    out = list("table" = list("caption" = "Corrélations entre gains des différents caractères mesurés : une valeur proche de 0 indique qu'il n'existe pas de corrélation, 
tandis qu'une valeur proche de 1 indique une forte corrélation. Voir tableau \\ref{Signif} pour l'explication des symboles utilisés.
\\\\
\\emph{Interprétation :} un gain de pmg ou de nombre de grains par épi du mélange n'est pas nécessairement associé à une perte en taux de protéine.
L'augmentation du poids des épis des mélanges se fait grâce à une augmentation de longueur des épis et de nombre de grains par épi.
                            ", "content" = list(Tab),"landscape"=TRUE,"tab.lab"="CorrelOverY")) ; OUT=c(OUT,out)
    
    
  }

  
  
  
  # 2.2.2. Distribution du gain du mélange par rapport à la moyenne de ses composantes sur le réseau -----
  cat("Histogrammes overyielding --------------------------------------------------------------- \n")
  out = list("subsection" = list("text"="Distribution du gain du mélange par rapport à la moyenne de ses composantes sur le réseau")); OUT = c(OUT, out)
  out = list("text" = "Ces graphiques présentent la distribution des différences de comportementnt des mélanges par rapport à la moyenne de leurs composantes respectives. 
              Chaque élément de ces histogrammes représente une comparaison entre un mélange et la moyenne de ses composantes, la couleur de chaque élément indique
              si la différence entre mélange et ses composantes est significative ou non (voir légende de chaque graphique et voir partie \\ref{SymbolesSignif} pour l'explication de la significativité des tests de comparaison).
             Un histogramme décalé vers la droite par rapport à 0 indique qu'une majorité des mélanges ont eu des valeurs plus fortes que la moyenne de leurs composantes. 
             A l'inverse si l'histogramme est décalé vers la gauche la majorité des mélanges ont eu des valeurs plus faibles que la moyenne de leurs composantes.
             Entre parenthèses est indiqué si le gain moyen est significativement différent de 0 : voir tableau \\ref{Signif} pour l'explication des symboles utilisés."); OUT = c(OUT, out)

P = list()
for (variable in intersect(vec_variables,vec_variables_mod1)){
  var = paste(strsplit(variable,"[.]")[[1]],collapse="")
  print(var)
  if(!is.null(mix_to_delete)){if(length(grep(paste(mix_to_delete,collapse="|"),Mixtures_all$data$son_germplasm))>0){Mixtures_all$data = Mixtures_all$data[-grep(paste(mix_to_delete,collapse="|"),Mixtures_all$data$son_germplasm),]}}
  if (!file.exists(paste(we_are_here,"/mixture_folder/figures/Histo_",var,".RData",sep=""))){
    p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, data_S = Mixtures_S, melanges_tot = Mix_tot, variable, 
                                 year=c("2016","2017"), model="model_1", plot.type = "mix.gain.distribution", person, nb_parameters_per_plot = 15,
                                 save=paste(we_are_here,"/AnalyseDonnees/donnees_brutes",sep=""), language=language,tab_proportions = tab_proportions)
    save(p_melanges,file=paste(we_are_here,"/mixture_folder/figures/Histo_",var,".RData",sep=""))
    P=c(P,list(p_melanges$plot$plot))
    png(paste(we_are_here,"/mixture_folder/figures/Histo_",var,".png",sep=""))
      print(p_melanges)
    dev.off()
  }else{
    p = get(load(paste(we_are_here,"/mixture_folder/figures/Histo_",var,".RData",sep="")))
    P=c(P,list(p$plot$plot))
  }
}

  out = list("figure" = list("caption" ="Distribution des différences relatives entre les comportement des mélanges et les comportements moyens
                                                       de leurs composantes respectives pour les différents caractères mesurés.
                                                       La ligne rouge verticale indique la différence moyenne des mélanges par rapport à la moyenne de leurs composantes respectives 
                                                       tandis que la ligne pointillée noire est fixée sur une différence nulle.", 
                                   "content" = P, "layout" = matrix(c(1,2), ncol = 1), "width" = 0.6))
  OUT = c(OUT, out)
  

  
  # 2.2.3. Différentiel de sélection ------
  cat("Différentiel de sélection ----------------------------------------- \n")
  out = list("subsection" = list("text"="Différentiels de sélection sur le réseau")); OUT = c(OUT, out)
  out = list("text" = "Les graphiques suivants présentent les résultats de la \\textbf{comparaison entre les bouquets de sélection et le vrac correspondant}, 
             séparément pour les sélections faites dans les composantes et celles faites dans les mélanges. \\\\
            Chaque élément de l'histogramme représente une comparaison entre un bouquet de sélection et son vrac correspondant, et la couleur représente la significativité
            de la comparaison (voir légende pour chaque graphique et voir partie \\ref{SymbolesSignif} pour l'explication de la significativité des tests de comparaison).
             Pour le \\textbf{PMG, poids de l'épi, nombre moyen de grains par épi et taux de protéine}, un histogramme décalé vers la droite par rapport à 0 (ligne pointillée) indique que globalement le 
              différentiel de sélection est positif (sélection > vrac),
             tandis qu'un histogramme décalé vers la gauche indique un différentiel de sélection négatif. \\\\
             Pour la \\textbf{couleur, présence de barbe et courbure}, un histogramme décalé vers la droite indique que les bouquets de sélection sont globalement plus foncés, 
            plus barbus ou plus courbés respectivement.
             A l'inverse si l'histogramme est décalé vers la gauche alors les bouquets sont globalement plus clairs, moins barbus et moins courbés respectivement.
            \\\\
             La ligne pointillée indique le zéro (différentiel de sélection nul).\\\\
             On constate que les caractères sélectionnés sont surtout le poids de l'épi, le nombre moyen de grains par épi et dans une moindre mesure la courbure de l'épi,
             sa couleur et le poids de mille grains. De manière générale, la sélection est exercée dans la même direction dans les composantes et dans les mélanges sauf pour la couleur
             et le taux de protéine."); OUT = c(OUT, out)
  
  if(file.exists("./mixture_folder/tex_files/Explication_DiffSel.png")){
  out = list("includeimage" = list("content"= "./tex_files/Explication_DiffSel.png", 
"caption"= "Explication des graphiques qui suivent : pour chaque événement de sélection est calculé le différentiel de sélection.
Sur les graphiques sont présentés les histogrammes de ces différentiels de sélection, séparément pour les sélections faites dans les composantes (histogramme du dessus)
et les sélections faites dans les mélanges (histogramme du dessous). Les différentes couleurs sur le graphique représentent la significativité de chaque différentiel de sélection :
voir partie \\ref{SymbolesSignif} pour les explication de la significativité des tests de comparaison.")) ; OUT=c(OUT,out)
  }

  if (file.exists("./mixture_folder/figures/Diff_Sel/DifferentielSelectionReseau-French_2016.pdf")){
   out =  list("includepdf" = "figures/Diff_Sel/DifferentielSelectionReseau-French_2016.pdf") ; OUT=c(OUT,out)
  }

  
  # 2.2.4. Réponse à la sélection ------
  cat("Réponse à la sélection -----------------------------------------------------")
  out = list("subsection" = list("text"="Impact des pratiques de sélection sur le comportement des mélanges")); OUT = c(OUT, out)
  out = list("text" = "En 2017 nous pouvons comparer l'\\textbf{effet des pratiques de sélection testées sur le comportement des mélanges (1 année de sélection) : 
             une année de sélection dans les composantes avant de mélanger (M2) et la sélection dans le mélange (M3)}. \\\\
Le tableau suivant présente :
\\begin{itemize}
\\item pour chaque modalité de sélection (voir schéma du dispositif pour l'explication des modalités de sélection),
les \\textbf{différentiels de sélection moyen} (\"DS\", moyenne des comparaisons bouquet de sélection vs. vrac) pour les différents caractères mesurés 
ainsi que l'\\textbf{effet de la sélection sur le comportement du mélange} (réponse à la sélection \"RS\" : moyenne des comparaison de la modalité de sélection avec le mélange non sélectionné)
\\item la comparaison du comportement du mélange sélectionné (Modalité 3) et du mélange issu des sélections dans les composantes (Modalité 2)
\\item pour les caractères pour lesquelles on a des mesures individuelles, la comparaison de la variabilité observée dans le mélange sélectionné (Modalité 3) 
et dans le mélange issu des sélections dans les composantes (Modalité 2)
\\end{itemize}
\\\\

\\emph{Elements d'interprétation :}
\\begin{itemize}
\\item On constate pour certains caractères que malgré le différentiel de sélection important en 2016, ça n'a pas forcément un effet important sur le 
mélange l'année suivante (PMG). Pour d'autres caractères, comme le poids de l'épi et le nombre de grains par épi, on a un effet important de la sélection sur le comportement du mélange 
l'année suivante.
\\item On remarque aussi que sélectionner dans le mélange (modalité 3) semble donner une réponse à la sélection mieux corrélée au différentiel de sélection et 
plus importante que mélanger les sélections faites dans les composantes (notamment pour le poids de l'épi et le nombre moyen de grains par épis).
\\item Il semble que l'on conserve une diversité phénotypique plus importante en mélangeant les sélections faites dans les composantes qu'en sélectionnant dans le mélange (dernière colonne).
\\end{itemize}

"); OUT = c(OUT, out)
  
  t = get_mixture_tables(res_model1, year=NULL, year_DS=as.character(seq(2016,as.numeric(year)-1,1)), year_RS=as.character(seq(2016,as.numeric(year),1)),
                                   mix_to_delete=NULL,
                                   language=language,
                                   data_mixtures=data_mixtures,
                                   vec_variables =vec_variables, 
                                   data_S_all=data_S_all, 
                                   data_SR_all=data_SR_all, 
                                   path_to_tables = path_to_tables,
                                   list_trad=list_trad,
                                   table.type="selection.modalities",
                                   res_model_varintra = res_model_varintra)
    
  table = lapply(t,function(x){
      ds=x$DS ; rs=x$RS$Res ; vi=x$varIntra
      b = unique(unlist(lapply(list(ds,rs,vi),function(x){return(unique(apply(rs, 1, function(x) all(is.na(x)))))})))
      if(b != TRUE | length(b)>1){
        ds[is.na(ds)] = " " ; rs[is.na(rs)] = " " ; vi[is.na(vi)] = " "
        return(c(paste(ds["Total","mean"],"% ",ds["Total","stars"]," (n=",ds["Total","n"],")",sep=""),
                 paste(ds[grep("Mod 1",rownames(ds)),"mean"],"% ",ds[grep("Mod 1",rownames(ds)),"stars"]," (n=",ds[grep("Mod 1",rownames(ds)),"n"],")",sep=""), 
                 paste(rs["mean_gain","M1 Composantes"],"% ",rs["stars","M1 Composantes"]," (n=",rs["n","M1 Composantes"],")",sep=""),
                 paste(rs["mean_gain","M1 Melanges"],"% ",rs["stars","M1 Melanges"]," (n=",rs["n","M1 Melanges"],")",sep=""),
                 paste(ds[grep("Mod 2",rownames(ds)),"mean"],"% ",ds[grep("Mod 2",rownames(ds)),"stars"]," (n=",ds[grep("Mod 2",rownames(ds)),"n"],")",sep=""), 
                 paste(rs["mean_gain","M2"],"% ",rs["stars","M2"]," (n=",rs["n","M2"],")",sep=""),
                 paste(ds[grep("Mod 3",rownames(ds)),"mean"],"% ",ds[grep("Mod 3",rownames(ds)),"stars"]," (n=",ds[grep("Mod 3",rownames(ds)),"n"],")",sep=""), 
                 paste(rs["mean_gain","M3"],"% ",rs["stars","M3"]," (n=",rs["n","M3"],")",sep=""),
                 paste(rs["mean_gain","M3vsM2"],"% ",rs["stars","M3vsM2"]," (n=",rs["n","M3vsM2"],")",sep=""),
                 paste(vi["mean_gain","M3vsM2"],"% ",vi["stars","M3vsM2"]," (n=",vi["n","M3vsM2"],")",sep="")
        ))
      }else{
        return(NULL)
      }
      
    })
  Table = NULL
  for (i in table){Table=rbind(Table,i)}
  null = vec_variables[grep("NULL",table)]
  warning(paste("no data for ",null,sep=""))
  variables = vec_variables[-grep(paste(null,collapse="|"), vec_variables)]
  rownames(Table) =  unlist(lapply(variables,function(x){gsub("[.]"," ",x)}))
  rownames(Table) =  unlist(lapply( rownames(Table),function(x){strsplit(x,"-")[[1]][1]}))
  Table=cbind(rownames(Table),Table)
  Table = gsub("[(]n=[)]","",Table)
  Table = gsub("[(]n= [)]","",Table)
  if(language == "french"){
    colnames(Table) = c("Caractère", "Toutes modalités - DS", 
                      "Modalité 1 - DS" , "Modalité 1 - Composantes RS", "Modalité 1 - Melanges RP",
                      "Modalité 2 - DS" , "Modalité 2 - RP",
                      "Modalité 3 - DS" , "Modalité 3 - RS",
                      "Modalité 3 vs Modalité 2","Variabilité au sein des mélanges M3 vs M2")
  }else{
    colnames(Table) = c("Variable","All modalities - DS", 
                      "Modality 1 - DS" , "Modality 1 - Components RS", "Modality 1 - Mixtures RP",
                      "Modality 2 - DS" , "Modality 2 - RP",
                      "Modality 3 - DS" , "Modality 3 - RS",
                      "Modality 3 vs Modality 2","Intra mixture variability M3 vs M2")
  }
  write.table(Table,file=paste(path_to_tables,"/../resultats/tableaux/selection_modalities_",paste(year,collapse="-"),".csv",sep=""),sep=";")
  attributes(Table)$invert =FALSE
  out = list("table" = list("caption" = "\\textbf{Différentiel de sélection} (DS, données 2016)
et \\textbf{réponse à la sélection} (RS, données 2017). 
La valeur indiquée est le gain (ou la perte) en pourcentage du bouquet de sélection par rapport au vrac pour DS, et de la modalité de sélection du mélange
par rapport au mélange non sélectionné pour RS. Les symboles indiquent si la différence observée est significative ou non : voir tableau \\ref{Signif}
pour l'explication des symboles utilisés. L'avant dernière colonne présente la \\textbf{comparaison des modalités de sélection 2 et 3 (différence normalisée) :
sélection dans le mélange vs. sélections dans les composantes pour former le mélange},
tandis que la dernière colonne compare la \\textbf{variabilité observée} dans ces 2 modalités de mélange : pour ces deux dernières colonnes 
une valeur positive indique que la modalité 3 a une valeur supérieur à la modalité 2, à l'inverse une valeur négative indique que la modalité 2 est supérieure à la modalité 3. 
", "content" = list(Table),"landscape"=TRUE, "sep"=c(3,4,7,9,11,12))) ; OUT=c(OUT,out)

  
  # /!\ Get pdf ----------
  get.pdf(dir = paste(we_are_here, "/mixture_folder", sep = ""), 
          form.name = paste("test", year, sep = ":"), 
          LaTeX_head = "../tex_files/structure.tex", 
          LaTeX_body = OUT, 
          compile.tex = TRUE,
          color1 = "mln-green", 
          color2 = "mln-brown"
  )
  
}