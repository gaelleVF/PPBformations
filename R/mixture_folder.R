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
  pathway = NULL)
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
  
  if(FALSE){
    # get info from out_analyse_feedback_folder_1
    year = out_analyse_feedback_folder_1$year
    res_model1 = out_analyse_feedback_folder_1$res_model1
    res_model2 = out_analyse_feedback_folder_1$res_model2
    res_model_varintra = out_analyse_feedback_folder_1$res_model_varintra
    data_network_year = out_analyse_feedback_folder_1$data_network_year
    data_all =  out_analyse_feedback_folder_1$out_farmers_data[[person]]$data_all
    data_year =  out_analyse_feedback_folder_1$out_farmers_data[[person]]$data_year
    data_S_year =  out_analyse_feedback_folder_1$out_farmers_data[[person]]$data_S_year
    data_SR_year =  out_analyse_feedback_folder_1$out_farmers_data[[person]]$data_SR_year
    data_PPB_mixture = out_analyse_feedback_folder_1$out_farmers_data[[person]]$data_PPB_mixture
    Mixtures_all = out_analyse_feedback_folder_1$data_mixtures$Mixtures_all
    Mixtures_S = out_analyse_feedback_folder_1$data_mixtures$Mixtures_selection
    Mix_tot = out_analyse_feedback_folder_1$data_mixtures$Mix_tot
  }
  
  year = get(load(paste(pathway,"out_year.RData",sep="/")))
  out_farmers_data = get(load(paste(pathway,"out_out_farmers_data.RData",sep="/")))
  data_all =  out_farmers_data[[person]]$data_all
  data_year =  out_farmers_data[[person]]$data_year
  data_S_year =  out_farmers_data[[person]]$data_S_year
  data_SR_year =  out_farmers_data[[person]]$data_SR_year
  data_PPB_mixture = out_farmers_data[[person]]$data_PPB_mixture
  
  
  # Créer title page
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
      Chaque paysan choisi les populations qu'il souhaite assembler, c'est à partir de ce(s) mélange(s) que sont testées différentes pratiques de sélection : 
                      \\begin{itemize}
             \\item deux années de sélection dans les composantes avant de mélanger (Modalité 1 sur le schéma ci-dessous) ; 
             \\item une sélection dans les composantes avant de mélanger puis une sélection dans le mélange (Modalité 2) ; 
             \\item à partir du mélange créé sans sélection dans les composantes, deux années de sélection dans le mélange (Modalité 3).
           \\end{itemize}
             Ces pratiques de sélection sont comparées au mélange évoluant sans sélection massale (Modalité 4)


          \\begin{figure}[!h]
          \\begin{center} 
          \\includegraphics[width=.80\\textwidth]{",we_are_here,"tex_files/dispositifMelanges.png}
          \\caption{Schéma du dispositif sur 3 ans : les rectancles représentent les parcelles (plein : mélange ; vide : composante) et les flèches une sélection (trait plein) ou non (trait pointillé). 
Les fermes satellites mettent en places les modalités 2, 3 et 4 tandis que les fermes régionales mettent en place l'ensemble des modalités.}
          \\end{center} 
          \\end{figure}

            
            Le dispositif expérimental est adapté de celui utilisé en routine, avec le(s) témoin(s) répétés deux fois, le(s) mélange(s) répétés deux fois et les composantes non répétées.
            Cet essai peut tout à fait être semé dans la même parcelle que des populations que vous voudriez tester qui ne seraient pas intégrées dans le mélange, car les témoins utilisés sont les mêmes.
             Ce dossier présente les résultats des premières années d'essai. Si vous voulez mettre en place cet essai chez vous, c'est tout à fait possible,
          contactez votre animateur ou l'équipe de recherche !

          Ce dossier présente les résultats des essais depuis la saison 2015-2016. 
          \\begin{enumerate}
          \\item \\textbf{Une première partie traite des résultats dans votre ferme.}
          Si vous avez mis en place un essai mélange chez vous la première partie reprend l'ensemble des résultats sur votre ferme : 
          \\begin{itemize}
          \\item la comparaison du comportement du mélange par rapport aux comportement des composantes
          \\item la comparaison, pour chaque sélection faite, entre le bouquet de sélection et les épis pris au hasard.
          \\item la comparaison des réponses à la sélection des deux types de mélange (issu de sélection dans le mélange et issu du mélange des sélections dans les composantes) 
et la comparaison de ces mélanges au mélange non sélectionné
          \\end{itemize}
          \\item \\textbf{Une deuxième partie traite des résultats dans le réseau de fermes.}
         La seconde partie présente les résultats sur l'ensemble du réseau de fermes participant à l'essai. 
        Nous pouvons constater si globalement il y a un gain à faire un mélange par rapport à cultiver des populations \"en pur\" et voir si une pratique de sélection est plus favorable qu'une autre.
          \\end{enumerate}
  \\newpage",sep="/")
  
  p = paste(we_are_here, "/tex_files/intro_melanges", ".tex", sep = "")
  sink(p);	cat(a);	sink()
  
  out=list("input" = "../tex_files/intro_melanges.tex")
  OUT=c(OUT,out)
  
  res_model1 = get(load(paste(pathway,"out_res_model1.RData",sep="/")))
  
  # 2. Essai Mélanges --------------------------------------------------------------------------------------------------------------------------------------
  out = list("chapter" = "Résultats de l'essai mélanges"); OUT = c(OUT, out)

  # 2.1. Résultats sur la ferme -----
  if (is.null(data_PPB_mixture$data)) { 
    out = list("text" = "Vous n'avez pas mis en place l'essai de sélection pour les mélanges sur votre ferme."); OUT=c(OUT,out)
  }else{
    data_mixtures = get(load(paste(pathway,"out_data_mixtures.RData",sep="/")))
    Mixtures_all = data_mixtures$Mixtures_all
    Mixtures_S = data_mixtures$Mixtures_selection
    Mix_tot = data_mixtures$Mix_tot
    
    levels(Mixtures_all$data$son) = c(levels(Mixtures_all$data$son) , "C70_ANB_2011_0001")
    Mixtures_all$data[Mixtures_all$data$son %in% "C70#S-crossés_ANB_2015_0001","son"] = as.factor("C70_ANB_2011_0001")
    Mixtures_all$data[Mixtures_all$data$germplasm_son %in% "C70#S-crossés","germplasm_son"] = "C70"
    
    out = list("section" = "Résultats sur la ferme"); OUT = c(OUT, out)
    out = list("text" = "Rappel des appellations pour les différentes sélections faites : \\\\ 
               #VA : Sélections faites dans les composantes pour la modalité 1 la première année.\\\\ 
               #VB : Sélections faites dans les composantes pour la modalité 1 la deuxième année. \\\\
               #JA : Sélections faites dans les composantes pour la modalité 2 la première année.\\\\
               #JB : Sélections faites dans le mélange 2 recomposé pour la modalité 2 la deuxième année.\\\\
               #BA : Sélections faites dans le mélange pour la modalité 3 la première année.\\\\
               #BB : Sélections faites dans le mélange pour la modalité 3 la deuxième année.\\\\"); OUT=c(OUT,out)
    
    ## par mélange, donner d'abord la comparaison mélange/composantes, puis différentiel de sélection, puis réponse à la sélection pour chaque mélange et chaque caractère
    Mixtures_all_person = Mixtures_all$data[Mixtures_all$data$location %in% person,]
    Mixtures_all_person$expe_melange = unlist(lapply(1:nrow(Mixtures_all_person),function(i){
      m = Mixtures_all_person[i,]
      if(m$sl_statut == "son"){return(m$expe_melange)}
      if(m$sl_statut == "father"){
        return(unique(Mixtures_all_person[grep(m$expe,Mixtures_all_person$mixture_id),"expe_melange"]))
      }
    }))
    vec_variables = names(res_model1)

    mix = plyr:::splitter_d(Mixtures_all_person, .(expe_melange))

    graphs_ferme_melanges = function(OUT,variable,titre,mel){
   #   out = list("subsection" = titre); OUT = c(OUT, out)
      
      # Comparaison mélange vs composantes
      p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = mel, data_S = Mixtures_S, melanges_tot = Mix_tot, variable, year=c("2016","2017"), model = "model_1", 
                                   plot.type = "comp.in.farm", person, nb_parameters_per_plot = 20, save=NULL)
      
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
            out = list("figure" = list("caption" = paste("Comparaison du \\textbf{",variable,"} du mélange et de ses composantes. 
                                                       Les populations qui partagent le même groupe (représenté par une même lettre) ne sont pas significativement différentes.
                                                       ",sep=""), "content" = a, "layout" = matrix(c(1,2), ncol = 1), "width" = 1, "landscape" = FALSE))
            OUT = c(OUT, out)#}
          
      #  }
    #  }
     
      # Différentiel de sélection
      # Correspondance data_S et mix
      
      
      
      # Comparaison modalités de sélection
      if(length(a)<2){
        p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = mel, data_S = Mixtures_S, melanges_tot = Mix_tot, variable, year=c("2016","2017"), model = "model_1",
                                     plot.type = "comp.mod", person, nb_parameters_per_plot = 20, save=NULL)
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
                                                       ",sep=""), "content" = a, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 0.7)); OUT = c(OUT, out)}
        
        }
      
      return(OUT)
    }
    
    for(i in 1:length(mix)){
      print(i)
      x=mix[[i]]
      out = list("subsection" = unique(x$expe_melange)); OUT=c(OUT,out)
      for (variable in vec_variables){
        print(variable)
        OUT = graphs_ferme_melanges(OUT,variable,titre=variable,mel = x)
      }
    }
    
    
    # Ajouter les différentiels de sélection !
    
} #end resultats sur la ferme
  
  # 2.2. Résultats sur le réseau de fermes -----
  out = list("section" = "Résultats sur le réseau de fermes"); OUT = c(OUT, out)
  out = list("text" = "Dans cette partie sont présentés les résultats de l'essai mélange sur le réseau de ferme. 
             On s'intéresse dans un premier temps à la différence observée entre la valeur des mélanges et celle de la moyenne de leurs composantes respectives, 
             permettant de constater si les mélanges apportent en moyenne plutôt un gain ou une perte par rapport à la moyenne de leurs composantes. 
             Puis on s'intéresse à la variabilité observée, pour chacun des mélanges testés, de chacune des \"moins bonnes\" composantes, de la meilleure composante,
             de la valeur moyenne des composante et de celle des mélanges.
             Enfin on compare la valeur moyenne de l'ensemble des mélange à celle de l'ensemble des composantes pour voir si on détecte un effet du mélange."); OUT=c(OUT,out)
  
  
  # 2.2.1. Distribution du gain du mélange par rapport à la moyenne de ses composantes sur le réseau -----
  out = list("subsection" = "Distribution du gain du mélange par rapport à la moyenne de ses composantes sur le réseau"); OUT = c(OUT, out)
  out = list("text" = "Ces graphiques présentent le comportement des mélanges par rapport à la moyenne de leurs composantes respectives. 
             Un histogramme décalé vers la droite par rapport à 0 indique qu'une majorité des mélanges se sont mieux comportés que la moyenne de leurs composantes. 
             A l'inverse si l'histogramme est décalé vers la gauche la majorité des mélanges se sont moins bien comportés que la moyenne de leurs composantes."); OUT = c(OUT, out)
  
  melanges_reseau = function(OUT,variable,titre,distrib=TRUE,comp_global=FALSE){
    # Histogramme distribution de l'overyielding
    var = paste(strsplit(variable,"[.]")[[1]],collapse="")
    if (!file.exists(paste(we_are_here,"/mixture_folder/figures/Histo_",var,".png",sep=""))){
      p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, data_S = Mixtures_S, melanges_tot = Mix_tot, variable, 
                                   year=c("2016","2017"), model="model_1", plot.type = "mix.gain.distribution", person, nb_parameters_per_plot = 15,
                                   save=paste(we_are_here,"/AnalyseDonnees/donnees_brutes",sep=""))
      save(p_melanges,file=paste(we_are_here,"/mixture_folder/figures/Histo_",var,".RData",sep=""))
      png(paste(we_are_here,"/mixture_folder/figures/Histo_",var,".png",sep=""))
      p_melanges
      dev.off()
      #    }else{
      #      load(paste(we_are_here,"/figures/Histo_",var,".RData",sep=""))
    }
#    out = list("subsection" = titre); OUT = c(OUT, out)
    out = list("includeimage" = list("caption" = paste("Distribution des rapports entre les comportement des mélanges et les comportements moyens
                                                       de leurs composantes respectives pour le \\textbf{",variable,"}.
                                                       La ligne rouge verticale indique le gain moyen des mélanges par rapport à la moyenne de leurs composantes respectives 
                                                       tandis que la ligne pointillée noire est fixée sur un gain nul.",sep=""), 
                                     "content" = paste("./figures/Histo_",var,".png",sep=""), "width" = 0.6))
    OUT = c(OUT, out)
    
    # Distribution des mélanges et composantes
    if(distrib){
      if (!file.exists(paste(we_are_here,"/figures/Distribution_",var,".png",sep=""))){
        p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, data_S = Mixtures_S, melanges_tot = Mix_tot, variable, year=c("2016","2017"), 
                                     model="model_1", plot.type = "mix.comp.distribution", person, nb_parameters_per_plot = 15,save=paste(we_are_here,"/AnalyseDonnees/donnees_brutes",sep=""))
        save(p_melanges,file=paste(we_are_here,"/figures/Distribution_",var,".RData",sep=""))
        png(paste(we_are_here,"/figures/Distribution_",var,".png",sep=""))
        p_melanges
        dev.off()
        #    }else{
        #      load(paste(we_are_here,"/figures/Distribution",var,".RData",sep=""))
      }
      
      out = list("includeimage" = list("caption" = paste("Distribution sur le réseau des mélanges, des moins bonnes et meilleures composantes 
                                                         ainsi que de la moyenne des composantes pour chaque mélange pour le ",variable,".
                                                         Le X noir représente la valeur moyenne pour chaque type.",sep=""), 
                                       "content" = paste("./figures/Distribution_",var,".png",sep=""),  "width" = 0.7))
      OUT = c(OUT, out)
    }
    
    if(comp_global){
      p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, data_S = Mixtures_S, melanges_tot = Mix_tot, variable, year=year, model="model_1",plot.type = "mixVScomp", person, nb_parameters_per_plot = 15)
      out = list("figure" = list("caption" = "Comparaison entre la moyenne des mélanges et la moyenne des composantes sur le réseau.
                                 Les moyennes sont significativement différentes si les lettres diffèrent.
                                 ", "content" = p_melanges$bp, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)
    }
    
    return(OUT)
    }
  
  # 2.2.1.1. poids de mille grains -----
  OUT = melanges_reseau(OUT,variable="poids.de.mille.grains",titre="Poids de mille grains",distrib=FALSE,comp_global=FALSE)
  
  # 2.2.1.2. Poids de l'épi -----
  OUT = melanges_reseau(OUT,variable="poids.de.l.epi",titre="Poids de l'épi",distrib=FALSE,comp_global=FALSE)
  
  # 2.2.1.3. Hauteur -----
  OUT = melanges_reseau(OUT,variable="hauteur",titre="Hauteur moyenne",distrib=FALSE,comp_global=FALSE)
  
  # 2.2.1.4. Longueur de l'épi -----
  OUT = melanges_reseau(OUT,variable="longueur.de.l.epi",titre="Longueur de l'épi",distrib=FALSE,comp_global=FALSE)
  
  # 2.2.1.5. LLSD -----
  OUT = melanges_reseau(OUT,variable="LLSD",titre="Distance dernière feuille - base de l'épi",distrib=FALSE,comp_global=FALSE)
  
  # 2.2.1.6. Nombre moyen de grains par épi  -----
  OUT = melanges_reseau(OUT,variable="nbr.estime.grain.par.epi",titre="Nombre moyen de grains par épi",distrib=FALSE,comp_global=FALSE)
  
  if(FALSE){
    # 3.2.2. Distribution des mélanges, de la moins bonne composante & la meilleure composante -----
    out = list("subsection" = "Distributions des mélanges, de la moins bonne et la meilleure composante pour chaque mélange"); OUT = c(OUT, out)
    out = list("text" = "Ces graphiques présentent, pour chacun des mélanges testés cette année, le comportement du mélange, de sa moins bonne composantes,
             de sa meilleure composante et le comportement moyen de ses composantes. 
             On peut observer sur ces graphiques la variabilité de comportement des mélanges ainsi que celle de leurs moins bonne
             et meilleure composantes.
             "); OUT = c(OUT, out)
    
    #  3.2.3. Comparaison de l'effet mélange par rapport à variété "pure" -----
    #  A virer...? 
    out = list("subsection" = "Comparaison de la performance moyenne des mélanges par rapport àa la performance moyenne des composantes"); OUT = c(OUT, out)
    out = list("text" = "On se pose la question de savoir s'il y a une différence significative entre la moyenne de tous les mélanges de l'essai 
             et la moyenne de toutes leurs composantes."); OUT = c(OUT, out)
  }
  

  
  # /!\ Get pdf ----------
  get.pdf(dir = paste(we_are_here, "/mixture_folder", sep = ""), 
          form.name = paste(person, year, sep = ":"), 
          LaTeX_head = "../tex_files/structure.tex", 
          LaTeX_body = OUT, 
          compile.tex = TRUE,
          color1 = "mln-green", 
          color2 = "mln-brown"
  )
  
}