# 0. help -----------------------------------------------------------------
#' Function to get the "feedback file" containing the results of the year's trial for each farmer based on the analysis coming from \code{analyse_feedback_folder_1}
#' 
#' @param dir Directory where folder for each person is created
#' 
#' @param person The farmer's name
#' 
#' @param out_analyse_feedback_folder_1 The outputs from \code{analyse_feedback_folder_1}
#' 
#' @details 
#' The function creates two folders :
#' \itemize{
#'  \item "tex_files" with the tex files used to create the pdf
#'  \item "feedback_folder" with, for each person, a folder with information coming from shinemas2R::get.pdf() see ?get.pdf for more details.
#' }
#' 
#' @return Generate tex and pdf files
#' 
#' @author Pierre Rivière, Gaelle Van Frank
#' 
#' 
feedback_folder_1 = function(
  dir = ".",
  person,
  pathway = NULL)
  # go ----------
{
  # Set the right folder and create folders tex_files and feedback_folder ----------
  a = dir(dir)
  if( !file.exists(dir) ){ stop("directory ", dir, " does not exist.") }
  
#  message("Loading data....")
#  out_analyse_feedback_folder_1 = get(load(pathway))
  
  
  setwd(dir)
  we_are_here = getwd()
  if( !is.element("tex_files", dir()) ) { system("mkdir tex_files") ; message("The folder tex_files has been created") }
  if( !is.element("feedback_folder", dir()) ) { system("mkdir feedback_folder") ; message("The folder feedback_folder has been created") }
  
  # Add info useful for feedback_folder_1
  p = system.file("extdata", "feedback_folder_1", package = "PPBformations")
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
    \\noindent{\\Large \\textbf{\\textsf{Résultats de la saison ",as.numeric(year)-1,"-",year,"}}}
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
          
        %  \\begin{wrapfigure}{l}{.20\\textwidth}
          \\begin{center} \\vspace{-20pt}
          \\includegraphics[width=.20\\textwidth]{",we_are_here,"tex_files/Logo-UMRGV.jpg}
          \\end{center} \\vspace{-20pt}
       %   \\end{wrapfigure}
          \\noindent
          Isabelle Goldringer \\href{mailto:isabelle.goldringer@inra.fr}{isabelle.goldringer@inra.fr} \\\\
          Gaëlle van Frank \\href{mailto:gaelle.van-frank@inra.fr}{gaelle.van-frank@inra.fr} \\\\
          Equipe DEAP, INRA Le Moulon  \\\\
          Ferme du Moulon, 91190 Gif sur Yvette \\\\
          
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
          
          Ce dossier fait le bilan des premières années d'expérimentations qui se sont écoulées dans votre ferme et dans le réseau de fermes.
          Il permet de vous accompagner dans votre sélection d'un point de vue agronomique.
          Il n'est pas question ici de la qualité du blé, de son comportement en panification.
          Cet aspect qualité sera intégré dans le futur.
          
          \\warning{Ce dossier n'est qu'un modeste complément de l'expertise que vous avez développé en observant les populations chez vous, dans les champs et peut être au fournil.}
          
          Ce document est séparé en deux parties:
          
          \\begin{enumerate}
          
          \\item \\textbf{Une première partie traite des résultats dans votre ferme.} Cette partie permet de vous orienter pour répondre à la question : \\textbf{Quelles populations se comportent le mieux dans ma ferme?}
          
          Pour cela nous vous donnons les données recueillies pour chaque étape du cycle de la population (automne, hiver, printemps et été) ainsi que les mesures qui ont été effectuées au Moulon pour le poids de mille grains, le taux de protéine, le poids des épis, et dans les champs pour la hauteur et la verse.
          
          Pour les personnes concernées, nous vous apportons également des résultats sur vos bouquets de sélection, sur la réponse de ces bouquets à la sélection ainsi que sur l'essai mélange.
          
          \\item \\textbf{Une deuxième partie traite des résultats dans le réseau de fermes.} Cette partie permet de vous orienter pour répondre à la question : \\textbf{Quelles populations serait-il plus intéressant de tester chez moi pour les prochains semis?} Ces résultats vous permettent de mobiliser la diversité évaluée dans le réseau de fermes.
          
          \\begin{itemize}
          \\item Dans un premier temps, nous montrons les fermes dans lesquelles les populations se comportent le plus comme dans  votre ferme. Cela vous permet de vous mettre en lien avec cette ferme afin de récupérer quelques populations.
          
          \\item Dans un deuxième temps nous vous proposons de prédire les valeurs qu'auraient eu certaines populations dans vos fermes cette année : on prédit le passé!
          Cette information est issue des modèles statistiques que nous avons développés.
          Comme tous modèles, il donne une information avec une certaine confiance qui est donnée en pourcentage.
          \\end{itemize}
          
          \\end{enumerate}
          
          Les graphiques et les tableaux sont expliqués au fur et à mesure du document.
          En cas de soucis de compréhension, n'hésitez pas à nous contacter (\\href{mailto:pierre@semencespaysannes.org}{pierre@semencespaysannes.org}, 06 87 13 46 98). 
          Quand nous n'avons pas eu les données, il n'y a pas de résultats et la mention \"Pas de données\" apparaît.\\\\
          
          Au delà de cette diversité disponible dans votre ferme et dans le réseau, vous pouvez développer de nouvelles populations.
          Pour cela deux solutions sont à votre disposition\\footnote{Il en existe d'autres mais elles sont plus compliquées à mettre en oeuvre. On revient sur ce point lors des formations.} : 
          
          \\begin{itemize}
          \\item mélanger des populations existantes
          \\item faire des croisements. Dans ce cas, vous pouvez nous envoyer les parents que vous souhaitez croiser. L'équipe de recherche peut les croiser au Moulon. Elle pourra également venir faire des formations afin que vous puissiez réaliser vous même vos croisements dans votre ferme.
          \\end{itemize}
          
          ~\\\\  
          
          Selon vos souhaits et le nombre de populations que vous voulez semer, nous vous proposerons d'être ferme régionale ou ferme satellite.
          La figure ci-dessous rappelle la particularité de ces deux types de fermes.
          Avec les témoins : \\colorbox{black}{\\textcolor{white}{Rouge-du-Roc}}; \\colorbox{black}{\\textcolor{white}{C14}}; \\colorbox{black}{\\textcolor{white}{C21}}; \\colorbox{black}{\\textcolor{white}{Renan}}.
          Nous pouvons avoir les plans suivant (ces plans sont modulables selon vos contraintes ...):
          
          \\begin{center}
          \\begin{tabular}{c c}
          
          Fermes régionales & Fermes satellites \\\\
          \\hline	\\includegraphics[width=.4\\textwidth]{",we_are_here,"tex_files/plan_FR.pdf} & \\includegraphics[width=.4\\textwidth]{",we_are_here,"tex_files/plan_FS_bis.pdf} \\\\
          
          4 témoins dans 2 blocs & pas de blocs; 1 témoin répété deux fois \\\\
          
          24 populations non répétées & 8 populations non répétées \\\\
          \\hline
          \\end{tabular}
          \\end{center}
          
          ~\\\\ 
          
          Nous avons joint à ce dossier trois fiches à remplir afin d'avoir 
          \\begin{itemize}
          \\item votre avis sur ce dossier. En effet, ce type de retour est en construction et nous avons besoin de vous pour l'améliorer.
          %\\item la liste des populations que vous souhaitez semer pour les prochains semis. Cela nous permettra de faciliter les échanges de semences et de vous proposer un plan
          \\item la liste éventuelle de parents que vous souhaiteriez croiser
          \\end{itemize}
          
          
          La fiche \\guill{Le rôle des paysans participant au projet de sélection collaborative sur les céréales} qui réapitule les différentes étapes du projet est présenté dans la partie 2 de ce document.
          
          \\vfill
          
          \\begin{flushright}
          Bonne lecture et bons semis!
          
          Le Réseau Semences Paysannes
          
          L'équipe DEAP de l'INRA du Moulon
          \\end{flushright}
          \\vfill
          
          
          \\newpage",sep="/")
  
  p = paste(we_are_here, "/tex_files/intro", ".tex", sep = "")
  sink(p);	cat(a);	sink()
  
  out=list("input" = "../tex_files/intro.tex")
  OUT=c(OUT,out)
  
  
  

  #1.2 Fiche paysans --------------
  a=paste("\\chapter{Le rôle des paysans participant au projet de sélection collaborative sur les céréales}
          
          \\begin{center}
          \\Large Version 5 du 30 septembre 2015
          \\end{center}
          
          
          \\section{Réunion de bilan de l'année écoulée et des prochains semis}
          Cette réunion a lieu dans la deuxième semaine de septembre à Paris. Pour le suivi de la dynamique,
          il est important que l'animateur et au moins un paysan y soient présents.
          
          \\section{Les semis : dispositifs expérimentaux}
          
          
          \\warning{Envoyez à votre animateur la liste des populations que vous souhaitez semer et il vous
          proposera un plan de semis. Ensuite, envoyer le plan définitif à votre animateur. Il vous
          enverra alors des étiquettes et des piquets afin de bien identifier vos micros-parcelles.}
          
          
          Pour le semis, deux choses sont importantes :
          
          \\begin{itemize}
          \\item semer le témoin qui vous sera envoyé si vous ne l'avez pas déjà. A part le témoin, vous
          semez ce que vous voulez ! Il est important de conserver les noms des sélections que l'on
          vous donne lorsque que vous faites des bouquets de sélection (sous la forme [nom de la
          variété]\\#[une lettre], par exemple Rouge-de-Bordeaux\\#R ou Blanc-de-la-Réole\\#E)
          
          \\item suivre un dispositif expérimental de type ferme régional \\yo{OU} satellites.
          Pour les fermes satellites, s'il y a peu de populations, on peut imaginer répéter le témoin une seule fois.
          \\end{itemize}
          
          Les témoins sont dans les cases noires. Les populations que vous choisissez sont dans les cases
          blanches. 
          La taille conseillée des micro-parcelles est entre 5 et 10m\\up{2}.
          \\\\
          
          
          \\begin{center}
          \\begin{tabular}{c c}
          Fermes régionales & Fermes satellites \\\\
          \\hline
          \\includegraphics[width=.4\\textwidth]{",we_are_here,"tex_files/plan_FR.pdf}  & \\includegraphics[width=.4\\textwidth]{",we_are_here,"tex_files/plan_FS_bis.pdf} 
          \\\\
          \\hline
          \\end{tabular}
          \\end{center}
          
          \\section{Suivi des populations avec les fiches}
          Votre animateur vous fera parvenir des fiches afin de suivre la culture. Ces fiches peuvent être
          issues du projet national ou issues de votre groupe.
          
          \\section{Envoi des épis récoltés par la poste pour qu'ils soient mesurés à l'INRA}
          
          A la récolte, votre animateur vous enverra un courrier avec deux types d'enveloppes :
          
          \\begin{itemize}
          \\item \\yo{« 50 épis au hasard »} pour y déposer 50 épis (ou moins s'il y a peu d'épis dans la micro-
          parcelle) pris au hasard dans la parcelle ;
          
          \\item \\yo{« bouquet d'épis sélectionnés »} si vous faites des bouquets de sélection. Ceci est optionnel
          mais peut être intéressant si vous souhaitez étudier vos sélections massales.
          Ces épis seront mesurés pour les barbes, la couleur, la courbure de l'épi, le poids de l'épi, le taux de
          protéine et le poids de mille grains. Une notice plus précise est fournie avec les sacs.
          \\end{itemize}
          
          \\warning{Il est important que ces sacs d'épis soient envoyés à l'INRA le plus vite possible !}
          
          Nous recevons en moyenne 700 sacs par an, les mesures doivent être terminées le 10 septembre,
          date limite pour faire les mesures de protéine. Si nous recevons tout fin août, ce n'est pas gérable ...
          
          ~\\\\
          
          \\yo{N'hésitez pas à nous contacter si vous souhaitez de plus amples informations :}
          
          Pierre Rivière, RSP ; \\url{pierre@semencespaysannes.org} ; 06 87 13 46 98
          
          \\newpage
          ", sep="/")
  
  p = paste(we_are_here, "/tex_files/fiche_paysans_SP_cereales_v5", ".tex", sep = "")
  sink(p);	cat(a);	sink()
  
  out=list("input" = "../tex_files/fiche_paysans_SP_cereales_v5.tex")
  OUT=c(OUT,out)
  
  # 2. Partie sur la ferme ---------------------------------------------------------------------------------------------------------------------------------------
  
  # les graph pour les fiches
  graph.fiche = function(data, variable, year) {
    
    if(variable == "note.globale.hiver---global"){ in.cap = "d'hiver" }
    if(variable == "note.globale.printemps---note.globale.printemps"){ in.cap = "de printemps" }
    if(variable == "note.globale.ete---global"){ in.cap = "d'été" }
    
    if(variable == "all_notes"){ 
      in.cap = "d'hiver, de printemps et d'été"
      variable = c("note.globale.hiver---global","note.globale.printemps---note.globale.printemps","note.globale.ete---global")
      if(length(unique(variable %in% colnames(data$data$data))) == 1 & unique(variable %in% colnames(data$data$data))==FALSE){
        p=NULL
      }else{
          p = get_interaction_cycle(data,variable,equal.ylim = TRUE,nb_parameters_per_plot_in.col = 8,year)
      }
	
    # }else{
    #   # 1ere année : mettre un tableau plutôt qu'un graph...
    #   if(){
    #     
    }else{
      p = get.ggplot(data = data, ggplot.type = "data-interaction", x.axis = "year", 
                     in.col = "germplasm", vec_variables = variable, 
                     nb_parameters_per_plot_in.col = 8, merge_g_and_s = TRUE)
    }
    
    if( !is.null(p) ) {
      out = list("figure" = list("caption" = paste("Evolution des notes globales", in.cap, "."), "content" = p, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1))
    } else { 
      out = list("figure" = NULL)
    }
    return(out)
  }
  

  out = list("chapter" = "Résultats dans la ferme"); OUT = c(OUT, out)
  
  out = list("text" = 
               "Dans ce chapitre, vous trouverez les résultats des données recueillies dans votre ferme.
             Ces résultats vous permettent de répondre à la question : \\textbf{Quelles populations se comportent le mieux dans ma ferme?}.
             "); OUT = c(OUT, out)
  
  # 2.0. Populations présentes dans la ferme ----------
  out = list("section" = "Populations semées cette année dans votre ferme"); OUT = c(OUT, out)
  D=out_farmers_data[[person]]$data_year$data$data
  a = unique(D[!is.na(D$block) |!is.na(D$X) | !is.na(D$Y) | !is.na(D$"poids.grains.mesure---poids.grains.mesure") | 
                 !is.na(D$"nbr_spikes---nbr.épiss") | !is.na(D$"poids.de.mille.grains---poids.de.mille.grains"),"son"])
  a = pop_ferme = unlist(lapply(as.character(a),function(x){strsplit(x,'_')[[1]][1]}))
  b = unique(out_farmers_data[[person]]$data_S_year$data$data$expe_name_2)
  b = unlist(lapply(as.character(b),function(x){strsplit(x,' | ')[[1]][3]}))
  b = unlist(lapply(as.character(b),function(x){strsplit(x,'_')[[1]][1]}))
  if(!is.null(b)){a = a[-grep(paste(b,collapse='|'),a)]}
  a=unique(a)
  a = paste(a, collapse=" ; ")
  
  out = list("text" = paste("Voici la liste des populations que vous avez semé cette année et pour lesquelles vous nous avez envoyé des informations ou des grains : \\textbf{", a,"}",sep="")); OUT = c(OUT, out)
  
  # 2.0.1. Evolution des notes globales au cours du cycle
  graph = graph.fiche(data_all, "all_notes",year)
  if(!is.null(graph$figure)){
    out = list("subsection" = "Evolution des notes globales de ces populations au cours de l'année"); OUT = c(OUT, out)
    OUT=c(OUT,graph)
  }
  
  rm(list="out_farmers_data")
  
  # 2.1. Automne ----------
  out = list("section" = "Automne"); OUT = c(OUT, out)
  comp=NULL
  vec_variables = c(
    "topographie---notice.topographie.semis.2",
    "pratiques.semis---notice.pratiques.semis",
    "battance---notice.battance",
    "surface.micro.parcelle---notice.surface.micro.parcelle",
    "date.semis---date",
    "surface.champs---notice.surface.champ",
    "type.de.sol.info1---notice.type.sol.1",
    "type.de.sol.info2---notice.type.sol.2",
    "info.sur.le.champs---notice.info.champ",
    "espace.entre.micro.parcelles---espace.entre.micro.parcelles",
    "densite.semis---notice.densite.semis",
    "nom.champ---notice nom.champ",
    "labour.avant.semis---notice.labour.avant.semis",
    "drainage---notice.drainage",
    "précédent.cultural---notice.précédent.cultural",
    "remarques.automne---commentaires")
  
  tab = get.table(data = data_year, table.type = "raw", vec_variables = vec_variables, col_to_display = c("germplasm", "block"), 
                  invert_row_col = FALSE, merge_g_and_s = TRUE, nb_duplicated_rows = 2, nb_row =NULL,
                  nb_col = 4)
  tab=traduction(tab,row_or_col = "row")
  if(!is.null(tab$duplicated_infos)){
      set_variables = unique(lapply(tab$duplicated_infos,function(x){return(colnames(x$duplicated_infos_variables))}))
      names(set_variables)=paste("set",seq(1:length(set_variables)),sep="-")
      TAB = lapply(set_variables,function(x) NULL)
      noms = NULL
      for (i in 1:length(tab$duplicated_infos)){
        name = colnames(tab$duplicated_infos[[i]]$duplicated_infos_variables)
        set = grep("TRUE",lapply(set_variables, identical, name))
        if(length(unique(dim(tab$duplicated_infos[[i]]$"duplicated_infos_variables")))==1 & nrow(tab$duplicated_infos[[i]]$"duplicated_infos_variables")==1){a=as.character(tab$duplicated_infos[[i]]$"duplicated_infos_variables"[1,1])
        }else{a=as.character(tab$duplicated_infos[[i]]$"duplicated_infos_variables")}
        a = c(paste("Groupe",i,sep=" "),a)
        TAB[[set]] = rbind(TAB[[set]],a)
        colnames(TAB[[set]]) = c("seed_lot",lapply(set_variables[[set]],function(y){strsplit(y,"---")[[1]][1]}))
        b=c(paste("Groupe ",i,sep=""), as.character(tab$duplicated_infos[[i]]$`duplicated_infos_seed-lots`$`seed-lots`))
        noms=rbind(noms,b)
      }
      a = unique(noms[,2])
      b=lapply(a,function(x){
        d = grep(TRUE,x == noms[,2])    #grep(paste("^",x,"$",sep=""),noms[,2]) 
        return(list("Groupe"=paste("Groupe ",paste(d,collapse = ",")," : ",unique(noms[d,2]),sep=""),"nom" = paste("Groupe ",d,sep="")))
      })
      noms=b

      TAB = lapply(TAB,function(x){
        if(nrow(x) == 1 ){
          nom=colnames(x)
          x = c(x[,1],NA,x[,2:ncol(x)])
          names(x) = c("germplasm","block",nom[2:length(nom)]) 
        }else{
          nom=colnames(x)
          x = cbind(x[,1],NA,x[,2:ncol(x)])
          colnames(x) = c("germplasm","block",nom[2:length(nom)])
        }
        return(x)
      })
      if(!is.null(tab$not_duplicated_infos)){
        for (i in 1:length(tab$not_duplicated_infos)){
          x=tab$not_duplicated_infos[[i]]
          name = colnames(x)[3:ncol(x)]
          set = grep("TRUE",lapply(set_variables, identical, name))
          colnames(x) = lapply( colnames(x),function(y){strsplit(y,"---")[[1]][1]})
          if(length(set)>0){
            TAB[[set]] = rbind(x,TAB[[set]])
          }else{
            TAB = c(TAB,list(x))
          }
        }
        TAB=lapply(TAB,function(x){
          attributes(x)$invert=FALSE
          return(x)
        })
        tab$not_duplicated_infos=TAB
        tab$duplicated_infos =NULL
      }else{
        TAB=lapply(TAB,function(x){
          attributes(x)$invert=FALSE
          return(x)
        })
        tab=TAB
      }
      

  }
  
  
  if(!is.null(tab)){
    for (i in 1:length(tab)){
      if(!is.null(tab[[i]])){
        if(is.list(tab[[i]])){
          for(j in 1:length(tab[[i]])){
            if(is.null(dim(tab[[i]][[j]]))){germ = tab[[i]][[j]]["germplasm"][grep("Groupe",tab[[i]][[j]]["germplasm"])]}else{germ = tab[[i]][[j]][,"germplasm"][grep("Groupe",tab[[i]][[j]][,"germplasm"])]}
            name=NULL
            for (n in germ){if(!is.null(n)){name = paste(name,noms[[grep(n,noms)]]$Groupe,sep="\\\\")}}
            out = list("table" = list("caption" = paste("Informations sur les pratiques culturales",name,sep="\n"), "content" = list(tab[[i]][[j]]),"landscape"=TRUE)); OUT = c(OUT, out)
          }
        }else{
          if(is.null(dim(tab[[i]]))){germ = tab[[i]]["germplasm"][grep("Groupe",tab[[i]]["germplasm"])]}else{germ = tab[[i]][,"germplasm"][grep("Groupe",tab[[i]][,"germplasm"])]}
          name=NULL
          for (n in germ){if(!is.null(n)){name = paste(name,noms[[grep(n,noms)]]$Groupe,sep="\\\\")}}
          out = list("table" = list("caption" = paste("Informations sur les pratiques culturales",name,sep="\n"), "content" = list(tab[[i]]),"landscape"=TRUE)); OUT = c(OUT, out)
        }
      }
     }
   comp = 1
  }
  
  vec_variables = 
    c("pluies.automne---pluies", 
      "températures.automne---température", 
      "notes.sur.le.climat.automne---notes.climatiques", 
      "accidents.climatiques.automne---accidents.climatiques")
  tab = get.table(data = data_year, table.type = "raw", vec_variables = vec_variables, nb_col = 5, col_to_display = NULL, merge_g_and_s = TRUE)
  tab= traduction(tab,"col")
  
  if(!is.null(tab)){out = list("table" = list("caption" = "Informations sur le climat d'automne", "content" = tab)); OUT = c(OUT, out) ; comp = 1}
  if(is.null(comp)){out=list("text" = "Pas de données"); OUT=c(OUT,out)}
  
  
  # 2.2. Hiver ----------
  out = list("section" = "Notations d'hiver"); OUT = c(OUT, out)
  comp=NULL
  
  # 2.2.1. Evolution de la note globale ----------
  graph = graph.fiche(data_all, "note.globale.hiver---global",year)
  if(!is.null(graph$figure)){
    out = list("subsection" = "Note globale"); OUT = c(OUT, out)
    OUT = c(OUT,graph)
    comp=1
  }

  
  
  # 2.2.2. Données hiver pour year ----------
    vec_variables = c("date.observation.hiver---date.observation", 
                    "reprise---reprise", 
                    "attitude des feuilles---attitude des feuilles", 
                    "port.au.tallage---port.au.tallage", 
                    "densité---densité", 
                    "commentaires.hiver---post_winter_observation_notes")
  tab = get.table(data = data_year, table.type = "raw", vec_variables = vec_variables, 
                  nb_col = 5, col_to_display = c("germplasm", "block"), merge_g_and_s = TRUE,nb_duplicated_rows=50)
  tab=traduction(tab,"col")
  if("reprise"%in% colnames(tab$not_duplicated_infos$`set-1`)){tab$not_duplicated_infos$`set-1`[order(as.numeric(as.character(tab$not_duplicated_infos$`set-1`[,"reprise"]))),]}
 

  if(!is.null(tab)){
    out = list("subsection" = paste("Données détaillées pour", year)); OUT = c(OUT, out)
    out = list("table" = list("caption" = "Sommaire de la fiche hiver", "content" = tab, landscape = TRUE)); OUT = c(OUT, out)
    comp=1
  }
  
  vec_variables = c("pluies.hiver---pluies", 
                    "températures.hiver---températures",
                    "accidents.climatiques.hiver---accidents.climatiques", 
                    "notes.sur.le.climat.hiver---notes.climatiques", 
                    "accidents.dans.le.champ.hiver---accidents.dans.le.champ", 
                    "notes.sur.le.champ.hiver---commentaires")
  tab = get.table(data = data_year, table.type = "raw", vec_variables = vec_variables, 
                  nb_col = 5, col_to_display = NULL, merge_g_and_s = TRUE)
  tab=traduction(tab,"col")
  if(!is.null(tab)){out = list("table" = list("caption" = "Informations sur le climat de l'hiver", "content" = tab)); OUT = c(OUT, out) ; comp = 1}
  if(is.null(comp)){out=list("text" = "Pas de données"); OUT=c(OUT,out)}
  
  # 2.3. Printemps ----------
  out = list("section" = "Notations de printemps"); OUT = c(OUT, out)
  comp=NULL
  # 2.3.1. Evolution de la note globale ----------
  graph = graph.fiche(data_all, "note.globale.printemps---note.globale.printemps",year)
  if(!is.null(graph$figure)){
    out = list("subsection" = "Note globale"); OUT = c(OUT, out)
    OUT = c(OUT,graph)
    comp=1
  }

  
  # 2.3.2. Données printemps pour year ----------
  vec_variables = c(
    "date.observation.printemps---date.observation",
    "tallage---tallage",
    "vigueur---vigueur",
    "couleur.printemps---couleur.printemps",
    "adventices.printemps---adventices",
    "date.épiaison_60-100---date.épiaison_60-100",
    "notes.maladies---notes.maladies", 
    "commentaires.printemps---commentaires",
    "note.épiaison---remarques.printemps_note")
  tab = get.table(data = data_year, table.type = "raw", vec_variables = vec_variables, 
                  nb_col = 6, col_to_display = c("germplasm", "block"), merge_g_and_s = TRUE)
  tab=traduction(tab,"col")
  
  if(!is.null(tab)){
    out = list("subsection" = paste("Données détaillées pour", year)); OUT = c(OUT, out)
    out = list("table" = list("caption" = "Sommaire de la fiche printemps", "content" = tab, landscape = TRUE)); OUT = c(OUT, out)
    comp=1
  }
  
  # Evolution maladies au cours du cycle
  vec_variables = c( "sain_note1---sain_note1",
                     "sain_note2---sain_note2",
                     "sain_note3---sain_note3",
                     "malade_note1---malade_note1",
                     "malade_note2---malade_note2",
                     "malade_note3---malade_note3",
                     "tres_malade_note1---tres_malade_note1",
                     "tres_malade_note2---tres_malade_note2",
                     "tres_malade_note3---tres_malade_note3"
    )
  tab = get.table(data = data_year, table.type = "raw", vec_variables = vec_variables, 
                  nb_col = 10, col_to_display = c("germplasm", "block"), merge_g_and_s = FALSE)
   if(!is.null(tab)){
     p = get_interaction_plot_disease(tab,5,vec_variables,type="year")
     a=NULL
     if(!is.null(p$Data_groups)){ for (i in 1:nrow(p$Data_groups)){a=paste(a,paste(p$Data_groups[i,1],p$Data_groups[i,2],sep=" : "),sep="\n")}}
     out = list("figure" = list("caption" = paste("Evolution de la note de maladie au cours de l'année. Si plusieurs populations ont reçu les même notes elles se trouvent dans le même groupe. 1 = sain ; 2 = malade ; 3 = très malade",
                                                  a,sep=""),"content" = p$plot, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1))
     OUT=c(OUT,out)
    comp=1
   }
  
  # Evolution maladies au cours des années
  tab = get.table(data = data_all, table.type = "raw", vec_variables = vec_variables, 
                  nb_col = 10, col_to_display = c("germplasm", "block"), merge_g_and_s = FALSE,nb_duplicated_rows = 500)
  if(!is.null(tab)){
    p = get_interaction_plot_disease(tab,12,vec_variables,type="all_year")
    if(!is.null(p)){
      out = list("figure" = list("caption" = "Evolution de la note de maladie au cours des années. 1 = sain ; 2 = malade ; 3 = très malade","content" = p, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1))
      OUT=c(OUT,out);comp=1
    }
    
  }
  
  
  vec_variables = c("pluies.printemps---pluies", 
                    "températures.printemps---températures", 
                    "accidents.climatiques.printemps---accidents.climatiques.printemps", 
                    "notes.sur.le.climat.printemps---notes.sur.le.climat.printemps", 
                    "accidents.dans.le.champ.printemps", 
                    "notes.sur.le.champ.printemps---notes.sur.le.champ.printemps")
  tab = get.table(data = data_year, table.type = "raw", vec_variables = vec_variables, 
                  nb_col = 6, col_to_display = NULL, merge_g_and_s = TRUE)
  tab=traduction(tab,"col")
  if(!is.null(tab)){out = list("table" = list("caption" = "Informations sur le climat du printemps", "content" = tab)); OUT = c(OUT, out); comp=1}
  if(is.null(comp)){out=list("text" = "Pas de données"); OUT=c(OUT,out)}
  
  # 2.4. Ete ----------
  out = list("section" = "Notations d'été"); OUT = c(OUT, out)
  comp=NULL
  # 2.3.1. Evolution de la note globale ----------
  graph = graph.fiche(data_all, "note.globale.ete---global",year)
  if(!is.null(graph$figure)){
    out = list("subsection" = "Note globale"); OUT = c(OUT, out)	
    OUT = c(OUT,graph);
    comp=1
  }

  
  # 2.3.2. Données ete pour year ----------
  vec_variables = c("date.observation.été---date.observation", 
                    "biomasse---biomasse", 
                    "hétérogénéité---hétérogénéité", 
                    "date.récolte---date.récolte", 
                    "poids.battage---poids.battage",
                    "rdt_parcelle---rdt_parcelle",
                    "commentaires.été---commentaires")
  tab = get.table(data = data_year, table.type = "raw", vec_variables = vec_variables, 
                  nb_col = 5, col_to_display = c("germplasm", "block"), merge_g_and_s = TRUE,nb_duplicated_rows=200)
  tab=traduction(tab,"col")
  if(!is.null(tab)){out = list("table" = list("caption" = "Sommaire de la fiche été", "content" = tab, landscape = TRUE)); OUT = c(OUT, out); comp=1}
  
  vec_variables = c("pluies.été---pluies",
                    "températures.été---températures", 
                    "accidents.climatiques.été---accidents.climatiques", 
                    "notes.sur.le.climat.été---notes.climatiques", 
                    "accidents.dans.le.champ.été---accidents.dans.le.champ", 
                    "notes.sur.le.champ.été---notes.sur.le.champ.été")
  tab = get.table(data = data_year, table.type = "raw", vec_variables = vec_variables, 
                  nb_col = 5, col_to_display = NULL, merge_g_and_s = TRUE)
  tab=traduction(tab,"col")
  if(!is.null(tab)){ 
    out = list("subsection" = paste("Données détaillées pour", year)); OUT = c(OUT, out)
    out = list("table" = list("caption" = "Informations sur le climat de l'été", "content" = tab)); OUT = c(OUT, out)
    comp=1
  }
  if(is.null(comp)){out=list("text" = "Pas de données"); OUT=c(OUT,out)}
  
  # 2.5. Mesure à la récolte ----------
  out = list("section" = "Mesures à la récolte"); OUT = c(OUT, out)

res_model1 = get(load(paste(pathway,"out_res_model1.RData",sep="/")))
  # 2.5.1. Mesures sur les populations ----------
  out = list("subsection" = "Mesures sur les populations"); OUT = c(OUT, out)

  interaction_and_score = function(OUT,res_model,variable,table=FALSE,titre,score=TRUE,inter_plot=FALSE){

    if(variable %in% names(res_model1) & length(grep(paste(person,year,sep=":"),names(res_model1[[variable]]$model.outputs$MCMC)))>0){
      out = list("subsubsection" = titre); OUT = c(OUT, out)
      if(variable == "taux.de.proteine"){
        out = list("text" = paste("Etant donné la quantité de sacs que nous recevons nous ne pouvons pas faire les analyse de protéine sur toutes les populations.
                                  Nous priorisons les sélection ainsi que les réponses à la sélection (semis l'année après sélection de cette sélection et de la population
                                  non sélectionnée correspondante).",sep="")); OUT = c(OUT, out)
      }
      res_model = res_model[[variable]]
      comp.mu = res_model$comp.par$comp.mu
      
      mcmc=names(res_model$model.outputs$MCMC)
      mcmc=mcmc[grep(person,mcmc)] ; mcmc=mcmc[grep("sigma",mcmc)]
      if(length(mcmc) == 1){
        # first year
        tab = comp.mu$data_mean_comparisons[[paste(person,year,sep=":")]]$mean.comparisons[,c("entry","median","groups")]
        colnames(tab)=c("Population",variable,"groupe")
        attributes(tab)$invert = FALSE
        #p =  plot.PPBstats(x=comp.mu, ggplot.type = "barplot", nb_parameters_per_plot = 10)$data_mean_comparisons[paste(person,year,sep=":")]
        if(!is.null(tab)){
          out = list("text" = paste("Le tableau ci-dessous présente le \\textbf{",variable,"} pour les populations récoltées cette année.",sep="")); OUT = c(OUT, out)
          out = list("table" = list("caption" = paste(variable," des populations récoltées en ",year,". 
Deux populations partageant la même lettre (colonne groupe) ne sont pas significativement différentes.",sep=""), "content" = tab)); OUT = c(OUT, out)
        }

        
      }else{
        # Interaction plot
        if(inter_plot){
          p_interaction_glob = plot.PPBstats(x=comp.mu, ggplot.type = "interaction", nb_parameters_per_plot = 10)
          p_interaction = p_interaction_glob$data_mean_comparisons[person]
          out = list("figure" = list("caption" = paste("
                               Comparaisons de moyennes pour le \\textbf{",variable,"} au cours du temps. 
                               Les populations qui partagent le même groupe pour une année donnée (représenté par une barre) ne sont pas significativement différentes.
                               Le pourcentage de confiance dans cette information est indiqué en dessous des points. 
                               Imp veut dire impossible : nous n’avons pas pu faire de groupe car la variabilité due au sol était trop importante.
                               ",sep=""), "content" = p_interaction, "layout" = matrix(c(1,2), ncol = 1), "width" = 1)); OUT = c(OUT, out)
        }
        
        
        # Score plot
        if(score){
          p_score =plot.PPBstats(comp.mu, ggplot.type = "score", nb_parameters_per_plot = 22)[person]
          out = list("figure" = list("caption" = paste("
                               Les chiffres donnés dans ce graphique correspondent à la valeur du \\textbf{",variable,"} pour chaque population les différentes années sur votre ferme.
                               L'échelle de couleur correspond aux groupes de significativité : des populations présentant des couleurs différentes sont significativement différentes.
                               Attention : Les groupements sont faits par année, donc on ne peut pas interpréter 2 populations présentes deux années différentes comme étant dans le même groupe si elles ont la même couleur.
                               ",sep=""), "content" = p_score, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)
        }
        
        
        # Interaction without statistical analysis
        if(inter_plot){
          if(person %in% names(p_interaction_glob$data_env_whose_param_did_not_converge)){
            out = list("figure" = list("caption" = paste("Evolution du \\textbf{",variable,"} au cours du temps sans analyses statistiques.",sep=""), "content" = p_interaction_glob$data_env_whose_param_did_not_converge[person], "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)
          }
        }
        # Table
        if(table){
          out = list("text" = paste("Le tableau ci-dessous présente le \\textbf{",variable,"} pour les populations récoltées cette année.",sep="")); OUT = c(OUT, out)
          tab = get.table(data = data_year, table.type = "mean", vec_variables = paste(variable,"---",variable,sep=""), 
                          nb_col = 5, col_to_display = "germplasm", merge_g_and_s = TRUE, order_var = paste(variable,"---",variable,sep=""))
          tab=traduction(tab,"col")
          tab$not_duplicated_infos$`set-1`[,variable] = round(as.numeric(as.character(tab$not_duplicated_infos$`set-1`[,variable])),2)
          out = list("table" = list("caption" = paste(variable," des populations récoltées en ",year,sep=""), "content" = tab)); OUT = c(OUT, out)
        }
      }
    }else{
      tab = get.table(data = data_year, table.type = "mean", vec_variables = paste(variable,"---",variable,sep=""), 
                      nb_col = 5, col_to_display = "germplasm", merge_g_and_s = TRUE, order_var = paste(variable,"---",variable,sep=""))
      tab=traduction(tab,"col")
      tab$not_duplicated_infos$`set-1`[,variable] = round(as.numeric(as.character(tab$not_duplicated_infos$`set-1`[,variable])),2)
      if(!is.null(tab$not_duplicated_infos$`set-1`[,variable])){
        out = list("subsubsection" = titre); OUT = c(OUT, out)
        out = list("text" = paste("Le tableau ci-dessous présente le \\textbf{",variable,"} pour les populations récoltées cette année.",sep="")); OUT = c(OUT, out)
        out = list("text" = "Si vous n'avez pas semé deux répétitions du témoin ou que vous ne nous avez pas envoyé d'information ou d'épis pour les 2 répétitions, nous n'avons pas pu analyser statistiquement les données. 
                   Voici un tableau présentant les données brutes mesurées sur les populations. 
                   L'année prochaine pensez à semer au moins \\textbf{2 répétitions du(des) témoin(s)} et à bien nous envoyer les informations et épis pour les 2 répétitions semées pour que l'on puisse estimer les valeurs de vos populations plus précisément !") ; OUT=c(OUT,out)
        out = list("table" = list("caption" = paste(variable," des populations récoltées en ",year,sep=""), "content" = tab)); OUT = c(OUT, out)
      }
    }
    

   return(OUT)
  }
  
    # 2.5.1.1. Poids de mille grains ----------
  OUT=interaction_and_score(OUT,res_model1,"poids.de.mille.grains",table=FALSE,titre = "Le poids de mille grains",score=TRUE,inter_plot=FALSE)

  # 2.5.1.2. Taux de protéine ----------
  OUT=interaction_and_score(OUT,res_model1,"taux.de.proteine",table=FALSE,titre = "Le taux de protéine",score=TRUE,inter_plot=FALSE)
  
   # 2.5.1.3. Poids de mille grains en fonction du taux de protéine ----------
  a=data_all$data$data
  if(length(grep(paste("^poids.de.mille.grains---poids.de.mille.grains$","^taux.de.proteine---taux.de.proteine$",sep="|"),colnames(a))) == 2){
    prot_ok =  a[!is.na(a[,"poids.de.mille.grains---poids.de.mille.grains"]) & !is.na(a[,"taux.de.proteine---taux.de.proteine"]),c(1:40,grep("^taux.de.proteine---taux.de.proteine$",colnames(a)),grep("^poids.de.mille.grains---poids.de.mille.grains$",colnames(a)))]
    prot = a[is.na(a[,"poids.de.mille.grains---poids.de.mille.grains"]) & !is.na(a[,"taux.de.proteine---taux.de.proteine"]),c(1:40,grep("^taux.de.proteine---taux.de.proteine$",colnames(a)))]
    pmg = a[!is.na(a[,"poids.de.mille.grains---poids.de.mille.grains"]) & is.na(a[,"taux.de.proteine---taux.de.proteine"]),c("son","poids.de.mille.grains---poids.de.mille.grains")]
    to_add = merge(prot,pmg,by="son")
    if(nrow(to_add)>0){
      out = list("subsubsection" = "Le taux de protéine en fonction du poids de mille grains"); OUT = c(OUT, out)
      a=rbind(prot_ok,to_add)
      a = a[a$son_year %in% c(as.character(as.numeric(year)-1),year),]
      D=data_all
      D$data$data=a
      p = get.ggplot(data = D, ggplot.type = "data-biplot", in.col = "year", 
                     vec_variables = c("poids.de.mille.grains---poids.de.mille.grains", "taux.de.proteine---taux.de.proteine"), 
                     hide.labels.parts = c("person:year"))
      out = list("figure" = list("caption" = "Relation entre le poids de mille grains et le taux de protéine", "content" = p, "width" = 1)); OUT = c(OUT, out)
    }
    
  }

  
  # 2.5.1.4. Poids de l'épi ----------
  OUT=interaction_and_score(OUT,res_model1,"poids.de.l.epi",table=FALSE,titre = "Le poids des épis",score=TRUE,inter_plot=FALSE)
  
  # 2.5.1.5. La hauteur et la verse ----------
  out = list("subsubsection" = "La hauteur et la verse"); OUT = c(OUT, out)
  comp=0
 # p = get.ggplot(data = data_all, ggplot.type = "data-barplot", x.axis = "year", in.col = "germplasm", 
 #                  vec_variables = "hauteur---hauteur", nb_parameters_per_plot_in.col = 8, merge_g_and_s = TRUE)
  OUT=interaction_and_score(OUT,res_model1,"hauteur",table=FALSE,titre = "Hauteur",score=TRUE,inter_plot=FALSE)
  if(length(grep("Hauteur",OUT))>0){comp=comp+0.5}
#   if(!is.null(p)){
#     out = list("figure" = list("caption" = "Evolution de la \\textbf{hauteur} au cours du temps", "content" = p, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1))
#     OUT = c(OUT, out) ; comp = comp+0.5
#   }else{
#    out = list("text" = "Pas de données pour la hauteur") ; OUT=c(OUT,out)
#  }
  
  # Notations verse : couché, droit... -> mettre sous format 1->5 pour les graphs. 1:à plat ; 5:droit
  D=data_all$data$data
  if(!is.null(D$"verse---verse_2")){
    D$"verse---verse" = unlist(lapply(D$"verse---verse_2",function(x){
      b=NULL
      if(!is.null(x) & x %in% c("à plat","a plat","à  plat","plat","100% a plat","100 a plat")){a="à plat"; b=1}
      if(!is.null(x) & x %in% c("couché","couche","100% couché")){a="couché"; b=1}
      if(!is.null(x) & x %in% c("intermediaire","intermédiaire","intermédiaire- presque droit","100 intermédiaire","100% intermédiaire")){a="intermédiaire"; b=1}
      if(!is.null(x) & x %in% c("presque droit","presque droits","presque droit- droit")){a="presque droit"; b=1}
      if(!is.null(x) & x %in% c("droit","100 droit","100% droit")){a="droit"; b=1}
      if(is.null(x) | is.na(x)){a="NA"; b=1}
      if(is.null(b)){a="NA"}
      return(a)
    }))
    data_all$data$data$"verse---verse" = D$"verse---verse"
    verse = c(1,2,3,4,5) ; names(verse) = c("à plat","couché","intermédiaire","presque droit","droit")
    data_all$data$data$verse = verse[data_all$data$data$`verse---verse_2`]
    
    p = get_heatmap(data = data_all, vec_variables="verse---verse",nb_parameters_per_plot=50)
  }else{p=NULL}
 
  if(!is.null(p)){
    out = list("figure" = list("caption" = "Evolution de la \\textbf{verse} au cours du temps.", "content" = p, "layout" = matrix(1, ncol = 1), "width" = 1))
    OUT = c(OUT, out) ; comp=comp + 0.5
  }else{
    out = list("text" = "Pas de données pour la verse") ; OUT=c(OUT,out)
  }
  
  # 2.5.1.6. La verse en fonction de la hauteur ----------
  if(comp ==1){
    out = list("subsubsection" = "La verse en fonction de la hauteur"); OUT = c(OUT, out)
    D=data_all$data$data
    
    if(FALSE){
      if(!is.null(D$"verse---verse_2")){
        D$"verse---verse" = unlist(lapply(D$"verse---verse",function(x){
          b=NULL
          if(!is.null(x) & x %in% "à plat"){a="1"}
          if(!is.null(x) & x %in% "couché"){a="2"}
          if(!is.null(x) & x %in% "intermédiaire"){a="3"}
          if(!is.null(x) & x %in% " presque droit"){a="4"}
          if(!is.null(x) & x %in% "droit"){a="5"}
          if(is.null(x) | is.na(x)){a="NA"}
          if(is.null(b)){a="NA"}
          return(a)
        }))
        data_all$data$data$"verse---verse" = D$"verse---verse"
      }
    }
    d=data_all
    d$data$data = D[D$son_year %in% c(as.character(as.numeric(year)-1),year),]
    p = get.ggplot(data = d, ggplot.type = "data-biplot", in.col = "year", 
                   vec_variables = c("verse", "hauteur---hauteur"), hide.labels.parts = c("person:year"))
    out = list("figure" = list("caption" = "Relation entre la \\textbf{verse}, 1=à plat, 3=intermédiaire, 5=droit, et la \\textbf{hauteur}", "content" = p, "width" = 1)); OUT = c(OUT, out)
  }
   
if(FALSE){
  # 2.5.1.7. Le rendement ----------
  out = list("subsubsection" = "Le rendement"); OUT = c(OUT, out)
  
  
  # !!!!!!!!! A FAIRE !!!!!!!  
  
  
  p = get.ggplot(data = data_all, ggplot.type = "data-biplot", in.col = "year", 
                 vec_variables = c("verse---verse", "hauteur---hauteur"), hide.labels.parts = c("person:year"))
  out = list("figure" = list("caption" = "Relation entre la verse et la hauteur", "content" = p, "width" = 1)); OUT = c(OUT, out)
}
  
  
  # 2.5.2. Etude de la sélection intra-population ----------
  
  selection_intra = function(res_model1, data_S_year, data_SR_year, variable){
    if (!is.null(data_S_year$data) & is.element(paste(variable,"---",variable,sep=""),colnames(data_S_year$data$data))) {
      data_version = format.data(data_S_year, data.on = "son", fuse_g_and_s = TRUE, format = "PPBstats")
      data_version$version = factor(data_version$version,levels=c(levels(data_version$version)[grep("vrac",levels(data_version$version))], levels(data_version$version) [grep("bouquet",levels(data_version$version))]))
      if(paste(person,year,sep=":")%in% names(res_model1[[variable]]$comp.par$comp.mu$data_mean_comparisons)){
        pS =plot.PPBstats(x=res_model1[[variable]]$comp.par$comp.mu, data_version = data_version, ggplot.type = "barplot", 
                          nb_parameters_per_plot=30)
        if(!is.null(pS$data_mean_comparisons[[1]])){
          pS$data_mean_comparisons[[1]] = lapply(pS$data_mean_comparisons[[1]],function(x){
            x = x + labs(title = variable)
            return(x)
          })
        }
      }else{
        pS =get.ggplot(data_S_year, ggplot.type = "data-barplot", vec_variables=paste(variable,variable,sep="---"), nb_parameters_per_plot_x.axis =20)
        pS=list("data_mean_comparisons"=NULL,"data_env_with_no_controls"=pS,"data_env_whose_param_did_not_converge"=NULL)
      }
     
      if(is.null(unlist(pS[[1]])) & is.null(pS[[2]]) & is.null(pS[[3]])){pS=NULL}
    } else {pS=NULL}
    
    if (person != "ADP" & !is.null(data_SR_year$data)  & is.element(paste(variable,"---",variable,sep=""), colnames(data_SR_year$data$data))) {
      data_version = format.data(data_SR_year, data.on = "son", fuse_g_and_s = TRUE, format = "PPBstats")
      group = unlist(lapply(as.character(data_version$group),function(x){strsplit(x," ")[[1]][length(strsplit(x," ")[[1]])]}))
      levels(data_version$version) = c(levels(data_version$version) [grep("vrac",levels(data_version$version))], levels(data_version$version) [grep("bouquet",levels(data_version$version))])
      data_version = data_version[grep("R",group),]
      pSR =plot.PPBstats(x= res_model1[[variable]]$comp.par$comp.mu, data_version = data_version, ggplot.type = "barplot", 
                          nb_parameters_per_plot=30)
      if(!is.null(pSR$data_mean_comparisons[[1]])){
        pSR$data_mean_comparisons[[1]] = lapply(pSR$data_mean_comparisons[[1]],function(x){
          x = x + labs(title = variable)
          return(x)
        })
      }
      if(is.null(unlist(pSR[[1]])) & is.null(pSR[[2]]) & is.null(pSR[[3]])){pSR=NULL}
      
    } else {pSR=NULL}
    return(list("pS"=pS, "pSR"=pSR))
  }
  
  vec_variables = c("poids.de.mille.grains","taux.de.proteine","poids.de.l.epi")
  
  dS = lapply(vec_variables,function(variable){
    a = selection_intra(res_model1, data_S_year, data_SR_year, variable)
    return(a)
  })
  names(dS) = vec_variables
 
  #  # 2.5.2.1. Poids de mille grains ----------
  # 
  # a = selection_intra(res_model1, data_S_year, data_SR_year, "poids.de.mille.grains")
  # pS1 = a$pS ; pSR1 = a$pSR
  # 
  # # 2.5.2.2. Protéine ----------
  # a = selection_intra(res_model1, data_S_year, data_SR_year, "taux.de.proteine")
  # pS2 = a$pS ; pSR2 = a$pSR
  # 
  # # 2.5.2.3. Poids de l'épi ----------
  # a = selection_intra(res_model1, data_S_year, data_SR_year, "poids.de.l.epi")
  # pS3 = a$pS ; pSR3 = a$pSR
 
  
  textS = list("text" = paste("
                              Vous avez la possibilité d'étudier votre sélection à l'intérieur des populations.\\\\
                              
                              \\textbf{Comment lire les graphiques ?}
                              
                              \\textbf{Le bouquet de sélection}: 
                              Ce graphique présente côte à côte les valeurs moyennes de votre bouquet de sélection (bouquetS) par rapport au vrac (vracS), c'est à dire la moyenne de ce qui n'a pas été sélectionné. 
                              La différence entre la moyenne du bouquet de sélection (bouquetS) et la moyenne du vrac (vracS) s'appelle le \\textbf{le différentiel de sélection}.
                              Cela permet de caractériser la manière dont vous avez sélectionné par rapport à la diversité disponible.
                              Si la sélection a été faite dans le cadre de l'essai sur les mélanges, la sélection correspondant à la modalité 1 (faite dans la population, qui sera semée en pur à l'automne) 
                              est notée #VA, la sélection correspondant à la modalité 2 (faite dans la population, qui sera mélangées aux autres sélections pour reformer le mélange) est noté #JA, tandis que
                              la sélection faite dans le mélange est notée #BA.
                              ",sep=""))
  
  
  textSR = list("text" = paste("
                               \\textbf{La réponse à la sélection}: 
                               Ce graphique présente, en plus des differentiel de sélection faits en ", year, ", la réponse à la sélection. 
                               C'est à dire les épis issus de votre bouquet (bouquetR) et les épis issus du vrac (vracR). 
                               Cela permet de quantifier l'éfficacité de votre sélection. 
                               Par exemple si votre bouquet (bouquetS) en ", year," augmentait le poids de mille grains de 5 grammes par rapport au vrac (vracS), trois scénarios sont possibles:
                               
                               \\begin{itemize}
                               
                               \\item bouquetR est supérieur de 5 grammes à vracR, alors la sélection a été totalement efficace : elle a été faite sur la partie génétique de la variation uniquement.
                               
                               \\item bouquetR est supérieur entre 0 et 5 grammes à vracR, alors la sélection a été partiellement efficace ; elle a été faite sur une partie génétique et également une partie environnementale.
                               
                               \\item bouquetR est supérieur de 0 grammes à vracR, alors la sélection n'a pas été effiace : elle a été faite sur la partie environmentale de la variation uniquement.
                               
                               \\item bouquetR est inférieur à vracR : ce cas n'est théoriquement pas possible. Ce genre de résultats peut venir du manque de puissance du dispositif expérimental ou de l'interaction de la population avec l'année.
                               
                               \\end{itemize}
                               ",sep=""))
  
  
  if(!is.null(unlist(dS))){
    out = list("subsection" = "Etude de la réponse à la sélection"); OUT = c(OUT, out)
  }
  
  pS = lapply(dS,function(x){return(x$pS)})
  pSR = lapply(dS,function(x){return(x$SR)})
get_pS_pSR <- function(data,variable){
  if(!is.null(data)){ 
    if(!is.null(data$data_mean_comparisons)){
      out = list("figure" = list("caption" = paste("Différentiel de sélection pour le \\textbf{",variable,"}. Le symbole au-dessus des populations représentent
                                                    la significativité de la différence de moyenne : le \".\" représente une faible significativité tandis que \"***\" représente une
                                                    forte significativité. S'il n'y a aucun symbole la différence n'est pas significative.",sep=""), "content" = data$data_mean_comparisons, "layout" = matrix(c(1), ncol = 1), "width" = 0.8, "landscape"=TRUE))
      OUT = c(OUT, out) 
    }
    
    if(!is.null(data$data_env_with_no_controls)){
      out = list("figure" = list("caption" = paste("Différentiel de sélection pour le \\textbf{",variable,"}. Les symboles de significativité ne sont pas présents car 
                                                   vous n'avez pas semé 2 répétitions du témoin ou vous ne nous avez pas envoyé des épis pour ces 2 répétitions.
                                                   Les barres qui peuvent être présentent représentent l'écart-type des mesures faites sur les épis",sep=""), "content" = data$data_env_with_no_controls, "layout" = matrix(c(1), ncol = 1), "width" = 0.8, "landscape"=TRUE))
      OUT = c(OUT, out) 
    }
  }
  return(OUT)
}

  
  if( !is.null(unlist(pS)) ){
    out = list("subsubsection" = "Le differentiel de sélection"); OUT = c(OUT, out)
    out = textS; OUT = c(OUT, out)
    
    for (variable in vec_variables){OUT=get_pS_pSR(pS[[variable]],variable)}
  }
  
  if(!is.null(unlist(pSR))){
    out = list("subsubsection" = "La réponse à la sélection"); OUT = c(OUT, out)
    out = textSR; OUT = c(OUT, out)
    
    for (variable in vec_variables){OUT=get_pS_pSR(pSR[[variable]],variable)}
  }
  
  
  # 3. Essai Mélanges --------------------------------------------------------------------------------------------------------------------------------------
  out = list("chapter" = "Résultats de l'essai mélanges"); OUT = c(OUT, out)
  out = list("text" = "Cet essai, mis en place à l'automne 2015, vise à comparer les effets de différentes pratiques de sélection des mélanges sur leur comportement. 
             Les pratiques testées sont : 
             \\begin{itemize}
             \\item deux années de sélection dans les composantes avant de mélanger ; 
             \\item une sélection dans les composantes avant de mélanger puis une sélection dans le mélange ; 
             \\item à partir du mélange créé sans sélection dans les composantes, deux années de sélection dans le mélange.
             \\end{itemize}
             Ces pratiques de sélection sont comparées au mélange évoluant sans sélection massale. 
             Un dossier présentant les résultats des deux première années vous sera envoyé. Si vous voulez mettre en place cet essai chez vous, c'est tout à fait possible,
             contactez votre animateur ou l'équipe de recherche !"); OUT = c(OUT, out)
  
  # 3.1. Résultats sur la ferme -----
  if (is.null(data_PPB_mixture$data)) { 
    out = list("text" = "Vous n'avez pas mis en place l'essai de sélection pour les mélanges sur votre ferme cette année."); OUT=c(OUT,out)
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
    out = list("text" = "Les gaphiques suivant permettent de comparer la valeur du mélange à celles de ses composantes et à la valeur moyenne des composantes."); OUT=c(OUT,out)
    
    
  graphs_ferme_melanges = function(OUT,variable,titre){
    out = list("subsection" = titre); OUT = c(OUT, out)
    
      # Comparaison mélange vs composantes
      p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, data_S = Mixtures_S, melanges_tot = NULL, variable, year=year, model = "model_1", plot.type = "comp.in.farm", person, nb_parameters_per_plot = 20,save=NULL)
      for (i in 1:length(p_melanges[[1]])){
        if(!is.null(p_melanges[[1]][[i]][[1]]$plot)){
          out = list("figure" = list("caption" = paste("Comparaison du \\textbf{",variable,"} du mélange et de ses composantes. 
                                                                              Les populations qui partagent le même groupe (représenté par une même lettre) ne sont pas significativement différentes.
                                                                              ",sep=""), "content" = p_melanges[[1]][[i]][[1]]$plot, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)}
      }
      
      # Comparaison modalités de sélection
      p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, data_S = Mixtures_S, melanges_tot = Mix_tot, variable, year=year, model = "model_1", plot.type = "comp.mod", person, nb_parameters_per_plot = 20,save=NULL)
      for (i in 1:length(p_melanges[[1]])){
        if(!is.null(p_melanges[[1]][[i]]$plot)){
          out = list("figure" = list("caption" = paste("Comparaison du \\textbf{",variable,"} des différentes modalités de sélection des mélanges. 
                                                                              Les modalités qui partagent le même groupe (représenté par une même lettre) ne sont pas significativement différentes.
                                                                              ",sep=""), "content" = p_melanges[[1]][[i]]$plot, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)}
      }
      return(OUT)
  }
    
    # 3.1.1. Poids de mille grains -----
    if ("poids.de.mille.grains" %in% names(res_model1) & length(grep(paste(person,year,sep=":"),names(res_model1[["poids.de.mille.grains"]]$model.outputs$MCMC)))>0){OUT=graphs_ferme_melanges(OUT,"poids.de.mille.grains","poids de mille grains")}
    
    # 3.1.2. Taux de protéine -----
    if ("taux.de.proteine" %in% names(res_model1) & length(grep(paste(person,year,sep=":"),names(res_model1[["taux.de.proteine"]]$model.outputs$MCMC)))>0){OUT=graphs_ferme_melanges(OUT,"taux.de.proteine","Taux de protéine")}
  
    # 3.1.3. Poids de l'épi -----
    if ("poids.de.l.epi" %in% names(res_model1) & length(grep(paste(person,year,sep=":"),names(res_model1[["poids.de.l.epi"]]$model.outputs$MCMC)))>0){OUT=graphs_ferme_melanges(OUT,"poids.de.l.epi","Poids de l'épi")}
  
    # 3.1.4. Hauteur -----
   if ("hauteur" %in% names(res_model1) & length(grep(paste(person,year,sep=":"),names(res_model1[["hauteur"]]$model.outputs$MCMC)))>0){OUT=graphs_ferme_melanges(OUT,"hauteur","Hauteur")}
  
     # 3.1.5. Longueur de l'épi -----
    if ("longueur.de.l.epi" %in% names(res_model1) & length(grep(paste(person,year,sep=":"),names(res_model1[["longueur.de.l.epi"]]$model.outputs$MCMC)))>0){OUT=graphs_ferme_melanges(OUT,"longueur.de.l.epi","Longueur de l'épi")}
  
    # 3.1.6. LLSD -----
   if ("LLSD" %in% names(res_model1) & length(grep(paste(person,year,sep=":"),names(res_model1[["LLSD"]]$model.outputs$MCMC)))>0){OUT=graphs_ferme_melanges(OUT,"LLSD","Distance dernière feuille - base de l'épi")}


    # 3.1.4. La hauteur et la verse ----------
    # A faire !
    # out = list("subsubsection" = "La hauteur et la verse"); OUT = c(OUT, out)
    # 
    # p = get.ggplot(data = data_PPB_mixture, ggplot.type = "data-interaction", x.axis = "year", in.col = "germplasm", 
    # 													 vec_variables = "hauteur---hauteur", nb_parameters_per_plot_in.col = 5, merge_g_and_s = TRUE)
    # out = list("figure" = list("caption" = "Evolution de la hauteur au cours du temps", "content" = p, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)
    # 
    # 
    # p = get.ggplot(data = data_PPB_mixture, ggplot.type = "data-interaction", x.axis = "year", in.col = "germplasm", 
    # 													 vec_variables ="verse---verse", nb_parameters_per_plot_in.col = 5, merge_g_and_s = TRUE)
    # out = list("figure" = list("caption" = "Evolution de la verse au cours du temps", "content" = p, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)
    
      }
  
if(FALSE){
  # 3.2. Résultats sur le réseau de fermes -----
  out = list("section" = "Résultats sur le réseau de fermes"); OUT = c(OUT, out)
  out = list("text" = "Dans cette partie sont présentés les résultats de l'essai mélange sur le réseau de ferme. 
             On s'intéresse dans un premier temps à la différence observée entre la valeur des mélanges et celle de la moyenne de leurs composantes respectives, 
             permettant de constater si les mélanges apportent en moyenne plutôt un gain ou une perte par rapport à la moyenne de leurs composantes. 
             Puis on s'intéresse à la variabilité observée, pour chacun des mélanges testés, de chacune des \"moins bonnes\" composantes, de la meilleure composante,
             de la valeur moyenne des composante et de celle des mélanges.
             Enfin on compare la valeur moyenne de l'ensemble des mélange à celle de l'ensemble des composantes pour voir si on détecte un effet du mélange."); OUT=c(OUT,out)
  
  
  # 3.2.1. Distribution du gain du mélange par rapport à la moyenne de ses composantes sur le réseau -----
  out = list("subsection" = "Distribution du gain du mélange par rapport à la moyenne de ses composantes sur le réseau"); OUT = c(OUT, out)
  out = list("text" = "Ces graphiques présentent le comportement des mélanges par rapport à la moyenne de leurs composantes respectives. 
             Un histogramme décalé vers la droite par rapport à 0 indique qu'une majorité des mélanges se sont mieux comportés que la moyenne de leurs composantes. 
             A l'inverse si l'histogramme est décalé vers la gauche la majorité des mélanges se sont moins bien comportés que la moyenne de leurs composantes."); OUT = c(OUT, out)
  
  melanges_reseau = function(OUT,variable,titre,distrib=TRUE,comp_global=FALSE){
    # Histogramme distribution de l'overyielding
    var = paste(strsplit(variable,"[.]")[[1]],collapse="")
    if (!file.exists(paste(we_are_here,"/figures/Histo_",var,".png",sep=""))){
      p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, data_S = Mixtures_S, melanges_tot = Mix_tot, variable, 
                                   year=c("2016","2017"), model="model_1", plot.type = "mix.gain.distribution", person, nb_parameters_per_plot = 15,
                                   save=paste(we_are_here,"/AnalyseDonnees/donnees_brutes",sep=""))
      save(p_melanges,file=paste(we_are_here,"/figures/Histo_",var,".RData",sep=""))
      png(paste(we_are_here,"/figures/Histo_",var,".png",sep=""))
      p_melanges
      dev.off()
      #    }else{
      #      load(paste(we_are_here,"/figures/Histo_",var,".RData",sep=""))
    }
    out = list("subsection" = titre); OUT = c(OUT, out)
    out = list("includeimage" = list("caption" = paste("Distribution des rapports entre les comportement des mélanges et les comportements moyens
                                                       de leurs composantes respectives pour le ",variable,".
                                                       La ligne rouge verticale indique le gain moyen des mélanges par rapport à la moyenne de leurs composantes respectives 
                                                       tandis que la ligne pointillée noire est fixée sur un gain nul.",sep=""), 
                                     "content" = paste("./figures/Histo_",var,".png",sep=""), "width" = 0.7))
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
  
  # 3.2.1.1. poids de mille grains -----
  OUT = melanges_reseau(OUT,variable="poids.de.mille.grains",titre="Poids de mille grains",distrib=FALSE,comp_global=FALSE)
  
  # 3.2.1.2. Poids de l'épi -----
  OUT = melanges_reseau(OUT,variable="poids.de.l.epi",titre="Poids de l'épi",distrib=FALSE,comp_global=FALSE)
  
  # 3.2.1.3. Hauteur -----
  OUT = melanges_reseau(OUT,variable="hauteur",titre="Hauteur moyenne",distrib=FALSE,comp_global=FALSE)
  
  # 3.2.1.4. Longueur de l'épi -----
  OUT = melanges_reseau(OUT,variable="longueur.de.l.epi",titre="Longueur de l'épi",distrib=FALSE,comp_global=FALSE)
  
  # 3.2.1.5. LLSD -----
  OUT = melanges_reseau(OUT,variable="LLSD",titre="Distance dernière feuille - base de l'épi",distrib=FALSE,comp_global=FALSE)
  
  # 3.2.1.6. Nombre moyen de grains par épi  -----
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
  
}

  
  # 4. Le réseau de fermes -----------------------------------------------------------------------------------------------------------------------------------------
data_network_year = get(load(paste(pathway,"out_data_network_year.RData",sep="/"))) 
  
  # 4.1. Intro ----------
  out = list("chapter" = "Resultats dans le réseau de fermes"); OUT = c(OUT, out)
  
  out = list("text" = paste(
    "Dans ce chapitre, vous trouverez les résultats des données recueillies dans les autres fermes du réseau.
    Attention, ces résultats sont possibles uniquement si nous avons eu vos épis à mesurer.
    
    Ces résultats vous permettent de répondre à la question : \\textbf{Quelles populations serait-il plus intéressant de tester chez moi pour les prochains semis?}
    Ces résultats vous permettent de mobiliser la diversité évaluée dans le réseau de fermes.
    Plutôt que de choisir des populations au hasard dans le réseau, nous vous indiquons les fermes sur lesquelles les populations se sont comportées (un peu) de la même manière que dans votre ferme cette année.
    On ne propose que les fermes pour les années ", (as.numeric(year) - 1), " et ", year,".
    
    Les différentes fermes sont nommées par les initiales des paysans suivies de l'année de récolte. Par exemple TDE:2014 pour la ferme TDE en 2014.
    La carte ci dessous montre le nombre et la répartition des populations récoltées en ",year,".",sep="")); OUT = c(OUT, out)
  
  p = get.ggplot(data = data_network_year, ggplot.type = "network-reproduction-harvested", ggplot.display = "map", pie.size = .15)
  out = list("figure" = list("caption" = paste("Répartition des fermes participant au projet en ", year," et nombre de populations récoltées."), "content" = p, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)
  
  
  # 4.2. Regroupement des fermes ----------
  out = list("section" = "Regroupement des fermes"); OUT = c(OUT, out)
  
  out = list("text" = "
             Le graphique ci-dessous permet de regrouper les combinaisons ferme:année selon leurs similitudes. 
             Plus les fermes sont proches sur le graphique, plus les populations cultivées en commun ont eu un comportement similaires dans ces fermes. 
             Pour réaliser ce graphique, nous avons pris en compte le comportement des populations pour le poids de mille grains, le taux de protéine, la hauteur et le poids des épis.
             
             Vous pouvez essayer des populations cultivées dans une ferme qui apparait proche de la votre. 
             Les dossiers de chaque paysan(ne) est disponible à la demande."); OUT = c(OUT, out)
  
rm(list=c("data_network_year"))
res_model2 = get(load(paste(pathway,"out_res_model2.RData",sep="/")))

  Model2 = lapply(res_model2,function(x){return(x$model.outputs)})
  
  clust = parameter_groups(Model2, parameter = "theta")
  p_PCA = plot.PPBstats(clust,ind_to_highlight=paste(person,year,sep=":"))
  out = list("figure" = list("caption" = paste("Groupement des fermes en cluster selon le comportement des populations. Le point noir correspond à votre ferme cette année."),
                             "content" = p_PCA$clust$cluster_all, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)
  if(paste(person,year,sep=":") %in% p_PCA$clust$cluster_1$data$name){
    out = list("figure" = list("caption" = paste("Représentation des fermes groupées avec la votre pour différentes années. Le point noir correspond à votre ferme cette année."), "content" = p_PCA$clust$cluster_1, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)
  }
  if(paste(person,year,sep=":") %in% p_PCA$clust$cluster_2$data$name){
    out = list("figure" = list("caption" = paste("Représentation des fermes groupées avec la votre pour différentes années. Le point noir correspond à votre ferme cette année."), "content" = p_PCA$clust$cluster_2, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)
  }

  clust_of_personyear = clust$clust$clust[paste(person, year, sep = ":"),"clust"]
  farm_in_the_group = rownames(clust$clust$clust)[which(clust$clust$clust$clust == clust_of_personyear)]
  farm_in_the_group = farm_in_the_group[-which(farm_in_the_group == paste(person, year, sep = ":"))]
  toget = c(grep(year, farm_in_the_group), grep(as.character(as.numeric(year)-1), farm_in_the_group))
  if( length(toget) > 0 ) { farm_in_the_group = farm_in_the_group[toget] }
  farm_in_the_group = farm_in_the_group[order(farm_in_the_group)]
  
  out = list("text" = paste("Les fermes présentes dans le même groupe que votre ferme pour les années ", as.character(as.numeric(year)-1), " et ", year  ," sont : ", paste(farm_in_the_group, collapse = ", "))); OUT = c(OUT, out)
  
  #comp.theta = res_model2$comp.par[[variable]]$comp.theta
  #p_barplot_theta =plot.PPBstats(comp.theta, ggplot.type = "barplot")
  
if(FALSE){
  # 4.3. Caractéristiques génétiques des populations dans le réseau ----------
  out = list("section" = "Caractéristiques génétiques des populations dans le réseau"); OUT = c(OUT, out)
  
  # 4.3.1. Effets génétiques des populations
  out = list("subsection" = "Effets génétiques des populations"); OUT = c(OUT, out)
  
  out = list("text" = paste("
                            Les \\textbf{effets génétiques correpondent à la valeur intrinsèque des populations} : c'est la part de la valeur du caractère qui est due à la génétique de la plante 
                            (on retire l'effet de l'environnement et de l'interaction plante x environnement).
                            Ces caractéristiques génétiques ont été estimées à partir du comportement des populations dans le réseau de fermes pour l'ensemble des années.
                            ")); OUT = c(OUT, out)
  
  out = list("text" = paste("
                            Les tableaux suivant présentent les effets génétiques de populations présentes dans le réseau.
                            A titre comparatif sont ajoutés dans ces tableaux les effets génétiques de populations présentes dans votre ferme cette année. 
                            \\textit{Attention :} sont reportés dans ce tableau les effets génétiques des populations, qui ne correspondent pas aux valeurs mesurées directement sur les populations 
                            (ces dernières prenent en compte les effets de l'environnement et l'interaction population x environnement en plus de l'effet génétique) : il est donc
                            normal que ces valeurs ne soient pas identiques à celles présentées dans la partie \"Mesures à la récolte\".
                            ")); OUT = c(OUT, out)
  
  
  
  
  effet_genet = function(OUT,variable,col_name){
    comp.alpha = res_model2[[variable]]$comp.par$comp.alpha
    
    out = list("text" = paste("Le tableau ci-dessous présente les populations qui ont des effets génétiques les plus faibles et les plus importants dans le réseau pour le \\textbf{",variable,"}.",sep="")); OUT = c(OUT, out)
    tab = comp.alpha$mean.comparisons[,c("parameter","median","groups")]
    tab$parameter = ex_between(tab$parameter, "[", "]")
    colnames(tab) = c("Population",col_name,"groupe")
    attributes(tab)$invert = FALSE
    ferme = tab[grep(paste(pop_ferme,collapse="|"),tab$Population),]
    ferme$Population = paste(ferme$Population,"*",sep=" ")
    tab_tail = rbind(tail(tab),tail(ferme,2)) ; tab_tail = tab_tail[order(tab_tail[,2]),]
    tab_head = rbind(head(tab),head(ferme,2)) ; tab_head = tab_head[order(tab_head[,2]),]
    
    out = list("table" = list("caption" = paste("Populations présentant des effets génétiques les plus faibles dans le réseau pour le ",variable,".
                                                A titre comparatif sont reportées dans le tableau les populations présentes chez vous cette année ayant les effets génétiques les plus faibles pour le \\textbf{",variable,"} (*).",sep=""), "content" = tab_head)); OUT = c(OUT, out)
    out = list("table" = list("caption" = paste("Populations présentant des effets génétiques les plus importants dans le réseau pour le ",variable,".
                                                A titre comparatif sont reportées dans le tableau les populations présentes chez vous cette année ayant les effets génétiques les plus importants pour le \\textbf{",variable,"} (*).",sep=""), "content" = tab_tail)); OUT = c(OUT, out)
    
    return(OUT)
  }
  
  if("poids.de.mille.grains" %in% names(Model2)){OUT = effet_genet(OUT,"poids.de.mille.grains","Poids de mille grains moyen")}
  if("taux.de.proteine" %in% names(Model2)){OUT = effet_genet(OUT,"taux.de.proteine","Taux de protéine moyen")}
  if("poids.de.l.epi" %in% names(Model2)){OUT = effet_genet(OUT,"poids.de.l.epi","Poids d'épi moyen")}
  
  
  # 4.3.2. Sensibilité des populations à l'interaction
  out = list("subsection" = "Sensibilité des populations à l'interaction"); OUT = c(OUT, out)
  
  out = list("text" = paste("
                            La sensibilité à l'interation des populations renseigne sur leur comportement en moyenne dans le réseau par rapport aux autres populations.
                            Moins elles sont sensibles à l’interaction, plus elles se comportent moyennement de la même manière dans les fermes par rapport aux autres populations.
                            Plus elles sont sensibles et plus leur comportement va différer du comportement moyen des population lorsqu'on la cultive sur d'autres fermes.
                            ")); OUT = c(OUT, out)
  
  sensibilite = function(tab,variable){
    comp.beta = res_model2[[variable]]$comp.par$comp.beta$mean.comparisons
    tab1 = tail(comp.beta)
    tab=cbind.data.frame(tab,rownames(tab1))
    colnames(tab)[ncol(tab)]=variable
    return(tab)
  }
  
  tab=data.frame(rep(0,6))
  name=NULL
  if ("poids.de.mille.grains" %in% names(Model2)) {tab=sensibilite(tab,"poids.de.mille.grains");name=c(name,"poids.de.mille.grains")}
  if ("taux.de.proteine" %in% names(Model2)) {tab=sensibilite(tab,"taux.de.proteine");name=c(name,"taux.de.proteine")}
  if ("poids.de.l.epi" %in% names(Model2)) {tab=sensibilite(tab,"poids.de.l.epi");name=c(name,"poids.de.l.epi")}
  
  tab=tab[,-1]
  
  tab = as.data.frame(apply(tab,2,function(x) {unlist(ex_between(x,"[","]"))}))
  attributes(tab)$invert = FALSE
  out = list("table" = list("caption" = paste("Populations qui sont le moins sensible à l'interaction dans le réseau pour le ",
                                              paste(name,collapse=", "),sep=" "), "content" = tab)); OUT = c(OUT, out)
  
  # 4.3.3. Effet génétique en fonction de la sensibilité à l'environnement
  out = list("subsection" = "Effet génétique en fonction de la sensibilité à l'environnement"); OUT = c(OUT, out)
  
  out = list("text" = paste("Sur les graphiques suivants sont placées les population selon leur effet génétique et leur sensibilité à l'interaction.
                            Les populations situées en bas à droite ont un effet génétique important et sont peu sensibles à l'interaction.
                            A l'inverse les populations en haut à gauche ont un effet génétique plus faibles que la moyenne des populations et sont sensibles à l'interaction.")); OUT = c(OUT, out)
  
  genet_sensi = function(OUT,variable){
    comp.alpha = res_model2[[variable]]$comp.par$comp.alpha
    comp.beta = res_model2[[variable]]$comp.par$comp.beta
    comp.beta$mean.comparisons = cbind(comp.beta$mean.comparisons,rownames(comp.beta$mean.comparisons))
    colnames(comp.beta$mean.comparisons) = c("median","parameter")
    p_alpha_beta =plot.PPBstats(x = comp.alpha, y = comp.beta,  ggplot.type = "biplot-alpha-beta", nb_parameters_per_plot = 100)
    p_alpha_beta=lapply(p_alpha_beta,function(x){x=x+labs(x="Effet génétique",y="sensibilité")})
    out = list("figure" = list("caption" = paste("Effet génétique en fonction de la sensibilité à l'environnement pour le \\textbf{",variable,"} de toutes les populations.
                               ",sep=""), "content" = p_alpha_beta, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)
    return(OUT)
  }
  
  if ("poids.de.mille.grains" %in% names(Model2)) { OUT=genet_sensi(OUT,"poids.de.mille.grains") }
  if ("taux.de.proteine" %in% names(Model2)) { OUT=genet_sensi(OUT,"taux.de.proteine") }
  #  if ("poids.de.l.epi" %in% names(Model2)) { OUT=genet_sensi(OUT,"poids.de.l.epi") }

}

  # 4.4. Prédire le passé ----------
  out = list("section" = "Prédire le passé"); OUT = c(OUT, out)
  
  out = list("text" = "
             Ici nous vous proposons de prédire les valeurs qu’auraient eu certaines populations dans votre ferme cette année : on prédit le passé ! 
             Cette information est issue des modèles statistiques que nous avons développés et est possible si nous avons reçu les épis cette année. 
             A titre de comparaison, vous retrouverez notées avec une * les valeurs des trois meilleures populations cultivées chez vous cette année.
             "); OUT = c(OUT, out)
  
  # Comme tous modèles, il donne une information avec une certaine confiance qui est donnée en pourcentage.
  
  # Mettre l'intervalle de confiance dans notre cas: graph que pour les premiers ?!? A creuser
  tab_predict_past = function(OUT,variable,data_S){
    data_S=data_S$data$data
    data_S = unique(data_S[,c("son","expe","sl_statut","expe_name","expe_name_2","son_germplasm","father","father_germplasm","son_person")])
    data_S = data_S[grep("bouquet",data_S$sl_statut),]
    Sel_year = unlist(lapply(as.character(data_S$son),function(x){strsplit(x,"_")[[1]][1]}))
    out = res_model2[[variable]]$predict.past[[paste(person, year, sep = ":")]]$MCMC
    if(!is.null(out)){
      quantiles=NULL
      for(i in 1:ncol(out)){quantiles = rbind(quantiles,quantile( out[i,], probs=c(0, 0.05, 0.10, 0.50, 0.90, 0.95, 1)))}
      rownames(quantiles)=unlist(ex_between(names(out),"[",","))
      
      if( nrow(quantiles) > 0 ) {
        quantiles = quantiles[order(quantiles[,"50%"], decreasing = TRUE),]
        quantiles = quantiles[c(c(1:5),c((nrow(quantiles) - 5):nrow(quantiles))),]
        quantiles = cbind.data.frame(rownames(quantiles), quantiles$`50%`)
        
        if(length(grep(paste(person,year,sep=":"),names(res_model1[[variable]]$comp.par$comp.mu$data_mean_comparisons)))>0){
          tab_pop = res_model1[[variable]]$comp.par$comp.mu$data_mean_comparisons[[paste(person,year,sep=":")]]$mean.comparisons[,c("parameter","median")]
          if(!is.null(Sel_year)){tab_pop = tab_pop[-grep(paste(Sel_year,collapse="|"),tab_pop$parameter),]}
          tab_pop=tail(tab_pop,n=3)
          germ = unlist(ex_between(tab_pop$parameter, "[", "]")) ; germ = unlist(lapply(germ,function(x){strsplit(x,",")[[1]][1]}))
          tab_pop$parameter = paste(germ,"*",sep=" ") ; colnames(tab_pop)=colnames(quantiles)
          quantiles = rbind(quantiles,tab_pop) 
        }
        quantiles = quantiles[order(quantiles[,2]),]
        colnames(quantiles) = c("population",variable)
      }
    }else{quantiles=NULL}
 
    
    if (!is.null(quantiles)){
      attributes(quantiles)$invert = FALSE
      out = list("table" = list("caption" = paste("Populations qui auraient eu des \\textbf{",variable,"} les plus importants chez vous cette année. A titre de comparaison, les meilleures populations pour cette variable semées cette année sur votre ferme sont notées avec *.",sep=""), "content" = quantiles)); OUT = c(OUT, out)  
    }else{
      out = list("text" = paste(variable," : il n'est pas possible de prédire ces valeurs car nous n'avons aucune données phénotypiques sur votre ferme pour cette année.",sep="") 
); OUT = c(OUT, out)
    }
    
    return(OUT)
  }
  

  if ("poids.de.mille.grains" %in% names(Model2) & !is.null(res_model2[["poids.de.mille.grains"]]$predict.past)) {OUT = tab_predict_past(OUT,"poids.de.mille.grains",data_S_year)}
  if ("taux.de.proteine" %in% names(Model2) & !is.null(res_model2[["taux.de.proteine"]]$predict.past)) {OUT = tab_predict_past(OUT,"taux.de.proteine",data_S_year)}
  if ("poids.de.l.epi" %in% names(Model2) & !is.null(res_model2[["poids.de.l.epi"]]$predict.past)) {OUT = tab_predict_past(OUT,"poids.de.l.epi",data_S_year)}

  
  system("mkdir ./feedback_folder/figures")
  system("cp ./figures/*.png ./feedback_folder/figures")
  rm(list=setdiff(ls(), c("we_are_here","OUT","person","year")))
  
  # /!\ Get pdf ----------
  get.pdf(dir = paste(we_are_here, "/feedback_folder", sep = ""), 
          form.name = paste(person, year, sep = ":"), 
          LaTeX_head = "../tex_files/structure.tex", 
          LaTeX_body = OUT, 
          compile.tex = TRUE,
          color1 = "mln-green", 
          color2 = "mln-brown"
  )
  
}
