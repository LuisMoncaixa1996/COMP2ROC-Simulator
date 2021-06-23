## add sub menu items in shinydashboard sidebar

# load the required packages
require(devtools)
install_version("ROCR", version = "1.0-7", repos = "http://cran.us.r-project.org")
library(shiny)
library(shinydashboard)
library(DT)
library(Comp2ROC)
library(plotly)
library(shinyWidgets)

#Fun??o do COMP2ROC para apresentar representa??es gr?ficas das curvas ROC emp?ricas e ?reas abaixo da curva
roc.curves.boot_v2 <-
    function(data,  nb=1000,  alfa=.05,  name,  mod1,  mod2,  paired) {
        if (missing(paired))
            paired = data$Related
        #cat("Related: ", paired, "\n")
        
        nomeg = paste(name)
        
        sim1.ind = unlist(data[1])
        sim2.ind = unlist(data[2])
        
        sim1.sta = unlist(data[3])
        
        if (!paired)
            sim2.sta = unlist(data[4])
        else
            sim2.sta = sim1.sta
        
        imax  = length(sim1.sta[sim1.sta == 0])
        jmax  = length(sim1.sta[sim1.sta == 1])
        imax2 = length(sim2.sta[sim2.sta == 0])
        jmax2 = length(sim2.sta[sim2.sta == 1])
        
        total_cases  = imax + jmax
        total_cases2 = imax2 + jmax2
        
        sim1 = t(rbind(sim1.sta, sim1.ind)[, order(sim1.sta, 
                                                   sim1.ind)])
        sim2 = t(rbind(sim2.sta, sim2.ind)[, order(sim2.sta, 
                                                   sim2.ind)])
        dados = rbind(cbind(sim1[, 2],
                            sim1[, 1]),
                      cbind(sim2[, 2],
                            sim2[, 1]))
        
        mod = 2
        
        norm.nRows    = max(imax, imax2)
        abnorm.nRowns = max(jmax, jmax2)
        # norm defines the negative cases
        norm = array(data=NA, 
                     c(norm.nRows, 
                       mod))
        # abnorm defines the positive cases
        abnorm = array(data=NA, 
                       c(abnorm.nRowns, 
                         mod))
        
        # data are atributed to the arrays norm and abnorm
        for (i in 1:imax)
            norm[i, 1] = dados[              i, 1]
        for (i in 1:imax2)
            norm[i, 2] = dados[i + total_cases, 1]
        
        for (j in 1:jmax)
            abnorm[j, 1] = dados[               j + imax, 1]
        for (j in 1:jmax2)
            abnorm[j, 2] = dados[j + total_cases + imax2, 1]
        
        # calculates various values (frequencies, ...)
        sim1.pred <- prediction(sim1.ind, sim1.sta)
        sim2.pred <- prediction(sim2.ind, sim2.sta)
        
        # calculate points of the curves (TPR and FPR)
        sim1.curve <- performance(sim1.pred, "tpr", "fpr")
        sim2.curve <- performance(sim2.pred, "tpr", "fpr")
        
        # calculates the areas
        sim1.auc <- unlist(performance(sim1.pred, "auc")@y.values)
        sim2.auc <- unlist(performance(sim2.pred, "auc")@y.values)
        
        
        # calculates FPR and TPR
        sim1.fpr <- array(unlist(sim1.curve@x.values))
        sim1.tpr <- array(unlist(sim1.curve@y.values))
        sim2.fpr <- array(unlist(sim2.curve@x.values))
        sim2.tpr <- array(unlist(sim2.curve@y.values))
        
        diff.areas = sim1.auc - sim2.auc
        
        roc.curves.plot(sim1.curve,
                        sim2.curve,
                        mod1,
                        mod2)
        
        #creation of the structure
        sim1.ind.sample   = array(data=NA, c(length(sim1.sta), nb))
        sim2.ind.sample   = array(data=NA, c(length(sim2.sta), nb))
        sim1.fpr.sample   = array(data=NA, c(length(sim1.fpr), nb))
        sim2.fpr.sample   = array(data=NA, c(length(sim2.fpr), nb))
        sim1.tpr.sample   = array(data=NA, c(length(sim1.tpr), nb))
        sim2.tpr.sample   = array(data=NA, c(length(sim2.tpr), nb))
        sim1.auc.sample   = c()
        sim2.auc.sample   = c()
        diff.auc.sample   = c()
        dist1.sample      = array(data=NA, c(             100, nb))
        dist2.sample      = array(data=NA, c(             100, nb))
        diff.areas.sample = array(data=NA, c(             101, nb))
        
        for(b in 1:nb){
            #sim1.ind.data = which(norm[, 1] == 1)
            
            repeat {
                sim1.ind.sample[, b] = c(sample(  norm[, 1], imax,  TRUE),
                                         sample(abnorm[, 1], jmax,  TRUE))
                if( length( unique(sim1.sta[ which( !is.na(sim1.ind.sample[, b]))])) == 2)
                    break
            }
            repeat{
                sim2.ind.sample[, b] = c(sample(  norm[, 2], imax2, TRUE),
                                         sample(abnorm[, 2], jmax2, TRUE))
                if( length( unique(sim2.sta[ which( !is.na(sim2.ind.sample[, b]))])) == 2)
                    break
            }
            # calculates various values (frequencies,  ...)
            sim1.pred.sample <- prediction(sim1.ind.sample[, b],
                                           sim1.sta)
            
            #if (length(sim2.ind.sample[, b][!is.na(sim2.ind.sample[, b])]) != length(sim2.sta[!is.na(sim2.ind.sample[, b])])) {
            #    print("Here!")
            #}
            sim2.pred.sample <- prediction(sim2.ind.sample[, b],
                                           sim2.sta)  #[!is.na(sim2.ind.sample[, b])]
            
            # calculate points of the curves (TPR and FPR)
            sim1.curve.sample <- performance(sim1.pred.sample, "tpr", "fpr")
            sim2.curve.sample <- performance(sim2.pred.sample, "tpr", "fpr")
            
            # calculates the areas
            sim1.auc.sample[b] <- unlist(performance(sim1.pred.sample, "auc")@y.values)
            sim2.auc.sample[b] <- unlist(performance(sim2.pred.sample, "auc")@y.values)
            
            diff.auc.sample[b] <- sim1.auc.sample[b] - sim2.auc.sample[b]
            
            # calculate FPR and TPR
            sim1.fpr.sample[1:length(unlist(sim1.curve.sample@x.values)), b] <- array(unlist(sim1.curve.sample@x.values))
            sim1.tpr.sample[1:length(unlist(sim1.curve.sample@y.values)), b] <- array(unlist(sim1.curve.sample@y.values))
            sim2.fpr.sample[1:length(unlist(sim2.curve.sample@x.values)), b] <- array(unlist(sim2.curve.sample@x.values))
            sim2.tpr.sample[1:length(unlist(sim2.curve.sample@y.values)), b] <- array(unlist(sim2.curve.sample@y.values))
            
            result = rocsampling(array(unlist(sim1.curve.sample@x.values)),
                                 array(unlist(sim1.curve.sample@y.values)),
                                 array(unlist(sim2.curve.sample@x.values)),
                                 array(unlist(sim2.curve.sample@y.values)))
            dist1.sample[, b] = result$dist1
            dist2.sample[, b] = result$dist2
            diff.areas.sample[, b] = result$diffareas
        }
        
        
        result = rocsampling(sim1.fpr,
                             sim1.tpr,
                             sim2.fpr,
                             sim2.tpr)
        rocsampling.summary(result,
                            mod1,
                            mod2)
        
        dist1 = result$dist1                                
        dist2 = result$dist2
        diff.dist  = dist1 - dist2
        diff.areas = result$diffareas
        
        rstar1 = sqrt(total_cases)*(dist1.sample - dist1)
        rstar2 = sqrt(total_cases)*(dist2.sample - dist2)
        rstar  = rstar1 - rstar2
        
        mean1.boot = c()
        sd1.boot   = c()
        mean2.boot = c()
        sd2.boot   = c()
        mean.boot  = c()
        sd.boot    = c()
        qL.boot    = c()
        qU.boot    = c()
        mean.rstar = c()
        sd.rstar   = c()
        qL.rstar   = c()
        qU.rstar   = c()
        
        for(i in 1:length(dist1.sample[, 2])) {
            mean1.boot[i] = mean(dist1.sample[i,])
            mean2.boot[i] = mean(dist2.sample[i,])
            mean.boot[i]  = mean(dist1.sample[i,] - dist2.sample[i,])
            
            sd1.boot[i] = sd(dist1.sample[i,])
            sd2.boot[i] = sd(dist2.sample[i,])
            sd.boot[i]  = sd(dist1.sample[i,] - dist2.sample[i,])
            
            qL.boot[i] = quantile(dist1.sample[i,] - dist2.sample[i,],     alfa/2)
            qU.boot[i] = quantile(dist1.sample[i,] - dist2.sample[i,], 1 - alfa/2)
            
            mean.rstar[i] = mean(    rstar[i,])
            sd.rstar[i]   = sd(      rstar[i,])
            qL.rstar[i]   = quantile(rstar[i,],     alfa/2)
            qU.rstar[i]   = quantile(rstar[i,], 1 - alfa/2)
        }
        
        qL.t = qt(    alfa/2, total_cases - 2)
        qU.t = qt(1 - alfa/2, total_cases - 2)
        
        IC.par       = array(data=NA, c(100, 2))
        IC.per       = array(data=NA, c(100, 2))
        IC.bcp       = array(data=NA, c(100, 2))
        IC.par.rstar = array(data=NA, c(100, 2))
        IC.per.rstar = array(data=NA, c(100, 2))
        
        for(i in 1:length(qL.rstar)) {
            IC.par[i, 1] = diff.dist[i] - mean.boot[i] + qL.t*sd.boot[i]
            IC.par[i, 2] = diff.dist[i] - mean.boot[i] + qU.t*sd.boot[i]
            IC.per[i, 1] = diff.dist[i] - mean.boot[i] +      qL.boot[i]  #diff.dist[i] + qL.boot[i]  #*sd.boot[i]
            IC.per[i, 2] = diff.dist[i] - mean.boot[i] +      qU.boot[i]  #diff.dist[i] + qU.boot[i]  #*sd.boot[i]
            
            IC.bcp[i, 1] = diff.dist[i] - mean.boot[i] +      qL.boot[i]  #diff.dist[i] + qL.boot[i]  #*sd.boot[i]
            IC.bcp[i, 2] = diff.dist[i] - mean.boot[i] +      qU.boot[i]  #diff.dist[i] + qU.boot[i]  #*sd.boot[i]
            
            IC.par.rstar[i, 1] = diff.dist[i] + qL.t*sd.rstar[i]
            IC.par.rstar[i, 2] = diff.dist[i] + qU.t*sd.rstar[i]
            IC.per.rstar[i, 1] = diff.dist[i] +      qL.rstar[i]  #*sd.boot[i]
            IC.per.rstar[i, 2] = diff.dist[i] +      qU.rstar[i]  #*sd.boot[i]
        }
        
        
        LB = min(c(IC.par[, 1],
                   IC.per[, 1]))
        UB = max(c(IC.par[, 2],
                   IC.per[, 2]))
        
        
        mean.areas.boot = c()
        sd.areas.boot   = c()
        qL.areas.boot   = c()
        qU.areas.boot   = c()
        for(i in 1:length(diff.areas.sample[, 2])) {
            mean.areas.boot[i] = mean(    diff.areas.sample[i,])
            sd.areas.boot[i]   = sd(      diff.areas.sample[i,])            
            qL.areas.boot[i]   = quantile(diff.areas.sample[i,],     alfa/2)
            qU.areas.boot[i]   = quantile(diff.areas.sample[i,], 1 - alfa/2)
        }
        IC.areas.par = array(data=NA, c(101, 2))
        IC.areas.per = array(data=NA, c(101, 2))
        for(i in 1:length(qL.areas.boot)) {
            IC.areas.par[i, 1] = diff.areas[i] - mean.areas.boot[i] + qL.t*sd.areas.boot[i]
            IC.areas.par[i, 2] = diff.areas[i] - mean.areas.boot[i] + qU.t*sd.areas.boot[i]
            IC.areas.per[i, 1] = diff.areas[i] - mean.areas.boot[i] +      qL.areas.boot[i]  #diff.areas[i] + qL.areas.boot[i]  #*sd.boot[i]
            IC.areas.per[i, 2] = diff.areas[i] - mean.areas.boot[i] +      qU.areas.boot[i]  #diff.areas[i] + qU.areas.boot[i]  #*sd.boot[i]
        }
        
        LB = min(c(IC.areas.par[, 1], IC.areas.per[, 1]))
        UB = max(c(IC.areas.par[, 2], IC.areas.per[, 2]))
        
        degrees = result$lineslope*90/(pi/2)
        
        plot(degrees[1:101], c(rep(LB, 50), rep(UB, 51)), type='n', xlab="Degrees", ylab="Area between curves")
        abline(h=0, col="gray60")
        lines(degrees[1:101], IC.areas.per[, 1], col='red',    lty=8)    
        #lines(degrees[1:101], IC.areas.par[, 1], col='red',    lty=1)    
        lines(degrees[1:101], diff.areas,        col='blue')     
        lines(degrees[1:101], IC.areas.per[, 2], col='green',  lty=9)
        #lines(degrees[1:101], IC.areas.par[, 2], col='green',  lty=3)
        title("Areas Between ROC Curves")
        
        IC1.auc.per = c()
        IC2.auc.per = c()
        IC1.auc.per[1] = quantile(sim1.auc.sample,     alfa/2)
        IC1.auc.per[2] = quantile(sim1.auc.sample, 1 - alfa/2)
        IC2.auc.per[1] = quantile(sim2.auc.sample,     alfa/2)
        IC2.auc.per[2] = quantile(sim2.auc.sample, 1 - alfa/2)
        
        IC.diff.auc.per = c()
        IC.diff.auc.per[1] = quantile(diff.auc.sample,     alfa/2)
        IC.diff.auc.per[2] = quantile(diff.auc.sample, 1 - alfa/2)
        
        resultdelong = comp.roc.delong(sim1.ind,
                                       sim1.sta,
                                       sim2.ind,
                                       sim2.sta,
                                       paired)
        
        nc = 0
        for(i in 1:(length(result$diffareas) - 1)) {
            if (result$diffareas[i] < 0 && result$diffareas[i + 1] > 0)
                nc = nc + 1
            if (result$diffareas[i] > 0 && result$diffareas[i + 1] < 0)
                nc = nc + 1
        }
        
        resultboot = comp.roc.curves(result, ci.flag=TRUE, graph.flag=F, nomeg)
        
        #if (k == 1) {
        #    cat("\n   Delong & & & & & & & & Permutation & & & & Bootstrap & & & & & &\\\n")
        #    cat("\n  Delong & & & & & & & & Permutation & & & & Bootstrap & & & & & &\\\n", file=paste(nomeg, "Results.txt"), append=TRUE)
        #    cat("\n & AUC_1 & SE_1 & AUC_2 & SE_2 & R & Diff & Z & pvalue & AUC_1 & AUC_2 & pvalue & ncoss & IC_AUC_1 & IC_AUC_1 & IC_AUC_2 & IC_AUC_2 & IC_DIFF & IC_DIFF &\\\n")
        #    cat("\n & AUC_1 & SE_1 & AUC_2 & SE_2 & R & Diff & Z & pvalue & AUC_1 & AUC_2 & pvalue & ncoss & IC_AUC_1 & IC_AUC_1 & IC_AUC_2 & IC_AUC_2 & IC_DIFF & IC_DIFF &\\\n", file=paste(nomeg, "Results_zang.txt"), append=TRUE);
        #}
        #cat(resultdelong$AUC[1], resultdelong$SE[1], resultdelong$AUC[2], resultdelong$SE[2], resultdelong$R[1, 2], resultdelong$AUC[1] - resultdelong$AUC[2], resultdelong$Z, resultdelong$pvalue, result$AUC1, result$AUC2, resultboot$pvalue, nc, IC1.auc.per[1], IC1.auc.per[2], IC2.auc.per[1], IC2.auc.per[2], IC.diff.auc.per[1], IC.diff.auc.per[2], "\\\n",  sep=" & ")
        #cat(resultdelong$AUC[1], resultdelong$SE[1], resultdelong$AUC[2], resultdelong$SE[2], resultdelong$R[1, 2], resultdelong$AUC[1] - resultdelong$AUC[2], resultdelong$Z, resultdelong$pvalue, result$AUC1, result$AUC2, resultboot$pvalue, nc, IC1.auc.per[1], IC1.auc.per[2], IC2.auc.per[1], IC2.auc.per[2], IC.diff.auc.per[1], IC.diff.auc.per[2], "\\\n",  sep=" & ", file=paste(nomeg, "Results_zang.txt"), append=TRUE)
        
        resultlist = list(Area1=resultdelong$AUC[1],
                          SE1=resultdelong$SE[1],
                          Area2=resultdelong$AUC[2],
                          SE2=resultdelong$SE[2],
                          CorrCoef=resultdelong$R[1, 2],
                          diff=(resultdelong$AUC[1] - resultdelong$AUC[2]),
                          zstats=resultdelong$Z,
                          pvalue1=resultdelong$pvalue,
                          TrapArea1=result$AUC1,
                          TrapArea2=result$AUC2,
                          bootpvalue=resultboot$pvalue,
                          nCross=nc,
                          ICLB1=IC1.auc.per[1],
                          ICUB1=IC1.auc.per[2],
                          ICLB2=IC2.auc.per[1],
                          ICUB2=IC2.auc.per[2],
                          ICLBDiff=IC.diff.auc.per[1],
                          ICUBDiff=IC.diff.auc.per[2])
        return(resultlist)
    }

#Interface da aplica??o
ui <- shinyUI(
    dashboardPage(skin = 'black',
        dashboardHeader(title = "COMP2ROC Simulator", titleWidth = 300,tags$li(class = "dropdown",
                                                                     dropMenu(
                                                                         dropdownButton("Info", circle = TRUE, status = 'primary', icon = icon('info'),size = "sm", up = FALSE),
                                                                         h3(strong('Informacao')),
                                                                         br(),
                                                                         h5(div(includeMarkdown("Info.Rmd"), 
                                                                                align="justify")),
                                                                         placement = "right",
                                                                         arrow = TRUE) )),
        dashboardSidebar(
            width = 350,
            sidebarMenu(id = 'sidebarmenu',
                        # Menu correspondente ao Upload dos datasets
                        menuItem("Upload Dataset", tabName = "Upload", icon = icon("upload")),
                        
                        # Menus de compara??o entre curvas dependendo do tipo de vari?vel
                        menuItem("Comparacao de curvas ROC - Dependentes",
                                 icon = icon("clipboard"),
                                 menuSubItem("Escolher Variaveis",
                                             tabName = "Escolher_Variaveis",
                                             icon = icon("edit")),
                                 menuSubItem("Resultados Graficos",
                                             tabName = "Resultados_Graficos",
                                             icon = icon("chart-area")),
                                 menuSubItem("Resultados Estatisticos",
                                             tabName = "Resultados_Estatisticos",
                                             icon = icon("list-ol"))),
                                             
                        menuItem("Comparacao de curvas ROC - Independentes",
                                 icon = icon("clipboard"),
                                 menuSubItem("Escolher Variaveis",
                                             tabName = "Escolher_Variaveis_Ind",
                                             icon = icon("edit")),
                                 menuSubItem("Resultados Graficos",
                                             tabName = "Resultados_Graficos_Ind",
                                             icon = icon("chart-area")),
                                 menuSubItem("Resultados Estatisticos",
                                             tabName = "Resultados_Estatisticos_Ind",
                                             icon = icon("list-ol"))),
                        
                        #Menu informativo com refer?ncias acerca da aplica??o
                        menuItem("Sobre", tabName = "Sobre", icon = icon("book"))
                        )),
        
        
        dashboardBody(
            tabItems(
                # Toda a interface do menu Upload
                tabItem("Upload", sidebarLayout(
                    sidebarPanel(
                        fileInput("file1",
                                  "Pick your Data",
                                  multiple = TRUE,
                                  accept = c("text/csv","test/comma-separated-values, text/plain",".csv")),
                        tags$hr(),
                        checkboxInput("header","Header", TRUE),
                        radioButtons("sep","Separator",choices = c(Comma = ",",
                                                                   Semicolon = ";",
                                                                   Tab = "\t",
                                                                   selected = ",")),
                        tags$hr(),
                        radioButtons("quote","Quote",choices = c(None = "" ,
                                                                 "Double Quote" = '"',
                                                                 "Single Quote" = "'",
                                                                 selected = '"')),
                        tags$hr(),
                        radioButtons("disp","Display",choices = c(Head = "head",
                                                                  ALL = "all",
                                                                  selected = "head")),
                        checkboxGroupInput("Dependency", "Type of variables:", c("Dependentes", "Independentes")),
                        actionButton("submitbutton", "Submit", class = "btn btn-primary")
                    ),
                    
                    mainPanel(
                        DT::dataTableOutput("contents"), style = "height:800px;overflow-x: scroll;"
                    )
                )),
                #Interface do segundo menu (Dependentes) -> Escolha das vari?veis
                tabItem("Escolher_Variaveis", h4("Escolha as variaveis de interesse"), sidebarLayout(
                    sidebarPanel(
                        textInput("DPred1", "Prediction 1",""),
                        checkboxInput("Direction1", "Direction1", value = T),
                        textInput("DPred2", "Prediction 2",""),
                        checkboxInput("Direction2", "Direction2", value = T),
                        textInput("DResult", "Result", ""),
                        actionButton("Dsubmitbutton", "Submit", class = "btn btn-primary")),
                    mainPanel(
                        DT::dataTableOutput("Dependentes"), style = "height:800px;overflow-x: scroll;"
                        
                    ))),
                #Interface do segundo menu (Dependentes) -> Resultados Gr?ficos
                tabItem('Resultados_Graficos',  
                        plotOutput("plotOut"), downloadButton("Download_Plot", label = "Download"), plotOutput("plotOut2"),downloadButton("Download_Plot_2", label = "Download")),
                
                #Interface do segundo menu (Dependentes) -> Resultados Estat?sticos
                tabItem('Resultados_Estatisticos', sidebarLayout(
                    sidebarPanel(
                        checkboxGroupInput("Resultados", "Show Results:",c("Prediction 1","Prediction 2","Statistical Overall"), selected =c("Prediction 1","Prediction 2","Statistical Overall"))),
                    mainPanel(
                        tags$label(h2("Resultados Estatisticos")),
                        verbatimTextOutput("OutputDependentes"),
                        
                    ))),
                #Interface do terceiro menu (Independentes) -> Escolha das vari?veis
                tabItem("Escolher_Variaveis_Ind", h4("Escolha as variaveis de interesse"),sidebarLayout(
                    sidebarPanel(
                        textInput("IPred1", "Prediction 1",""),
                        textInput("IResult1", "Result 1"),
                        checkboxInput("Direction_1", "Direction_1", value = T),
                        textInput("IPred2", "Prediction 2",""),
                        textInput("IResult2", "Result 2",""),
                        checkboxInput("Direction_2", "Direction_2", value = T),
                        actionButton("Isubmitbutton", "Submit", class = "btn btn-primary")),
                    mainPanel(
                        DT::dataTableOutput("Independentes"), style = "height:800px;overflow-x: scroll;"
                    ))),
                #Interface do terceiro menu (Independentes) -> Resultados Gr?ficos
                    tabItem('Resultados_Graficos_Ind', 
                            plotOutput("IplotOut"), downloadButton("IDownload_Plot", label = "Download"), plotOutput("IplotOut2"),downloadButton("IDownload_Plot_2", label = "Download")),
                
                #Interface do terceiro menu (Independentes) -> Resultados Estat?sticos 
                    tabItem('Resultados_Estatisticos_Ind', sidebarLayout(
                        sidebarPanel(
                            checkboxGroupInput("IResultados", "Show Results:",c("Prediction 1","Prediction 2","Statistical Overall"), selected =c("Prediction 1","Prediction 2","Statistical Overall"))),
                        mainPanel(
                            tags$label(h2("Resultados Estatisticos")),
                            verbatimTextOutput("OutputIndependentes"),
                            ))),
                
                #Menu informativo
                    tabItem("Sobre",titlePanel("Sobre"),
                            br(),
                            div(includeMarkdown("About.Rmd"), 
                                align="justify")
                     
                    )
                
            )
        )
    )
)
server <- function(input, output) {
    
    #Tabela resultante do dataset importado pelo utilizador
    output$contents <- renderDataTable({
        if (input$submitbutton > 0) {
            req(input$file1)
            df <- read.csv(input$file1$datapath,header = input$header, sep = input$sep,
                           quote = input$quote)
            if(input$disp == "head"){
                return(head(df))
            }
            else{
                return(df)
            }
            isolate(datasetInput())
        }
    })
    
    #Dataset gerado apenas com as colunas selecionadas pelo utilizador
    output$Dependentes <- renderDataTable({
        req(input$file1)
        df <- read.csv(input$file1$datapath,header = input$header, sep = input$sep,
                               quote = input$quote)
        if (input$Dsubmitbutton > 0) {
                depen <- df[,c(input$DPred1, input$DPred2, input$DResult)]
                return(depen)
                isolate(datasetInput())
                }})
    
    #Data das vari?veis dependentes preparado para utilizar no package COMP2ROC
    data2 = reactive ({
            req (input$file1 )
            inFile2 = input$file1
            df2 = Comp2ROC:: read.file (inFile2$datapath, header.status = input$header, sep = input$sep,
                                            '.',input$DPred1,input$Direction1,input$DPred2, input$Direction2,
                                            input$DResult, T)
            return ( df2 ) })
    
    #Representa??es gr?ficas das curvas ROC emp?ricas e ?reas abaixo da curva e respectivo download    
    output$plotOut <- renderPlot({
            par(mfrow = c(1,2))
            roc.curves.boot_v2(data2(),1000, 0.05,name="Results",input$DPred1,input$DPred2,TRUE)
                })
    output$Download_Plot <-downloadHandler(
            filename=function(){
            paste("graph","png",sep=".")
            },content=function(file){
                png(file, height = 500, width = 1000)
                par(mfrow = c(1,2))
                roc.curves.boot_v2(data2(),1000, 0.05,name="Results",input$DPred1,input$DPred2,TRUE)
                dev.off() 
                }
            )
    #Objeto com os resultados estaisticos gerados.
    result <- reactive ({
            result = Comp2ROC :: roc.curves.boot(data2(),1000, 0.05,name="Results",input$DPred1,input$DPred2,TRUE)
            return(result)})
    
    #Reprenta??o gr?fica do histograma e Quantis.        
    output$plotOut2 <- renderPlot({
            result()
                
            })
    output$Download_Plot_2 <-downloadHandler(
            filename=function(){
                paste("hist","png",sep=".")
                },content=function(file){
                png(file, height = 500, width = 1000)
                Comp2ROC :: roc.curves.boot(data2(),1000, 0.05,name="Results",input$DPred1,input$DPred2,TRUE)
                dev.off() 
                }
            )
        # Representa??o dos resultados estatisticos das vari?veis selecionadas    
        output$OutputDependentes <- renderPrint({
                result <- result()
                if (all(c("Prediction 1", "Prediction 2", "Statistical Overall") %in% input$Resultados)) {
                    rocboot.summary(result,input$DPred1,input$DPred2)
                } else if ("Prediction 2" %in% input$Resultados) {
                    cat("\n")
                    cat("------------------------------------------------\n")
                    cat(input$DPred2, "\n")
                    cat("------------------------------------------------\n")
                    cat("Area:                               ", result$Area1, "\n")
                    cat("Standard Error:                     ", result$SE1, "\n")
                    cat("Area through Trapezoidal Method:    ", result$TrapArea1, "\n")
                    cat("CI Upper bound (Percentil Method):  ", result$ICUB1, "\n")
                    cat("CI Lower bound (Percentil Method):  ", result$ICLB1, "\n")
                } else if ("Prediction 1" %in% input$Resultados) {
                    cat("\n")
                    cat("------------------------------------------------\n")
                    cat(input$DPred1, "\n")
                    cat("------------------------------------------------\n")
                    cat("Area:                               ", result$Area1, "\n")
                    cat("Standard Error:                     ", result$SE1, "\n")
                    cat("Area through Trapezoidal Method:    ", result$TrapArea1, "\n")
                    cat("CI Upper bound (Percentil Method):  ", result$ICUB1, "\n")
                    cat("CI Lower bound (Percentil Method):  ", result$ICLB1, "\n")
                } else if ("Statistical Overall" %in% input$Resultados){
                    cat("\n")
                    cat("Correlation Coefficient between areas:  ", result$CorrCoef, "\n")
                    cat("\n")
                    cat("TEST OF DIFFERENCES\n")
                    cat("Z stats:  ", result$zstats,"\n")
                    cat("p-value:  ", result$pvalue1,"\n")
                    cat("\n")
                    cat("Sum of Global Areas Differences (TS):  ", result$diff, "\n")
                    cat("CI Upper bound (Percentil Method):  ", result$ICUBDiff, "\n")
                    cat("CI Lower bound (Percentil Method):  ", result$ICLBDiff, "\n")
                    cat("\n")
                    cat("Number of Crossings:  ", result$nCross, "\n")
                }
                
            })
            
            
        #Data set das com as vari?veis independentes selecionadas
            output$Independentes <- renderDataTable({
                req(input$file1)
                df <- read.csv(input$file1$datapath,header = input$header, sep = input$sep,
                               quote = input$quote)
                if (input$Isubmitbutton > 0) {
                    ind <- df[,c(input$IPred1, input$IResult1, input$IPred2, input$IResult2)]
                    return(ind)
                    isolate(datasetInput())
                }
                
            })
        #Data gerada e preparada para utilizar no package COMP2ROC    
            data3 = reactive ({
                req (input$file1 )
                inFile3 = input$file1
                df3 = Comp2ROC::read.file ( inFile3$datapath , header.status = input$header , sep = input$sep , ".", input$IPred1 ,
                                            input$Direction_1 , input$IPred2 , input$Direction_2 , input$IResult1 , FALSE , input$IResult2 )
                return ( df3 ) })
            
            #Representa??es gr?ficas das curvas ROC emp?ricas e ?reas abaixo da curva e respectivo download
            output$IplotOut <- renderPlot({
                par(mfrow = c(1,2))
                roc.curves.boot_v2(data3(),1000, 0.05,name="Results",input$IPred1,input$IPred2,FALSE)
                
                
            })
            output$IDownload_Plot <-downloadHandler(
                filename=function(){
                    paste("graph","png",sep=".")
                },content=function(file){
                    png(file, height = 500, width = 1000)
                    par(mfrow = c(1,2))
                    roc.curves.boot_v2(data3(),1000, 0.05,name="Results",input$IPred1,input$IPred2,FALSE)
                    dev.off() 
                }
            )
            #Objeto criado com os resultados estatisticos obtidos
            Ind_result <- reactive ({
                result = Comp2ROC :: roc.curves.boot(data3(),1000, 0.05,name="Results",input$IPred1,input$IPred2,FALSE)
                return(result)})
            
            #Representa??o gr?fica do histograma e Quantis
            output$IplotOut2 <- renderPlot({
                Ind_result()
                
            })
            output$IDownload_Plot_2 <-downloadHandler(
                filename=function(){
                    paste("hist","png",sep=".")
                },content=function(file){
                    png(file, height = 500, width = 1000)
                    Comp2ROC :: roc.curves.boot(data3(),1000, 0.05,name="Results",input$IPred1,input$IPred2,FALSE)
                    dev.off() 
                }
            )
            #Resultados estatisticos das vari?veis independentes selecionadas
            output$OutputIndependentes <- renderPrint({
                result <- Ind_result()
                if (all(c("Prediction 1", "Prediction 2", "Statistical Overall") %in% input$IResultados)) {
                    rocboot.summary(result,input$IPred1,input$IPred2)
                } else if ("Prediction 2" %in% input$IResultados) {
                    cat("\n")
                    cat("------------------------------------------------\n")
                    cat(input$IPred2, "\n")
                    cat("------------------------------------------------\n")
                    cat("Area:                               ", result$Area1, "\n")
                    cat("Standard Error:                     ", result$SE1, "\n")
                    cat("Area through Trapezoidal Method:    ", result$TrapArea1, "\n")
                    cat("CI Upper bound (Percentil Method):  ", result$ICUB1, "\n")
                    cat("CI Lower bound (Percentil Method):  ", result$ICLB1, "\n")
                } else if ("Prediction 1" %in% input$IResultados) {
                    cat("\n")
                    cat("------------------------------------------------\n")
                    cat(input$IPred1, "\n")
                    cat("------------------------------------------------\n")
                    cat("Area:                               ", result$Area1, "\n")
                    cat("Standard Error:                     ", result$SE1, "\n")
                    cat("Area through Trapezoidal Method:    ", result$TrapArea1, "\n")
                    cat("CI Upper bound (Percentil Method):  ", result$ICUB1, "\n")
                    cat("CI Lower bound (Percentil Method):  ", result$ICLB1, "\n")
                } else if ("Statistical Overall" %in% input$IResultados){
                    cat("\n")
                    cat("Correlation Coefficient between areas:  ", result$CorrCoef, "\n")
                    cat("\n")
                    cat("TEST OF DIFFERENCES\n")
                    cat("Z stats:  ", result$zstats,"\n")
                    cat("p-value:  ", result$pvalue1,"\n")
                    cat("\n")
                    cat("Sum of Global Areas Differences (TS):  ", result$diff, "\n")
                    cat("CI Upper bound (Percentil Method):  ", result$ICUBDiff, "\n")
                    cat("CI Lower bound (Percentil Method):  ", result$ICLBDiff, "\n")
                    cat("\n")
                    cat("Number of Crossings:  ", result$nCross, "\n")
                }
                
            })
    
}
shinyApp(ui = ui, server = server)