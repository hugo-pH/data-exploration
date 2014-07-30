library(shiny)
library(grid)
library(gridExtra)
library("ggplot2")
source("barplot_papaver_2.R")
library(agricolae)
library(car)
library(datasets)
library(agridat)
library(RColorBrewer)
options(shiny.maxRequestSize = 9*1024^2)
shinyServer(function(input, output) {
 
  
  #
  
  
  output$select.source<-renderUI({
  
  if (input$d.source == "public"){
  
    selectInput("dataset",
                label="Choose the dataset",
                choices = c("PlantGrowth", "InsectSprays", "germination", "wheat",
                            "potato", "ryegrass", "eden.potato", "darwin.maize", 
                            "adugna.sorghum", "apple.uniformity", "beall.webworms",
                            "blackman.wheat","bliss.borers")
    ) 
    
  }else{
    
  fileInput('file1', 'Choose file to upload',
            accept = c(
              'text/csv',
              'text/comma-separated-values',
              'text/tab-separated-values',
              'text/plain',
              '.csv',
              '.tsv'
            )
  )
  
  }
  })
  output$select.header<-renderUI({
    if (input$d.source == "public"){
      NULL
    }else{
      checkboxInput('header', 'Header', TRUE)    
    }
  })
  
  output$select.sep<-renderUI({
    if (input$d.source == "public"){
      NULL
    }else{
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ',')
    }
  })
  output$select.quote<-renderUI({
    if (input$d.source == "public"){
      NULL
    }else{
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"')
    }
  })
  
  bulk <- reactive({
    

    if (input$d.source == "public"){
      
      switch(input$dataset,
             
             "PlantGrowth" = PlantGrowth,
             "InsectSprays" = InsectSprays,
             "germination" = carlson.germination,
             "wheat" = crossa.wheat,
             "potato" = cornelius.maize,
             "ryegrass" = denis.ryegrass,
             "eden.potato" = eden.potato,
             "darwin.maize" = darwin.maize,
             "adugna.sorghum" = adugna.sorghum,
             "apple.uniformity" = batchelor.apple.uniformity,
             "beall.webworms" = beall.webworms,
             "blackman.wheat" = blackman.wheat,
             "bliss.borers" = bliss.borers)
    }else{
      
      
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
    
    }
  })
  
# bulk<-reactive({
#   switch(input$dataset,
#        
#          "PlantGrowth" = PlantGrowth,
#          "InsectSprays" = InsectSprays,
#          "germination" = carlson.germination,
#          "wheat" = crossa.wheat,
#          "potato" = cornelius.maize,
#          "ryegrass" = denis.ryegrass,
#          "eden.potato" = eden.potato,
#          "darwin.maize" = darwin.maize,
#          "adugna.sorghum" = adugna.sorghum,
#          "apple.uniformity" = batchelor.apple.uniformity,
#          "beall.webworms" = beall.webworms,
#          "blackman.wheat" = blackman.wheat,
#          "bliss.borers" = bliss.borers)
# })  
  
  output$select.res_var<-renderUI({
    d<-bulk()
    op<-colnames(d)
    selectInput("res_var",
                label="Choose the response variable",
                choices = op,
                selected = NULL
    ) 
  })
  
  output$select.gr_var<-renderUI({
    d<-bulk()
    op<-colnames(d)
    selectInput("gr_var",
                label="Choose the agroupation variable",
                choices = op,
                selected = NULL) 
  })
  
  
  
  output$select.gr2<-renderUI({
    if(input$check.gr2 == FALSE){
      return()
    }else{
      d<-bulk()
      op<-colnames(d)
      selectInput("gr_var_2",
                  label="Choose a second agrupation variable",
                  choices = op,
                  selected = NULL) 
    }
    
  })
  
  output$check.gr3<-renderUI({
    if(input$check.gr2 == FALSE){
      return()
    }else{
      checkboxInput("check.gr3",
                    label = "Select a third agrupation variable",
                    value = FALSE)
    }
  })
  output$select.gr3<-renderUI({
    if(input$check.gr2 == FALSE){
      return()
    }else{
      if(input$check.gr3 == FALSE){
        return()
      }else{
        d<-bulk()
        op<-colnames(d)
        selectInput("gr_var_3",
                    label="Choose a third agrupation variable",
                    choices = op,
                    selected = NULL) 
      }
    }
    
  })
  
  
  
  data<-reactive({
    b<-bulk()
    gr_var<-which(colnames(b)==input$gr_var)
    res_var<-which(colnames(b)==input$res_var)
    gr_var2<-which(colnames(b)==input$gr_var_2)
    gr_var3<-which(colnames(b)==input$gr_var_3)
    if(input$check.gr2 == FALSE){
      colnames(b)[gr_var]<-"group"
      colnames(b)[res_var]<-"value"
      b[["group"]]<-gsub(" ", "_", b$group, fixed=T)
    }else{
      
      if(input$check.gr3 == TRUE){
        colnames(b)[gr_var]<-"gr1"
        colnames(b)[res_var]<-"value"   
        colnames(b)[gr_var2]<-"gr2"
        colnames(b)[gr_var3]<-"gr3"
        b[["group"]]<-interaction(b$gr1, b$gr2, b$gr3)
#         b[["gr1"]]<-gsub(" ", "_", b$gr1, fixed=T)
#         b[["gr2"]]<-gsub(" ", "_", b$gr2, fixed=T)
#         b[["gr3"]]<-gsub(" ", "_", b$gr3, fixed=T)
#         b[["group"]]<-gsub(" ", "_", b$group, fixed=T)
      }else{colnames(b)[gr_var]<-"gr1"
            colnames(b)[res_var]<-"value"   
            colnames(b)[gr_var2]<-"gr2"
            b[["group"]]<-interaction(b$gr1, b$gr2)
#             b[["gr1"]]<-gsub(" ", "_", b$gr1, fixed=T)
#             b[["gr2"]]<-gsub(" ", "_", b$gr2, fixed=T)
#             b[["group"]]<-gsub(" ", "_", b$group, fixed=T)
      }
    }
    b
  })
  
  output$boxplot<-renderPlot({
    data<-data()
    
    
    #     if(input$gr2 == TRUE){
    #       gr_var2<-which(colnames(data)==input$gr_var_2)
    #       colnames(data)[gr_var2]<-"group_2"
    #     }
    #     
    
    validate(#Avoid red error message to be shown when the user changes the attribute. Meanwhile, print the message "waiting for your selection"
      need(nrow(data)>1, "Waiting for your selection")
    )
    n.colors<-length(levels(as.factor(data$group)))
    # Brewer palette Paired 12
    pal12 = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", 
              "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A",
              "#FFFF99", "#B15928")
    palet<-colorRampPalette(pal12)(n.colors)
    p<-ggplot(data, aes(factor(group), value, fill=group)) 
    
    p<- p + geom_boxplot()  +
      xlab("agroupation variable") +
      ylab("response variable")  +
      scale_fill_manual(values = palet) +
      theme(axis.text.x=element_text(face="bold", angle=70, hjust=1, size=12)) 
    
    print(p)
  })
  
  output$box.facet<-renderPlot({
    data<-data()
    if(input$check.gr2 == FALSE){
      return()
    }else{
      
      n.colors<-length(levels(as.factor(data$gr1)))
      # Brewer palette Paired 12
      pal12 = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", 
                "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A",
                "#FFFF99", "#B15928")
      palet<-colorRampPalette(pal12)(n.colors)
      validate(#Avoid red error message to be shown when the user changes the attribute. Meanwhile, print the message "waiting for your selection"
        need(nrow(data)>1, "Waiting for your selection")
      )
      
      p<-ggplot(data, aes(factor(gr1), value, fill=gr1)) 
      if(input$check.gr2 == TRUE){
        p <-p + facet_wrap(~gr2)
        if(input$check.gr3 == TRUE){
          p <-p + facet_wrap(~gr2*gr3)
        }
      }
      p<- p + geom_boxplot()  +
        xlab("agroupation variable") +
        ylab("response variable")  +
        scale_fill_manual(values = palet) +
        theme(axis.text.x=element_text(face="bold", angle=70, hjust=1, size=12)) 
      
      print(p)
    }
  })
  
  
  output$histo<-renderPlot({
    data<-data()
    
    
    range.value<- (range(data$value)[2] - range(data$value)[1])/50 
    p<-ggplot(data, aes(x = value)) +
      geom_histogram(aes(x=value, y=..density.., fill = ..density..), binwidth=range.value) + #use density instead of counts, nicer view
      geom_density() +
      scale_fill_gradient("Count", low = "green", high = "red")
    print(p)
  })
  
  #Normality tab, print the result of an normality test       
  output$norm<-renderPrint({
    data<-data()
    validate(#Avoid red error message to be shown when the user changes the attribute. Meanwhile, print the message "waiting for your selection"
      need(nrow(data)>0, "Waiting for your selection")
    )
    shap.res<-shapiro.test(data$value)
    shap.res  
    #   }
  })  
  
  output$qqplot<-renderPlot({
    data<-data()
    qqnorm(data$value)
  })
  
  output$homo.plot<-renderPlot({
    data<-data()
    n.colors<-length(levels(as.factor(data$group)))
    # Brewer palette Paired 12
    pal12 = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", 
              "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A",
              "#FFFF99", "#B15928")
    palet<-colorRampPalette(pal12)(n.colors)
    
    b1<- ggplot(data, aes(group, value)) +
      geom_boxplot(aes(fill = group)) +
      scale_fill_manual(values = palet) +
      theme(legend.position = "none") +
      theme(legend.position = "none", axis.text.x=element_text(face="bold", angle=60, hjust=1, size=12)) 
    
    b2<-ggplot(data, aes(group, value)) + 
      geom_jitter(alpha=I(1/2), aes(color = group)) +
      scale_color_manual(values = palet) +
      theme(legend.position = "none") +
      theme(legend.position = "none", axis.text.x=element_text(face="bold", angle=60, hjust=1, size=12))   
    b3<-ggplot(data, aes(x=value)) + 
      scale_fill_manual(values = palet) +
      scale_color_manual(values = palet) +
      stat_density(aes(ymax = ..density..,  ymin = -..density..,
                       fill = group, color = group),
                   geom = "ribbon", position = "identity") +
      facet_grid(. ~ group) +
      coord_flip() +
      theme(legend.position = "none", axis.text.x=element_text(face="bold", angle=60, hjust=1, size=12)) 
    
    g.plot<-grid.arrange(b1, b2, b3, nrow=1)
    
    
    
  })
  
  
  
  #Homocedasticity tab, , print the result of an levene Test for homogenity of variances.
  output$homo<-renderPrint({
    
    data<-data()
    validate(#Avoid red error message to be shown when the user changes the attribute. Meanwhile, print the message "waiting for your selection"
      need(nrow(data)>0, "Waiting for your selection")
    )
    if(input$check.gr2 == TRUE){
      lev.res<-leveneTest(value ~ gr1*gr2, data=data)
      if(input$check.gr3 == TRUE){
        lev.res<-leveneTest(value ~ gr1*gr2*gr3, data=data)
      }
    }else{
      lev.res<-leveneTest(value ~ group, data=data)  
    }
    
    lev.res
    
  })
  
  output$aov<-renderPrint({
    #   if (input$go  ==0){ # if the 'Run' button is not clicked, return nothing.
    #     
    #     return(NULL)
    #   }else{ 
    data<-data()
    validate(#Avoid red error message to be shown when the user changes the attribute. Meanwhile, print the message "waiting for your selection"
      need(nrow(data)>0, "Waiting for your selection")
    )
    
    if (input$stat  == "par"){ #Perform a test, ANOVA or Kruskal depending on the obtained levene.Test p.value 
      #If the p.value is > 0.05 an ANOVA will be performed, else a kruskal.test will be performed. 
      
      if(input$check.gr2 == TRUE){
        aov.res<-aov(value ~ gr1*gr2, data)
        if(input$check.gr3 == TRUE){
          aov.res<-aov(value ~ gr1*gr2*gr3, data)
        }
      }else{
        aov.res<-aov(value ~ group, data)
      }
      summary(aov.res)
    }else{
      
      if(input$check.gr2 == TRUE){
        kw.res<-"There is any non-parametric test available for two or more levels"
        if(input$check.gr3 == TRUE){
          kw.res<-"There is any non-parametric test available for two or more levels"
        }
      }else{
        kw.res<-kruskal.test(value~as.factor(group), data = data)  
      }    
      
      kw.res
    }
    
  })
  #Groups tab. Print the result of an statistical test to search for differences among groups (Tukey test or Kruskall)
  output$groups<-renderPrint({
    #   if (input$go  ==0){ # if the 'Run' button is not clicked, return nothing.
    #     
    #     return(NULL)
    #   }else{ 
    #Perform a test, ANOVA or Kruskal depending on the obtained levene.Test p.value 
    data<-data()
    validate(#Avoid red error message to be shown when the user changes the attribute. Meanwhile, print the message "waiting for your selection"
      need(nrow(data)>0, "Waiting for your selection")
    )
    
    
    if (input$stat  == "par"){ #Perform a test, ANOVA or Kruskal depending on the obtained levene.Test p.value 
      #If the p.value is > 0.05 an ANOVA will be performed, else a kruskal.test will be performed. 
      
      #For calculate the groups with Tukey test, performs an ANOVA over the 'group' column. This column cotains or the first group, or the interaction among them if there are more than one. 
      
      aov.res<-aov(value ~ group, data)
      
      aov.sum<-summary(aov.res)
      
      aov.pval<-aov.sum[[1]]$'Pr(>F)'[[1]]#here is where the p.value is obtained in the summary(aov) object
      if(aov.pval < 0.05){ # If the ANOVA returns a p.value < 0.05 a Tukey test will be performed
        tuk.res<-HSD.test(aov.res, "group", group = T)  
        tuk.res
      }else{ # Else, print this:
        print("Anova was no significant")
      }
    }else{
      
      if(input$check.gr2 == TRUE){
        kw.res<-"There is any non-parametric test available for two or more levels"
        if(input$check.gr3 == TRUE){
          kw.res<-"There is any non-parametric test available for two or more levels"
        }
      }else{
        kw.res<-kruskal.test(value~as.factor(group), data = data)  
        
        
        #       kw.res
        if(kw.res[["p.value"]] < 0.05){ # If the p.value obtained is < 0.05 a kruskal test from the agricolae package will be performed.
          group.res<-kruskal(y=data$value, trt=data$group, group=T,  p.adj="bonferroni")
          group.res
        }else{# Else, print this:
          print("The kruskal test was not significant")
        }
      }
      
    }
    
  })
  output$barplot_stat<-renderPlot({
    data<-data()
    p<-printplot(data, c.gr2=input$check.gr2, c.gr3=input$check.gr3, stat=input$stat)
    print(p) 
    
  })
  
  
  output$data<-renderTable({
    data<-data()
    data
  })
})