printplot<-function(data, c.gr2, c.gr3, stat){
  
  data$value<-as.numeric(data$value)
  dataplot<-aggregate(data$value, by= list(data$group),
                      mean, na.rm= T, simplify = T)
  sd<-aggregate(data$value, by= list(data$group),
                sd, na.rm= T, simplify = T)
  dataplot$sd<-sd$x
  
  colnames(dataplot)<-c("group", "mean", "sd")
  
  #Define basic plots
  #Plot for values with significant differences. Define colours
  plot_sig<- ggplot(data=dataplot, aes(x=group, y=mean)) 
  
  #Plot for values without significant differences,use only one colour
  plot_no<- ggplot(data=dataplot, aes(x=group, y=mean)) +
    geom_bar(stat="identity", fill="#009E73")
  
  
  
  if (stat  == "par"){ #Perform a test, ANOVA or Kruskal depending on the obtained levene.Test p.value 
    #If the p.value is > 0.05 an ANOVA will be performed, else a kruskal.test will be performed. 
    
    #For calculate the groups with Tukey test, performs an ANOVA over the 'group' column. This column cotains or the first group, or the interaction among them if there are more than one. 
    
    
    aov.res<-aov(value ~ group, data)
    
    aov.sum<-summary(aov.res)
    
    aov.pval<-aov.sum[[1]]$'Pr(>F)'[[1]]#here is where the p.value is obtained in the summary(aov) object
    if(aov.pval < 0.05){ # If the ANOVA returns a p.value < 0.05 a Tukey test will be performed
      group.res<-HSD.test(aov.res, "group", group = T)
      #Remove blank spaces at the end of the group name string.
      group.res$groups$trt<-gsub(" ", "", group.res$groups$trt, fixed=T)
      #Merge the dataplot df with the group.res$groups df obtained in the HSD.test. In this way, the letters for each group are appended to the original dataplot df as a new column named 'M'. The merge is possible beacause of the 'groups$trt' column, which contains the group names.
      dataplot<-merge(dataplot, group.res$groups, by.x = "group", by.y="trt")
      gs.pal <- colorRampPalette(c("red","blue"),bias=.1,space="rgb")
      n.colors<-length(levels(as.factor(dataplot$M)))
      # Brewer palette Paired 12
      pal12 = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", 
                "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A",
                "#FFFF99", "#B15928")
      palet<-colorRampPalette(pal12)(n.colors)
      #When ANOVA p.value < 0.05, the bar colours are mapped to the M column (groups from HSD.test), therefore different colours means significant differences.
      p <- plot_sig + geom_bar(data=dataplot, aes(fill=M), stat="identity") +
        scale_fill_manual(values = palet) +
        geom_text(data=dataplot, aes(x=group, y=(1.1 * mean + sd), label = M))
      
      
    }else{ #If the ANOVA pvalue > 0.05, use the plot_no, the same colour for all bars
      p <- plot_no
    }
    
    
  }else{
    
    if(c.gr2 == TRUE){
      p <- plot_no
      if(c.gr3 == TRUE){
        p <- plot_no
      }
    }else{
      kw.res<-kruskal.test(value~as.factor(group), data = data)  
      
      if(kw.res[["p.value"]] < 0.05){ # If the p.value obtained is < 0.05 a kruskal test from the agricolae package will be performed.
        group.res<-kruskal(y=data$value, trt=data$group, group=T, p.adj="bonferroni")
        #Remove blank spaces at the end of the group name string.  
        group.res$groups$trt<-gsub(" ", "", group.res$groups$trt, fixed=T)
        #Merge the dataplot df with the group.res$groups df obtained in the kruskal test. In this way, the letters for each group are appended to the original dataplot df as a new column named 'M'. The merge is possible beacause of the 'groups$trt' column, which contains the group names.
        dataplot<-merge(dataplot, group.res$groups, by.x = "group", by.y="trt")
        n.colors<-length(levels(as.factor(dataplot$M)))
        # Brewer palette Paired 12
        pal12 = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", 
                  "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A",
                  "#FFFF99", "#B15928")
        palet<-colorRampPalette(pal12)(n.colors)
        #When kruskal.test p.value < 0.05, the bar colours are mapped to the M column (groups from 'kruskal' agricolae function), therefore different colours means significant differences.
        p <- plot_sig + geom_bar(data=dataplot, aes(fill=M), stat="identity") +
          scale_fill_manual(values = palet) +
          geom_text(data=dataplot, aes(x=group, y=(1.1 * mean + sd), label = M)) 
      }else{ #If the kruskal.test pvalue > 0.05, use the plot_no, the same colour for all bars
        p <- plot_no
      }
    }
    
  }
  p <- p  +   geom_errorbar(aes(ymin= mean - sd, ymax=mean + sd),
                            side=.3,
                            width=.2,                    # Width of the error bars
                            position=position_dodge(.9)) +
    theme(axis.text.x=element_text(face="bold", angle=60, hjust=1,  size=12)) # Turn 60º the stock names within the x axis. 
  
  
  print(p)
}