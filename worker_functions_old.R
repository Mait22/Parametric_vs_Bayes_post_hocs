library(ggplot2)
library(car)
library(agricolae)
library(BayesFactor)
library(cowplot)
library(ggthemes)
library(stringr)

#Helper functions
s.err <- function(x) sd(x,na.rm = TRUE)/sqrt(sum(!is.na(x)))

RobustMax <- function(x) {if (sum(!is.na(x))>0) max(x,na.rm = TRUE) else NA}

RobustMin <- function(x) {if (sum(!is.na(x))>0) min(x,na.rm = TRUE) else NA}


#Reading test data
TEST_FILE <- read.csv("TEST_FILE.csv",stringsAsFactors = FALSE)[1:63,]



setClass("interaction",
         representation(df = "data.frame",
                        f1.label = "character",
                        f2.label = "character",
                        value.label = "character",
                        f.levels1 = "character",
                        f.levels2 = "character",
                        f.levels.int = "character",
                        f.levels1.null = "logical",
                        f.levels2.null = "logical",
                        f1.averages.se = "data.frame",
                        f2.averages.se = "data.frame",
                        inter.averages.se = "data.frame"
         ))




interaction.maker <- function(df,factor1=NULL,factor2=NULL,value){
  
  
  return.results <- new("interaction")
  df[,"interaction"] <- rep(NA, times = dim(df)[1])

  
  
  
  
  if(is.null(factor1) == TRUE & is.null(factor2) == TRUE){
    return.results@f.levels1.null <- TRUE
    return.results@f.levels2.null <- TRUE
    
    return(return.results)
    
    
  }
  
  if(is.null(factor1) == FALSE & is.null(factor2) == TRUE){
    
    
    return.results@f1.label <- factor1
    return.results@value.label <- value
    
    df[,factor1] <- as.character(df[,factor1])
    return.results@df <- df
    
    df[,factor1] <- as.factor(df[,factor1])
    
    return.results@f.levels1 <- levels(df[,factor1])
    return.results@f.levels1.null <- FALSE
    return.results@f.levels2.null <- TRUE
    
    
    #Averages and SE computations
    results <- as.data.frame(matrix(rep(NA, times = length(return.results@f.levels1)*3),
                                    nrow = length(return.results@f.levels1),ncol = 3))
    
    results[,1] <- return.results@f.levels1
      
    names(results) <- c("Factor", "Mean", "SE")
    for (i in results[,"Factor"]){
      subset <- df[df[,factor1] == i,]
      results[results[,"Factor"] == i,c("Mean", "SE")] <- c(mean(subset[,value],na.rm = TRUE), s.err(subset[,value]))
    }

    return.results@f1.averages.se <- results
    
    #Returning results
    return(return.results)
  }
  
  if(is.null(factor1) == TRUE & is.null(factor2) == FALSE){
    
    return.results@f2.label <- factor2
    return.results@value.label <- value
    
    df[,factor2] <- as.character(df[,factor2])
    return.results@df <- df
    
    df[,factor2] <- as.factor(df[,factor2])
    return.results@f.levels2 <- levels(df[,factor2])
    return.results@f.levels1.null <- TRUE
    return.results@f.levels2.null <- FALSE
    
    
    #Averages and SE computations
    results <- as.data.frame(matrix(rep(NA, times = length(return.results@f.levels2)*3),
                                    nrow = length(return.results@f.levels2),ncol = 3))
    
    results[,1] <- return.results@f.levels2
    
    names(results) <- c("Factor", "Mean", "SE")
    for (i in results[,"Factor"]){
      subset <- df[df[,factor2] == i,]
      results[results[,"Factor"] == i,c("Mean", "SE")] <- c(mean(subset[,value],na.rm = TRUE), s.err(subset[,value]))
    }
    
    return.results@f2.averages.se <- results

    
    return(return.results)
    
  }
  
  if(is.null(factor1) == FALSE & is.null(factor2) == FALSE){
    
    return.results@f.levels1.null <- FALSE
    return.results@f.levels2.null <- FALSE
    
    return.results@f1.label <- factor1
    return.results@f2.label <- factor2
    return.results@value.label <- value
    
    #Making interaction term
    factor1_vec <- unique(df[,factor1])
    factor2_vec <- unique(df[,factor2])
    
    combined_vec <- c()
    n = 0
    
    for (i in c(1:length(factor1_vec))){
      for (m in c(1:length(factor2_vec))){
        
        combined_vec[n+1] <- paste(factor1_vec[i],"###",factor2_vec[m], sep = "")
        n <- n+1
        
        
      }
      
    }
    
    
    #Adding interaction term to df
    
    for(i in c(1:dim(df)[1])){
      
      df[i,"interaction"] <- paste(df[i,factor1],"###",df[i,factor2], sep = "")
    
    }
    
    
    df[,"interaction"] <- factor(df[,"interaction"], levels = combined_vec)
    df[,"interaction"] <- as.character(df[,"interaction"])
    return.results@df <- df
    
    
    
    
    df[,factor1] <- as.factor(df[,factor1])
    df[,factor2] <- as.factor(df[,factor2])
    return.results@f.levels1 <- levels(df[,factor1]) 
    return.results@f.levels2 <- levels(df[,factor2]) 
    return.results@f.levels.int <- combined_vec
    
    
    
    #Computing the averages
    #First factor
    results <- as.data.frame(matrix(rep(NA, times = length(return.results@f.levels1)*3),
                                    nrow = length(return.results@f.levels1),ncol = 3))
    
    results[,1] <- return.results@f.levels1
    
    names(results) <- c("Factor", "Mean", "SE")
    for (i in results[,"Factor"]){
      subset <- df[df[,factor1] == i,]
      results[results[,"Factor"] == i,c("Mean", "SE")] <- c(mean(subset[,value],na.rm = TRUE), s.err(subset[,value]))
    }
    
    return.results@f1.averages.se <- results
    
    #Seccond factor
    results <- as.data.frame(matrix(rep(NA, times = length(return.results@f.levels2)*3),
                                    nrow = length(return.results@f.levels2),ncol = 3))
    
    results[,1] <- return.results@f.levels2
    
    names(results) <- c("Factor", "Mean", "SE")
    for (i in results[,"Factor"]){
      subset <- df[df[,factor2] == i,]
      results[results[,"Factor"] == i,c("Mean", "SE")] <- c(mean(subset[,value],na.rm = TRUE), s.err(subset[,value]))
    }
    
    return.results@f2.averages.se <- results
    
    
    #Interaction
    results <- as.data.frame(matrix(rep(NA, times = length(return.results@f.levels.int)*3),
                                    nrow = length(return.results@f.levels.int),ncol = 3))
    
    results[,1] <- return.results@f.levels.int
    
    names(results) <- c("Factor", "Mean", "SE")
    for (i in results[,"Factor"]){
      subset <- df[df[,"interaction"] == i,]
      results[results[,"Factor"] == i,c("Mean", "SE")] <- c(mean(subset[,value],na.rm = TRUE), s.err(subset[,value]))
    }
    
    return.results@inter.averages.se <- results
    
    return(return.results)
  }
    
}











setClass("post.hoc",
         representation(df = "data.frame",
                        f1.comps = "data.frame",
                        f2.comps = "data.frame",
                        interaction.comps = "data.frame",
                        f.levels1.null = "logical",
                        f.levels2.null = "logical",
                        post.hoc.type = "character"
                        

         ))



post_hoc <- function(interaction.object,
                     post.hoc.type = "LSD.test"
                     
){
  
  return.results <- new("post.hoc")
  return.results@df <- interaction.object@df
  return.results@post.hoc.type <- post.hoc.type
  return.results@f.levels1.null <- interaction.object@f.levels1.null
  return.results@f.levels2.null <- interaction.object@f.levels2.null
  
  if(interaction.object@f.levels1.null == FALSE & interaction.object@f.levels2.null == FALSE){
    
    factor1 <- interaction.object@df[,interaction.object@f1.label]
    factor2 <- interaction.object@df[,interaction.object@f2.label]
    inter <- interaction.object@df[,"interaction"]
    value <- interaction.object@df[,interaction.object@value.label]
    
    
    if(post.hoc.type == "LSD.test"){
      comp.f1 <- LSD.test((aov(lm(value ~ factor1, data=interaction.object@df))),
                       "factor1",console=FALSE,group = F)$comparison[2]
      
      comp.f2 <- LSD.test((aov(lm(value ~ factor2, data=interaction.object@df))),
                       "factor2",console=FALSE,group = F)$comparison[2]
      
      comp.inter <- LSD.test((aov(lm(value ~ inter, data=interaction.object@df))),
                       "inter",console=FALSE,group = F)$comparison[2]
    }
    
    if(post.hoc.type == "HSD.test"){
      comp.f1 <- HSD.test((aov(lm(value ~ factor1, data=interaction.object@df))),
                          "factor1",console=FALSE,group = F)$comparison[2]
      
      comp.f2 <- HSD.test((aov(lm(value ~ factor2, data=interaction.object@df))),
                          "factor2",console=FALSE,group = F)$comparison[2]
      
      comp.inter <- HSD.test((aov(lm(value ~ inter, data=interaction.object@df))),
                             "inter",console=FALSE,group = F)$comparison[2]
    }
    
    if(post.hoc.type == "duncan.test"){
      comp.f1 <- duncan.test((aov(lm(value ~ factor1, data=interaction.object@df))),
                          "factor1",console=FALSE,group = F)$comparison[2]
      
      comp.f2 <- duncan.test((aov(lm(value ~ factor2, data=interaction.object@df))),
                          "factor2",console=FALSE,group = F)$comparison[2]
      
      comp.inter <- duncan.test((aov(lm(value ~ inter, data=interaction.object@df))),
                             "inter",console=FALSE,group = F)$comparison[2]
    }
    
    
    comp.f1[,2] <- comp.f1[,1]
    comp.f1[,1] <- row.names(comp.f1)
    names(comp.f1) <- c("comp", "pvalue")
    comp.f1$group1 <- unlist(lapply(strsplit(comp.f1[,1],split = " - "),"[",1))
    comp.f1$group2 <- unlist(lapply(strsplit(comp.f1[,1],split = " - "),"[",2))
    rownames(comp.f1) <- c(1:dim(comp.f1)[1])
    
    
    comp.f2[,2] <- comp.f2[,1]
    comp.f2[,1] <- row.names(comp.f2)
    names(comp.f2) <- c("comp", "pvalue")
    comp.f2$group1 <- unlist(lapply(strsplit(comp.f2[,1],split = " - "),"[",1))
    comp.f2$group2 <- unlist(lapply(strsplit(comp.f2[,1],split = " - "),"[",2))
    rownames(comp.f2) <- c(1:dim(comp.f2)[1])
    
    
    comp.inter[,2] <- comp.inter[,1]
    comp.inter[,1] <- row.names(comp.inter)
    names(comp.inter) <- c("comp", "pvalue")
    comp.inter$group1 <- unlist(lapply(strsplit(comp.inter[,1],split = " - "),"[",1))
    comp.inter$group2 <- unlist(lapply(strsplit(comp.inter[,1],split = " - "),"[",2))
    rownames(comp.inter) <- c(1:dim(comp.inter)[1])
    
    
    #BF computations F1
    for(i in c(1:dim(comp.f1)[1])){
      crit <- interaction.object@df[,interaction.object@f1.label] == comp.f1[i,"group1"] | interaction.object@df[,interaction.object@f1.label] == comp.f1[i,"group2"]
      subset <- interaction.object@df[crit,]
      comp.formula <- paste(interaction.object@value.label," ~ ", interaction.object@f1.label,sep = "")
      BF.res = ttestBF(formula = as.formula(comp.formula), data = subset)
      comp.f1[i,"BF"] <- abs(BF.res@bayesFactor[,"bf"])
    }
    
    #BF computations F2
    for(i in c(1:dim(comp.f2)[1])){
      crit <- interaction.object@df[,interaction.object@f2.label] == comp.f2[i,"group1"] | interaction.object@df[,interaction.object@f2.label] == comp.f2[i,"group2"]
      subset <- interaction.object@df[crit,]
      comp.formula <- paste(interaction.object@value.label," ~ ", interaction.object@f2.label,sep = "")
      BF.res = ttestBF(formula = as.formula(comp.formula), data = subset)
      comp.f2[i,"BF"] <- abs(BF.res@bayesFactor[,"bf"])
    }
    
    
    #BF computations inter
    for(i in c(1:dim(comp.inter)[1])){
      crit <- interaction.object@df[,"interaction"] == comp.inter[i,"group1"] | interaction.object@df[,"interaction"] == comp.inter[i,"group2"]
      subset <- interaction.object@df[crit,]
      comp.formula <- paste(interaction.object@value.label," ~ ", "interaction",sep = "")
      BF.res = ttestBF(formula = as.formula(comp.formula), data = subset)
      comp.inter[i,"BF"] <- abs(BF.res@bayesFactor[,"bf"])
      
      

    }
    
    
    return.results@f1.comps <- comp.f1
    return.results@f2.comps <- comp.f2
    return.results@interaction.comps <- comp.inter
    
    return(return.results)
    
  }
  
  
  if(interaction.object@f.levels1.null == FALSE & interaction.object@f.levels2.null == TRUE){
    
    factor1 <- interaction.object@df[,interaction.object@f1.label]
    value <- interaction.object@df[,interaction.object@value.label]
    
    if(post.hoc.type == "LSD.test"){
      comp.f1 <- LSD.test((aov(lm(value ~ factor1, data=interaction.object@df))),
                          "factor1",console=FALSE,group = F)$comparison[2]
    }
    
    if(post.hoc.type == "HSD.test"){
      comp.f1 <- HSD.test((aov(lm(value ~ factor1, data=interaction.object@df))),
                          "factor1",console=FALSE,group = F)$comparison[2]
    }
    
    if(post.hoc.type == "duncan.test"){
      comp.f1 <- duncan.test((aov(lm(value ~ factor1, data=interaction.object@df))),
                          "factor1",console=FALSE,group = F)$comparison[2]
    }
    
    comp.f1[,2] <- comp.f1[,1]
    comp.f1[,1] <- row.names(comp.f1)
    names(comp.f1) <- c("comp", "pvalue")
    comp.f1$group1 <- unlist(lapply(strsplit(comp.f1[,1],split = " - "),"[",1))
    comp.f1$group2 <- unlist(lapply(strsplit(comp.f1[,1],split = " - "),"[",2))
    rownames(comp.f1) <- c(1:dim(comp.f1)[1])
    
    #F1 BF computations
    for(i in c(1:dim(comp.f1)[1])){
      crit <- interaction.object@df[,interaction.object@f1.label] == comp.f1[i,"group1"] | interaction.object@df[,interaction.object@f1.label] == comp.f1[i,"group2"]
      subset <- interaction.object@df[crit,]
      comp.formula <- paste(interaction.object@value.label," ~ ", interaction.object@f1.label,sep = "")
      BF.res = ttestBF(formula = as.formula(comp.formula), data = subset)
      comp.f1[i,"BF"] <- abs(BF.res@bayesFactor[,"bf"])
    }
    
    return.results@f1.comps <- comp.f1

    return(return.results)
    
  }
  
  
  if(interaction.object@f.levels1.null == TRUE & interaction.object@f.levels2.null == FALSE){
    
    factor2 <- interaction.object@df[,interaction.object@f2.label]
    value <- interaction.object@df[,interaction.object@value.label]
    
    if(post.hoc.type == "LSD.test"){
      comp.f2 <- LSD.test((aov(lm(value ~ factor2, data=interaction.object@df))),
                          "factor2",console=FALSE,group = F)$comparison[2]
    }
    
    if(post.hoc.type == "HSD.test"){
      comp.f2 <- HSD.test((aov(lm(value ~ factor2, data=interaction.object@df))),
                          "factor2",console=FALSE,group = F)$comparison[2]
    }
    
    if(post.hoc.type == "duncan.test"){
      comp.f2 <- duncan.test((aov(lm(value ~ factor2, data=interaction.object@df))),
                          "factor2",console=FALSE,group = F)$comparison[2]
    }
    
    comp.f2[,2] <- comp.f2[,1]
    comp.f2[,1] <- row.names(comp.f2)
    names(comp.f2) <- c("comp", "pvalue")
    comp.f2$group1 <- unlist(lapply(strsplit(comp.f2[,1],split = " - "),"[",1))
    comp.f2$group2 <- unlist(lapply(strsplit(comp.f2[,1],split = " - "),"[",2))
    rownames(comp.f2) <- c(1:dim(comp.f2)[1])
    
    #F2 BF computations
    for(i in c(1:dim(comp.f2)[1])){
      crit <- interaction.object@df[,interaction.object@f2.label] == comp.f2[i,"group1"] | interaction.object@df[,interaction.object@f2.label] == comp.f2[i,"group2"]
      subset <- interaction.object@df[crit,]
      comp.formula <- paste(interaction.object@value.label," ~ ", interaction.object@f2.label,sep = "")
      BF.res = ttestBF(formula = as.formula(comp.formula), data = subset)
      comp.f2[i,"BF"] <- abs(BF.res@bayesFactor[,"bf"])
    }
    
    return.results@f2.comps <- comp.f2
    
    return(return.results)
    
  }
  
}


setOldClass(c("gg"))

setClass("post.hoc.plot",
         representation(plot.data.f1 = "data.frame",
                        plot.data.f2 = "data.frame",
                        plot.data.inter = "data.frame",
                        plot.comp.f1 = "data.frame",
                        plot.comp.f2 = "data.frame",
                        plot.comp.inter = "data.frame",
                        plot.f1 = "gg",
                        plot.f2 = "gg",
                        plot.inter = "gg"
         ))



post_hoc_plot <- function(post.hoc.object,
                          interaction.object,
                          plot.title.f1.par = "Parametric Comparisons for F1",
                          plot.title.f2.par ="Parametric Comparisons for F2",
                          plot.title.inter.par = "Parametric Comparisons for Interactions",
                          plot.title.f1.bayes ="Bayes Factor for F1",
                          plot.title.f2.bayes ="Bayes Factor for F2",
                          plot.title.inter.bayes = "Bayes Factor for Interactions",
                          x.axis.labs = c("Factor no 1", "Factor no 2", "Interactions"),
                          y.axis.lab = "Mean +/- 1 SE",
                          BF.criteria = 5,
                          p.val.criteria = 0.1,
                          y.axis.length.factor = 1.25,
                          str.width = 15
                          ){
  
  return_results <- new("post.hoc.plot")
  
  #Factor I
  if(post.hoc.object@f.levels1.null == FALSE){
    w.data.f1.comps <- post.hoc.object@f1.comps
    w.data.f1.vals <- interaction.object@f1.averages.se
    factor.levels <- interaction.object@f.levels1
    
    factor.levels <- str_wrap(factor.levels, width = str.width)
    w.data.f1.vals[,"Factor"] <- str_wrap(w.data.f1.vals[,"Factor"], width = str.width)
    w.data.f1.comps[,"group1"] <- str_wrap(w.data.f1.comps[,"group1"], width = str.width)
    w.data.f1.comps[,"group2"] <- str_wrap(w.data.f1.comps[,"group2"], width = str.width)
    
    w.data.f1.vals[,"Factor"] <- factor(w.data.f1.vals[,"Factor"],levels = factor.levels)
    w.data.f1.comps[,"group1"] <- factor(w.data.f1.comps[,"group1"], levels = factor.levels)
    w.data.f1.comps[,"group2"] <- factor(w.data.f1.comps[,"group2"], levels = factor.levels)
    
    
    return_results@plot.data.f1 <- w.data.f1.vals
    return_results@plot.comp.f1 <- w.data.f1.comps
    
    y_axis_max <- (RobustMax(w.data.f1.vals[,"Mean"]) + RobustMax(w.data.f1.vals[,"SE"]))
    y_axis_min <- (RobustMin(w.data.f1.vals[,"Mean"]) - RobustMin(w.data.f1.vals[,"SE"]))
    
    f1.plot <- ggplot(data = w.data.f1.vals, aes(x= Factor, y= Mean))
    f1.plot <- f1.plot + coord_cartesian(ylim = c(y_axis_min/y.axis.length.factor,y_axis_max*y.axis.length.factor))  
    f1.plot <- f1.plot + geom_bar(position=position_dodge(), stat="identity", fill = "#56B4E9") 
    f1.plot <- f1.plot + theme_fivethirtyeight() + scale_colour_economist() 
    f1.plot <- f1.plot + geom_errorbar(aes(ymin=Mean-1*SE, ymax=Mean+1*SE),
                                           width=.2,                 
                                           position=position_dodge(.9), size = 1)
    
    
    #Annotations based on p-vals
    base.length <- 1.05
    f1.plot.par <- f1.plot
    for (i in c(1:dim(w.data.f1.comps)[1])){
      
      if(w.data.f1.comps[i,"pvalue"] < p.val.criteria){
        
        f1.plot.par <- f1.plot.par + annotate("segment", x = w.data.f1.comps[i,"group1"], 
                                      xend = w.data.f1.comps[i,"group2"] , 
                                      y = y_axis_max*base.length, 
                                      yend = y_axis_max*base.length, colour = "red", size = 1)
      }
      base.length <- base.length + 0.025
    }
    
    
    #Annotations based on BF-s
    base.length <- 1.05
    f1.plot.BF <- f1.plot
    for (i in c(1:dim(w.data.f1.comps)[1])){
      
      if(w.data.f1.comps[i,"BF"] > BF.criteria){
        
        f1.plot.BF <- f1.plot.BF + annotate("segment", x = w.data.f1.comps[i,"group1"], 
                                      xend = w.data.f1.comps[i,"group2"] , 
                                      y = y_axis_max*base.length, 
                                      yend = y_axis_max*base.length, colour = "red", size = 1)
      }
      base.length <- base.length + 0.025
    }
    
    f1.plot.BF <- f1.plot.BF + theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
    
    
    #F1 return plot  
    f1.plot.par <- f1.plot.par + ggtitle(plot.title.f1.par) +  theme(plot.title = element_text(size = 10, face = "bold"))
    f1.plot.BF <- f1.plot.BF + ggtitle(plot.title.f1.bayes) +  theme(plot.title = element_text(size = 10, face = "bold"))
    
    f1.plot <- ggdraw() +
    draw_plot(f1.plot.par, x = 0,y = 0,height = 1,width = 0.55) +  
    draw_plot(f1.plot.BF, x = 0.55,y = 0,height = 1,width = 0.45)
    
    return_results@plot.f1 <- f1.plot
  
  }
  
  #Factor II
  if(post.hoc.object@f.levels2.null == FALSE){
    w.data.f2.comps <- post.hoc.object@f2.comps
    w.data.f2.vals <- interaction.object@f2.averages.se
    factor.levels <- interaction.object@f.levels2
    
    factor.levels <- str_wrap(factor.levels, width = str.width)
    w.data.f2.vals[,"Factor"] <- str_wrap(w.data.f2.vals[,"Factor"], width = str.width)
    w.data.f2.comps[,"group1"] <- str_wrap(w.data.f2.comps[,"group1"], width = str.width)
    w.data.f2.comps[,"group2"] <- str_wrap(w.data.f2.comps[,"group2"], width = str.width)
    
    w.data.f2.vals[,"Factor"] <- factor(w.data.f2.vals[,"Factor"],levels = factor.levels)
    w.data.f2.comps[,"group1"] <- factor(w.data.f2.comps[,"group1"], levels = factor.levels)
    w.data.f2.comps[,"group2"] <- factor(w.data.f2.comps[,"group2"], levels = factor.levels)
    
    return_results@plot.data.f2 <- w.data.f2.vals
    return_results@plot.comp.f2 <- w.data.f2.comps
    
    y_axis_max <- (RobustMax(w.data.f2.vals[,"Mean"]) + RobustMax(w.data.f2.vals[,"SE"]))
    y_axis_min <- (RobustMin(w.data.f2.vals[,"Mean"]) - RobustMin(w.data.f2.vals[,"SE"]))
    
    f2.plot <- ggplot(data = w.data.f2.vals, aes(x= Factor, y= Mean))
    f2.plot <- f2.plot + coord_cartesian(ylim = c(y_axis_min/y.axis.length.factor,y_axis_max*y.axis.length.factor))  
    f2.plot <- f2.plot + geom_bar(position=position_dodge(), stat="identity", fill = "#56B4E9") 
    f2.plot <- f2.plot + theme_fivethirtyeight() + scale_colour_economist() 
    f2.plot <- f2.plot + geom_errorbar(aes(ymin=Mean-1*SE, ymax=Mean+1*SE),
                                       width=.2,                 
                                       position=position_dodge(.9), size = 1)
    
    
    #Annotations based on p-vals
    base.length <- 1.05
    f2.plot.par <- f2.plot
    for (i in c(1:dim(w.data.f2.comps)[1])){
      
      if(w.data.f2.comps[i,"pvalue"] < p.val.criteria){
        
        f2.plot.par <- f2.plot.par + annotate("segment", x = w.data.f2.comps[i,"group1"], 
                                              xend = w.data.f2.comps[i,"group2"] , 
                                              y = y_axis_max*base.length, 
                                              yend = y_axis_max*base.length, colour = "red", size = 1)
      }
      base.length <- base.length + 0.025
    }
    
    
    #Annotations based on BF-s
    base.length <- 1.05
    f2.plot.BF <- f2.plot
    for (i in c(1:dim(w.data.f2.comps)[1])){
      
      if(w.data.f2.comps[i,"BF"] > BF.criteria){
        
        f2.plot.BF <- f2.plot.BF + annotate("segment", x = w.data.f2.comps[i,"group1"], 
                                            xend = w.data.f2.comps[i,"group2"] , 
                                            y = y_axis_max*base.length, 
                                            yend = y_axis_max*base.length, colour = "red", size = 1)
      }
      base.length <- base.length + 0.025
    }
    
    f2.plot.BF <- f2.plot.BF + theme(axis.title.y=element_blank(),
                                     axis.text.y=element_blank(),
                                     axis.ticks.y=element_blank())
    
    
    #F1 return plot  
    f2.plot.par <- f2.plot.par + ggtitle(plot.title.f2.par) +  theme(plot.title = element_text(size = 10, face = "bold"))
    f2.plot.BF <- f2.plot.BF + ggtitle(plot.title.f2.bayes) +  theme(plot.title = element_text(size = 10, face = "bold"))
    
    f2.plot <- ggdraw() +
      draw_plot(f2.plot.par, x = 0,y = 0,height = 1,width = 0.55) +  
      draw_plot(f2.plot.BF, x = 0.55,y = 0,height = 1,width = 0.45)
    
    return_results@plot.f2 <- f2.plot
    
  }
  
  
  #Interaction plot
  if(post.hoc.object@f.levels1.null == FALSE & post.hoc.object@f.levels2.null == FALSE){
    w.data.inter.comps <- post.hoc.object@interaction.comps
    w.data.inter.vals <- interaction.object@inter.averages.se
    
    factor.levels <- interaction.object@f.levels.int
    w.data.inter.vals[,"Factor"] <- factor(w.data.inter.vals[,"Factor"],levels = factor.levels)
    w.data.inter.comps[,"group1"] <- factor(w.data.inter.comps[,"group1"], levels = factor.levels)
    w.data.inter.comps[,"group2"] <- factor(w.data.inter.comps[,"group2"], levels = factor.levels)
    
    factor.levels <- str_wrap(factor.levels, width = str.width)
    w.data.inter.vals[,"Factor"] <- str_wrap(w.data.inter.vals[,"Factor"], width = str.width)
    w.data.inter.comps[,"group1"] <- str_wrap(w.data.inter.comps[,"group1"], width = str.width)
    w.data.inter.comps[,"group2"] <- str_wrap(w.data.inter.comps[,"group2"], width = str.width)
    
    
    return_results@plot.data.inter <- w.data.inter.vals
    return_results@plot.comp.inter <- w.data.inter.comps
    
    y_axis_max <- (RobustMax(w.data.inter.vals[,"Mean"]) + RobustMax(w.data.inter.vals[,"SE"]))
    y_axis_min <- (RobustMin(w.data.inter.vals[,"Mean"]) - RobustMin(w.data.inter.vals[,"SE"]))
    
    inter.plot <- ggplot(data = w.data.inter.vals, aes(x= Factor, y= Mean))
    inter.plot <- inter.plot + coord_cartesian(ylim = c(y_axis_min/y.axis.length.factor,y_axis_max*y.axis.length.factor*1.5))  
    inter.plot <- inter.plot + geom_bar(position=position_dodge(), stat="identity", fill = "#56B4E9") 
    inter.plot <- inter.plot + theme_fivethirtyeight() + scale_colour_economist() 
    inter.plot <- inter.plot + geom_errorbar(aes(ymin=Mean-1*SE, ymax=Mean+1*SE),
                                       width=.2,                 
                                       position=position_dodge(.9), size = 1)
    inter.plot <- inter.plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
    
    #Annotations based on p-vals
    base.length <- 1.05
    inter.plot.par <- inter.plot
    for (i in c(1:dim(w.data.inter.comps)[1])){
      
      if(w.data.inter.comps[i,"pvalue"] < p.val.criteria){
        
        inter.plot.par <- inter.plot.par + annotate("segment", x = w.data.inter.comps[i,"group1"], 
                                              xend = w.data.inter.comps[i,"group2"] , 
                                              y = y_axis_max*base.length, 
                                              yend = y_axis_max*base.length, colour = "red", size = 1)
      }
      base.length <- base.length + 0.025
    }
    
    
    #Annotations based on BF-s
    base.length <- 1.05
    inter.plot.BF <- inter.plot
    for (i in c(1:dim(w.data.inter.comps)[1])){
      
      if(w.data.inter.comps[i,"BF"] > BF.criteria){
        
        inter.plot.BF <- inter.plot.BF + annotate("segment", x = w.data.inter.comps[i,"group1"], 
                                            xend = w.data.inter.comps[i,"group2"] , 
                                            y = y_axis_max*base.length, 
                                            yend = y_axis_max*base.length, colour = "red", size = 1)
      }
      base.length <- base.length + 0.025
    }
    
    inter.plot.BF <- inter.plot.BF + theme(axis.title.y=element_blank(),
                                     axis.text.y=element_blank(),
                                     axis.ticks.y=element_blank())
    
    
    #F1 return plot  
    inter.plot.par <- inter.plot.par + ggtitle(plot.title.inter.par) +  theme(plot.title = element_text(size = 10, face = "bold"))
    inter.plot.BF <- inter.plot.BF + ggtitle(plot.title.inter.bayes) +  theme(plot.title = element_text(size = 10, face = "bold"))
    
    inter.plot <- ggdraw() +
      draw_plot(inter.plot.par, x = 0,y = 0,height = 1,width = 0.55) +  
      draw_plot(inter.plot.BF, x = 0.55,y = 0,height = 1,width = 0.45)
    
    return_results@plot.inter <- inter.plot
    
  }
  
  
  
  
  return(return_results)
}





#TESTING
a <- interaction.maker(df = TEST_FILE,factor2 = "F2", value = "VAL")
aa <- post_hoc(a,post.hoc.type = "duncan.test")
aaa <- post_hoc_plot(post.hoc.object = aa,interaction.object = a,p.val.criteria = 0.05,BF.criteria = 0)


data <- a@df
data <- data[data[,"interaction"] == "A###E"| data[,"interaction"] == "B###Q",]
BF.res = ttestBF(formula = as.formula(VAL ~ interaction), data = data)
abs(BF.res@bayesFactor[,"bf"])




#Playground
library(BEST)
hdi(data[data[,"interaction"] == "B###Q","VAL"], credMass = 0.682)
