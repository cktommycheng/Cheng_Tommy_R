#Tommy Cheng 
#MAT510 HW6
#26 Oct 2016 
require(ggplot2)
require(grid)

##########Helper function############

test_data<- diamonds[1:300, ]

freq_table <- function(data) {
  lapply(data[, sapply(data,is.factor)], table) #prints out the freqency table 
}


printSummary <-function(data){
  lapply(data[, sapply(data,is.numeric)], summary)
}


pearson<- function(data){
  num <- sapply(data, is.numeric) #check to see if columns are numeric
  new_data <- data[,num]  #create a new dataframe to store the numeric columns 
  names <- colnames(new_data)   #create vector to store the numeric colnames 
  combonames <- combn(names, 2) #find all the combinations of any 2 col names 
  combo <- combn(length(colnames(new_data)), 2)   #find all the combination of the indices of the colnames
  variable <- paste(combonames[1,], combonames[2,], sep = '-')  #create vector to store the variables combinations
  pearson <- Pcoeff <- c()    #create empty vectors 
  
  for(i in 1:length(variable)){
    p <- cor(x= new_data[combo[1,i]], y = new_data[combo[2,i]])  #calculates the correlations between any two of the cols
    Pcoeff[i] <- p[1]   #extract the number from a list 
  }
  return(data.frame(variable, Pcoeff))    #combine as dataframe
}

abs_pearson <- function(dataset, threshold){
  row_index <- which(abs(dataset[,2]) > threshold)
  return(dataset[row_index, ])
}

find_Rsquare<- function(data){
  num <- sapply(data, is.numeric) #check to see if columns are numeric
  new_data <- data[,num]  #create a new dataframe to store the numeric columns 
  names <- colnames(new_data)   #create vector to store the numeric colnames 
  combonames <- combn(names, 2) #find all the combinations of any 2 col names 
  combo <- combn(length(colnames(new_data)), 2)   #find all the combination of the indices of the colnames
  variable <- paste(combonames[1,], combonames[2,], sep = '-')  #create vector to store the variables combinations
  Rsquare <- c()    #create empty vectors to store Rsquare
  
  for(i in 1:length(variable)){
    regression <- paste0(combonames[1,i], " ~ ", combonames[2,i]) 
    r1 <- summary( lm(as.formula(regression), data=new_data) )$r.squared
    Rsquare[i] <- r1  
  }
  return(data.frame(variable, Rsquare))    #combine as dataframe
}




# Multiple plot function
# Reference: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


numeric_plot <- function(data, plot_switch, binVec) {
  num <- sapply(data, is.numeric)
  data <- data[,num]
  for(name in colnames(data)) {
    
    if(plot_switch == "on"){
      grid.newpage()
      m <- lapply(data[name], mean)
      plot1 <- ggplot(data, aes_string(name)) + geom_histogram(fill="blue") + geom_vline(xintercept = m[[1]], colour="red") 
      plot2 <- ggplot(data, aes_string(name)) + geom_histogram(aes(y= ..density..), fill="blue") + geom_vline(xintercept = m[[1]], colour="red")
      #multiplot(plot1, plot2, cols = 1)
      pushViewport(viewport(layout = grid.layout(1, 2)))
      print(plot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
      print(plot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
    }
    
    if(plot_switch == "grid"){
      count_plots <- list()
      density_plots <- list()
      if(missing(binVec)){
        print(ggplot(data, aes_string(name), color = "blue") + geom_histogram(fill="blue")+ labs(title= "default bins"))
        print(ggplot(data, aes_string(name), color = "blue") + geom_histogram(aes(y= ..density..), fill="blue")+ labs(title= "default bins"))
      }else{
        for(i in 1:length(binVec)) {
          k <- ggplot(data, aes_string(name), color = "blue") + geom_histogram(fill="blue", bins = bin[i])+ labs(title= paste(binVec[i], "bins"))
          count_plots[[i]] <- k
        }
        multiplot(plotlist = count_plots, cols = 3)
        
        for(i in 1:length(binVec)) {
          k <- ggplot(data, aes_string(name), color = "blue") + geom_histogram(aes(y= ..density..), fill="blue", bins = bin[i])+ labs(title= paste(binVec[i], "bins"))
          density_plots[[i]] <- k
        }
        multiplot(plotlist = density_plots, cols = 3)
        
      }
    }
  }
}

cata_binary_plot <-function(data, plot_switch){
  cata_binary <- sapply(data, function(x) (is.factor(x) || is.logical(x)))
  cata_binary_data <- data[cata_binary]
  
  if(plot_switch == "on" || plot_switch == "grid") {
    for(name in colnames(cata_binary_data)) {
      j <- ggplot(cata_binary_data, aes_string(name), color = "grey") + geom_bar(fill="grey")
      print(j)
    }
  }
}


#main function
explore <- function(dataframe, plot_switch, thres, binVec){
  new_dataframe <- freq_table(dataframe)
  allSummary <- printSummary(dataframe)
  Coeff_table <- pearson(dataframe)
  AbsCoeff_table <-abs_pearson(Coeff_table, thres)
  Rsquare_table <- find_Rsquare(dataframe)
  numeric_plot(dataframe, plot_switch, binVec)
  cata_binary_plot(dataframe, plot_switch)
  new_list <-list(new_dataframe, allSummary, Rsquare_table, AbsCoeff_table)
  return(new_list)
}



#TestCase
#explore(test_data, "grid", 0.02)
mm <- explore(test_data, "off", 0.3, c(20, 60, 80, 100))