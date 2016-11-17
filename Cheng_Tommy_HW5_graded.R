#Author: Tommy Cheng
#Assignment 5
#Date: October 13 2016

require(ggplot2)
library(reshape2)

dat <- data.frame(diamonds)   #convert the dataset to a dataframe in R

#Problem1
#This function prints all the classes of the objects data.frame and print the attributes of a 
#specific dataset loaded in the function
#input: data = dataframe
#output: 
#
methods(class="data.frame")  #print all methods of the class data.frame

func1 <- function(data){
print(attributes(data))     #print the attributes of the dataframe diamond
print(ncol(data))    #print number of columns in a datadrame
}

func1(diamonds)


#Problem2
#This function prints out the number of rows of a dataset input
#Input: data= dataframe 
#Return: number of rows (integer)
print_numrows <- function(data){
cat('number of rows= ' , nrow(data), '\n')  
}

print_numrows(diamonds)
#Problem3
#This functon prints out all the column names of a dataset 
#Input: data= dataframe
#Output: Column names 
#Stuart- The names should print one per line.
print_colname <- function(data){
  cat('Column names: \n', colnames(data))
}
print_colname(diamonds)

#Problem4
#This function prints all the types of each column of a dataset
#input: data = Dataframe
#output: class of each column in the dataset

classdisplay <- function(data){
print(sapply(data, class))
}
classdisplay(diamonds)

#Problem5
# This function	loops through	a dataframe	and	calculate	the	mean	of	
# every	numeric	column. 
# Input: data = dataframe
# Output: mean (numeric)
find_mean <- function(data){
  numeric <- sapply(data, is.numeric) #check to see if the column is numeric
  print(sapply(data[numeric], mean))  #print out all the numeric column means
}
find_mean(diamonds)
#Problem6
#This function finds out all the factor class column and prints a frequency table for each factor 
#column
#Input: data = dataframe 
#Output = table of factor class
freq_tables <- function(data) {
  lapply(data[, sapply(data,is.factor)], table) #prints out the freqency table 
}

freq_tables(diamonds)

#Problem7
#This function calculates the number of NA rows of each column and gives the percentages of it
#input: data = dataframe
#output: number of rows and the percentages 

find_na <- function(data){
  #this function will loop through any dataframe and determine the number of rows 
  #containing NA (missing	value) in each column and the percentage of rows containing
  #an NA in any of the columns
  countsNArows<- apply(sapply(data,is.na),2,sum) # count the number of NA in each column
  percent <- sapply(data, function(x) sum(is.na(x))/length(x))# count the percentage of NA in each column
  dat <- rbind(countsNArows, percent)
  return(dat)
}

find_na(diamonds)
#Problem8
#This functions takes any dataframe as a parameter and returns a dataframe that 
#contains each pair of column names in the first column as a single string
#seperated by a "-" and their corresponding Pearson correlation coefficient
#in the second column.
#Input: data= dataframe
#Ouput: the combination of column names and their Pearson coefficients

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


#Problem9
#This function takes any dataframe as a parameter and returns a dataframe that 
#contains each pair of column names in the first column as a single string
#seperated by a "-" and their corresponding Pearson correlation coefficient
#in the second column.
#Input: data= dataset
#Output: scatter plot of each 2 columns and correlations
#
plotcorr <- function(data){
corr <- pearson(data)     #find the correlation of each two columns in the data set
numerics <- sapply(data, is.numeric)    #check numeric columns 
data <- data[,numerics]   #extract numeric columns
combos <- combn(names(data), 2)   #find all the combos of the names of columns

for (i in 1:nrow(corr)){        #plot all the scatter plots of each 2 columsn in the datasets with ggplots
  p <- ggplot(data, aes(x = data[,combos[1,i]], y = data[,combos[2,i]])) + 
    geom_point(size = 0.4) + 
    ggtitle(paste(corr[i,1], corr[i,2], sep = '  r = ')) + xlab(combos[1,i]) + ylab(combos[2,i])
  print(p)
}

}

#plotcorr(diamonds)
