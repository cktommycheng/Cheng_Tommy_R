#Tommy Cheng
#MAT 510 Assignment 6
#October 19 2016
#Group A

require(ggplot2)
require(grid)

#Problem2: Creates a simple scatter plot of Weight (“Carat”) and Price using Color (the	
#“Color” column in the diamonds dataframe) as a facet. 

scatter_plot <- ggplot(diamonds, aes(carat, price)) + labs(title='Diamonds-Weight to Price by Color')+ theme(plot.title = element_text(size = rel(1.6), colour = "blue"))+xlab("weight") + ylab("price")+ geom_point(aes(colour = factor(color)))
print(scatter_plot)

#Problem3: This is a linear scatter plot of Price and weight by plotting the natural log of the two variables.
# This first creates 2 new columns of the natural log of the variables. The columns are then added to the dataframe
ln_carat <- log(diamonds$carat) 
ln_price <- log(diamonds$price)
diamonds[, "ln_carat"] <- ln_carat    #add new variables to the dataframe
diamonds[, "ln_price"] <- ln_price    #add new variables to the dataframe
linearscatter_plot <- ggplot(diamonds, aes(ln_carat, ln_price))+ labs(title='Diamonds-Weight to Price(Linear)')+ theme(plot.title = element_text(size = rel(1.8), colour = "blue"))+xlab("weight") + ylab("price")+ geom_point(aes(colour = factor(color)))
print(linearscatter_plot)

#Problem4: Use lienar regression to regress ln(price) on ln(carat) and extract the residual of price. Then Plot 
#the price residual against weight
price_carat_lm <- lm(log(price) ~ log(carat), data=diamonds)  #lienar regression using ordinary least square
price_residual <- resid(price_carat_lm)   #extract the residuals
k<-ggplot(diamonds, aes(log(carat), price_residual))+ geom_point(aes(colour = factor(color))) + xlab('Weight') + ylab('Price Residuals') +labs(title='Diamonds-Weight to Price by Color')+ theme(plot.title = element_text(size = rel(1.6), colour = "blue"))+theme(legend.position = 'top')
print(k)

#Problem5: Use the grid package to create a overlay of three plots:
#a density histogram of the price and the histogram, a density histogram of	carat and the detrended 
#plot of price residual against weight. 

main_plot <- k+ guides(col = guide_legend(nrow = 1)) + theme(plot.title = element_text(size = rel(2)))
hist_price <- ggplot(diamonds, aes(price)) + geom_histogram(aes(y= ..density.., colour= factor(color)), binwidth = 50) + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position='none') 
hist_carat <- ggplot(diamonds, aes(carat)) + geom_histogram(aes(y= ..density.., colour= factor(color)), binwidth = 0.0225) + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position='none')
vp1 <- viewport(width = 0.4, height = 0.2, x = 0.265, y = 0.15)     #create a viewport frame with specific sizes and coordinates
vp2 <- viewport(width = 0.4, height = 0.2, x = 0.8, y = 0.75)     #create a viewport frame with specific sizes and coordinates
print(main_plot)                      
print(hist_price, vp = vp1)       
print(hist_carat, vp = vp2)



#Problem6: Using grid package	to create the a overlay of three plots: The rotated histogram
#on the left, the price and the histogram on the bottom, and the detrended histogram on top of
#price histogram

#Stuart- If you run all these sequentially without clearing the previous graph (problem 5), these
#will start to over write that graph. Good job overall.

vp3 <- viewport(angle = 90, width = 0.6, height = 0.2, x = 0.15, y = 0.55)  #frame for histogram of price
vp4 <- viewport(width = 0.6, height = 0.2, x = 0.6, y = 0.1)          #frame for historgram of carat
vp5 <- viewport(width = 0.75, height = 0.7, x = 0.6, y = 0.6)       #frame for the detrended plot 
print(hist_price, vp = vp3)
print(hist_carat, vp = vp4)
print(main_plot, vp = vp5)