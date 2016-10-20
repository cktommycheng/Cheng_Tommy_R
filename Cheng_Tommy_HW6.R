#Tommy Cheng
#MAT 510 Assignment 6
#October 19 2016


#Problem2
require(ggplot2)
require(grid)


data("diamonds")

p <- ggplot(diamonds, aes(carat, price)) + labs(title='Diamonds-Weight to Price by Color')+ theme(plot.title = element_text(size = rel(1.4), colour = "blue"))+xlab("weight") + ylab("price")+ geom_point(aes(colour = factor(color)))
print(p)


#Problem3
g <- ggplot(diamonds, aes(log(carat), log(price)))+ labs(title='Diamonds-Weight to Price(Linear)')+ theme(plot.title = element_text(size = rel(1.8), colour = "blue"))+xlab("weight") + ylab("price")+ geom_point(aes(colour = factor(color)))
print(g)

#Problem4
price_carat_lm <- lm(log(price) ~ log(carat), data=diamonds) 
summary(price_carat_lm)
price_residual <- resid(price_carat_lm)
k<-ggplot(diamonds, aes(log(carat), price_residual))+ geom_point(aes(colour = factor(color))) + xlab('Weight') + ylab('Price Residuals') +labs(title='Diamonds-Weight to Price by Color')+ theme(plot.title = element_text(size = rel(1.6), colour = "blue"))+theme(legend.position = 'top')
print(k)

#problem5
main_plot <- k+ guides(col = guide_legend(nrow = 1)) + theme(plot.title = element_text(size = rel(2)))
hist_price <- ggplot(diamonds, aes(price)) + geom_histogram(aes(y= ..density.., colour= factor(color)), binwidth = 50) + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position='none') 
hist_carat <- ggplot(diamonds, aes(carat)) + geom_histogram(aes(y= ..density.., colour= factor(color)), binwidth = 0.0225) + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position='none')
#print(hist_price)
#print(hist_carat)
vp1 <- viewport(width = 0.4, height = 0.2, x = 0.265, y = 0.15)
vp2 <- viewport(width = 0.4, height = 0.2, x = 0.8, y = 0.75)
print(main_plot)
print(hist_price, vp = vp1)
print(hist_carat, vp = vp2)



#problem6 
vp3 <- viewport(angle = 90, width = 0.6, height = 0.2, x = 0.15, y = 0.55)
vp4 <- viewport(width = 0.6, height = 0.2, x = 0.6, y = 0.1)
vp5 <- viewport(width = 0.75, height = 0.7, x = 0.6, y = 0.6)
print(hist_price, vp = vp3)
print(hist_carat, vp = vp4)
print(main_plot, vp = vp5)