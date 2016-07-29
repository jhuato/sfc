# data
x<-c(609, 629, 620, 564, 645, 493, 606, 629, 660, 630, 660, 629)
y<-c(241, 222, 233, 207, 247, 189, 226, 240, 226, 215, 226, 226)
z<-c(0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0)

ANOVA <- aov ( y ~ x ) # Analysis of variance
summary(ANOVA)

ANOV1 <- aov( y ~ x + z ) 
summary(ANOV1)

t.test(x, mu=575) # t test of H0: mu_x=575, H1: not so

cor.test(x,y) # Pearson test of H0: cor(x,y)=0, H1: not so
