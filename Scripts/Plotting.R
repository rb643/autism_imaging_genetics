library('ggplot2')
library("Rmisc")

setwd("~/Dropbox/PLS/PLSx_and_y_variables/Figure3")

## Load delta CT and scores
Scores = read.table('scores.txt',header = TRUE) # delta corticcal thickness

p1 <- ggplot(Scores, aes(x=merged.Discovery, y=merged.Validation)) +
  geom_point(colour = "black", size =3, shape = 16) + geom_point(colour = "#99CCFF") +
  geom_smooth(method=lm,   # Add linear regression line
              se=TRUE)  +  # Add shaded confidence region
  xlab('Discovery') +
  ylab('Validation 1 - GU') +
  ggtitle('∆CT') 

p2 <- ggplot(Scores, aes(x=merged.Discovery, y=merged.KKI_repli2)) +
  geom_point(colour = "black", size =3, shape = 16) + geom_point(colour = "#99CCFF") +
  geom_smooth(method=lm,   # Add linear regression line
              se=TRUE)  +  # Add shaded confidence region
  xlab('Discovery') +
  ylab('Validation 2 - KKI') +
  ggtitle('∆CT')

p3 <- ggplot(Scores, aes(x=merged.Validation, y=merged.KKI_repli2)) +
  geom_point(colour = "black", size =3, shape = 16) + geom_point(colour = "#99CCFF") +
  geom_smooth(method=lm,   # Add linear regression line
              se=TRUE)  +  # Add shaded confidence region
  xlab('Validation 1 - GU') +
  ylab('Validation 2 - KKI') +
  ggtitle('∆CT')

multiplot(p1, p2, p3, cols = 3)


p4 <- ggplot(Scores, aes(x=plsdiscscores..Comp.1., y=plsvalscores..Comp.1.)) +
  geom_point(colour = "black", size =3, shape = 16) + geom_point(colour = "#99CCFF") +
  geom_smooth(method=lm,   # Add linear regression line
              se=TRUE) +  # Add shaded confidence region
  xlab('Discovery') +
  ylab('Validation 1 - GU') +
  ggtitle('PLSR Scores')

p5 <- ggplot(Scores, aes(x=plsdiscscores..Comp.1., y=plskkiscores..Comp.1.)) +
  geom_point(colour = "black", size =3, shape = 16) + geom_point(colour = "#99CCFF") +
  geom_smooth(method=lm,   # Add linear regression line
              se=TRUE) +  # Add shaded confidence region
  xlab('Discovery') +
  ylab('Validation 2 - KKI') +
  ggtitle('PLSR Scores')

p6 <- ggplot(Scores, aes(x=plsvalscores..Comp.1., y=plskkiscores..Comp.1.)) +
  geom_point(colour = "black", size =3, shape = 16) + geom_point(colour = "#99CCFF") +
  geom_smooth(method=lm,   # Add linear regression line
              se=TRUE) +  # Add shaded confidence region
  xlab('Validation 1 - GU') +
  ylab('Validation 2 - KKI') +
  ggtitle('PLSR Scores')

multiplot(p4, p5, p6, cols = 3)


p7 <- ggplot(Scores, aes(x=plsdiscscores..Comp.1., y=merged.Discovery)) +
  geom_point(colour = "black", size =3, shape = 16) + geom_point(colour = "#99CCFF") +
  geom_smooth(method=lm,   # Add linear regression line
              se=TRUE)  +  # Add shaded confidence region
  xlab('Scores') +
  ylab('∆CT') +
  ggtitle('Discovery Data')

p8 <- ggplot(Scores, aes(x=plsvalscores..Comp.1., y=merged.Validation)) +
  geom_point(colour = "black", size =3, shape = 16) + geom_point(colour = "#99CCFF") +
  geom_smooth(method=lm,   # Add linear regression line
              se=TRUE)  +  # Add shaded confidence region
  xlab('Scores') +
  ylab('∆CT') +
  ggtitle('Validation 1 - GU')

p9 <- ggplot(Scores, aes(x=plskkiscores..Comp.1., y=merged.KKI_repli2)) +
  geom_point(colour = "black", size =3, shape = 16) + geom_point(colour = "#99CCFF") +
  geom_smooth(method=lm,   # Add linear regression line
              se=TRUE)  +  # Add shaded confidence region
  xlab('Scores') +
  ylab('∆CT') +
  ggtitle('Validation 2 - KKI')

multiplot(p7, p8, p9, cols = 3)

  
# load the OR's
DiscoveryOR = read.csv("DiscoveryOR.csv", header = TRUE)
ValidationOR = read.csv("ValidationOR.csv", header = TRUE)
ValidationOR2 = read.csv("Validation2OR.csv", header = TRUE)

p10 <- ggplot(data=DiscoveryOR, aes(x=Dataset, y=OR, ymin=Lower.CI, ymax=Upper.CI..95.., color = Category)) +
  geom_pointrange() + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  ylab("Mean (95% CI)") + ggtitle('Discovery') + 
  theme(axis.title.y=element_blank(),
        legend.position = c(0.65,0.8),
        legend.title = element_blank(),
        legend.text=element_text(size=6), 
        legend.background = element_rect(fill=alpha('white',0.5)))


p11 <- ggplot(data=ValidationOR, aes(x=Dataset, y=OR, ymin=Lower.CI, ymax=Upper.CI..95.., color = Category)) +
  geom_pointrange() + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  ylab("Mean (95% CI)") + ggtitle('Validation 1') + 
  theme(axis.title.y=element_blank(),
        legend.position = c(0.65,0.8),
        legend.title = element_blank(), 
        legend.text=element_text(size=6),
        legend.background = element_rect(fill=alpha('white',0.5)))

p12 <- ggplot(data=ValidationOR2, aes(x=Dataset, y=OR, ymin=Lower.CI, ymax=Upper.CI..95.., color = Category)) +
  geom_pointrange() + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  ylab("Mean (95% CI)") + ggtitle('Validation 2') + 
  theme(axis.title.y=element_blank(),
        legend.position = c(0.65,0.8),
        legend.title = element_blank(), 
        legend.text=element_text(size=6),
        legend.background = element_rect(fill=alpha('white',0.5)))

multiplot(p1, p4, p7, p10, p2, p5, p8, p11, p3, p6, p9, p12, plotlist = NULL, cols = 3)

### supplementary figures
setwd("~/Dropbox/PLS/PLSx_and_y_variables/SuppFig")
Loading = read.table('loadings.txt',header = TRUE)

sp1 <- ggplot(Loading, aes(x=plsdiscloadings..Comp.1., y=plsvalloadings..Comp.1.)) +
  geom_point(colour = "black", size =3, shape = 16) + geom_point(colour = "#99CCFF") +
  geom_smooth(method=lm,   # Add linear regression line
              se=TRUE)  +  # Add shaded confidence region
  xlab('Discovery') +
  ylab('Validation 1 - GU') +
  ggtitle('Loadings') 

sp2 <- ggplot(Loading, aes(x=plsdiscloadings..Comp.1., y=plskkiloadings..Comp.1.)) +
  geom_point(colour = "black", size =3, shape = 16) + geom_point(colour = "#99CCFF") +
  geom_smooth(method=lm,   # Add linear regression line
              se=TRUE)  +  # Add shaded confidence region
  xlab('Discovery') +
  ylab('Validation 2 - KKI') +
  ggtitle('Loadings') 

sp3 <- ggplot(Loading, aes(x=plsvalloadings..Comp.1., y=plskkiloadings..Comp.1.)) +
  geom_point(colour = "black", size =3, shape = 16) + geom_point(colour = "#99CCFF") +
  geom_smooth(method=lm,   # Add linear regression line
              se=TRUE)  +  # Add shaded confidence region
  xlab('Validation 1 - GU') +
  ylab('Validation 2 - KKI') +
  ggtitle('Loadings') 

multiplot(sp1, sp2, sp3, cols = 3)

Scores = read.table('scores.txt',header = TRUE) # delta corticcal thickness

ssp1 <- ggplot(Scores, aes(x=merged.Discovery, y=merged.KKI_replimales)) +
  geom_point(colour = "black", size =3, shape = 16) + geom_point(colour = "#99CCFF") +
  geom_smooth(method=lm,   # Add linear regression line
              se=TRUE)  +  # Add shaded confidence region
  xlab('Discovery') +
  ylab('Validation 2 - KKI \n males only') +
  ggtitle('∆CT')

ssp2 <- ggplot(Scores, aes(x=plsdiscscores..Comp.1., y=plskkimalesscores..Comp.1.)) +
  geom_point(colour = "black", size =3, shape = 16) + geom_point(colour = "#99CCFF") +
  geom_smooth(method=lm,   # Add linear regression line
              se=TRUE)  +  # Add shaded confidence region
  xlab('Discovery') +
  ylab('Validation 2 - KKI \n males only') +
  ggtitle('Scores')

ssp3 <- ggplot(Loading, aes(x=plsdiscloadings..Comp.1., y=plskkimalesloadings..Comp.1.)) +
  geom_point(colour = "black", size =3, shape = 16) + geom_point(colour = "#99CCFF") +
  geom_smooth(method=lm,   # Add linear regression line
              se=TRUE)  +  # Add shaded confidence region
  xlab('Discovery') +
  ylab('Validation 2 - KKI \n males only') +
  ggtitle('Loadings')

multiplot(ssp1, ssp2, ssp3, cols = 3)

write.table(DeltaCT$disc_scores, file = "DiscScores.txt", row.names = FALSE, col.names = FALSE)
write.table(DeltaCT$val_scores, file = "ValScores.txt", row.names = FALSE, col.names = FALSE)