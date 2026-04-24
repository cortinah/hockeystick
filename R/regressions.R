https://www.stat.uchicago.edu/~yibi/teaching/stat224/L15.pdf

supvis = read.table("http://www.stat.uchicago.edu/~yibi/s224/data/P176.txt", h=T)
library(ggplot2)
ggplot(supvis, aes(x=X, y=Y)) +geom_point() +geom_smooth(method='lm') +
  labs(x="# of Workers (X)", y="# of Supervisors (Y)")

lm1 = lm(Y ~ X, data=supvis)
ggplot(supvis, aes(x=X, y=lm1$res))+geom_point() +
  xlab("# of Supervised Workers (X)")+
  ylab("Residuals")+ geom_hline(yintercept=0)


lm2 = lm(Y ~ X + I(X^2), data=supvis)

ggplot(supvis, aes(x=X, y=lm2$res)) +geom_point() + geom_hline(yintercept=0) +
  xlab("# of Supervised Workers (X)") + ylab("Residuals")

library(MASS)
boxcox(lm(Y ~ X + I(X^2), data=supvis))

lm3 = lm(log(Y) ~ X + I(X^2), data=supvis)
summary(lm3)$coef
ggplot(supvis, aes(x=X, y=lm3$res)) +geom_point() + geom_hline(yintercept=0) +
  xlab("# of Supervised Workers (X)") + ylab("Residuals")
