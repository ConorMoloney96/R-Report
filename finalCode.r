load("C:\\workarea\\Maths\\.RData")
midscores = read.csv("midscores.csv", header=T)
set.seed(15144127)
rows = c(sample(1:50,10), sample(51:100, 10))
midsample = midscores[rows,]
midearly = midsample$score[midsample$submit=="early"]
midlate = midsample$score[midsample$submit=="late"]
hist(midearly)
hist(midlate)

boxplot(midearly, midlate, xlab="Midterm scores")
points(x=1,y=mean(midearly),pch=20)
points(x=2,y=mean(midlate),pch=20)

dataTable <- matrix(c(mean(midearly),sd(midearly),quantile(midearly),IQR(midearly),mean(midlate),sd(midlate),quantile(midlate),IQR(midlate)),ncol=2,nrow=8,byrow=FALSE)
colnames(dataTable) <- c("Midearly","Midlate")
rownames(dataTable) <- c("Mean","SD","Min","1st Quantile","2nd Quantile","3rd Quantile","Max","IQR")
dataTable <- as.table(dataTable)
dataTable

qqnorm(midearly)
qqline(midearly)

qqnorm(midlate)
qqline(midlate)

shapiro.test(midearly)
shapiro.test(midlate)

 t.test(rows, mu=50)
 var.test(midlate, midearly)
  t.test(midearly, midlate, var.equal=TRUE)
  wilcox.test(midearly, midlate, mu=0, conf.int=T, conf.level=0.95, correct=T,paired=F, exact=F)
with(midscores, plot(score~day))
model=lm(score~day, data=midscores)
summary(model)
abline(model)
set.seed(15144127)
simreps=1000
phat=rep(0,simreps)
for(i in 1:simreps){phat[i] =mean(rbinom(n=50, size=1, prob=.5)) }
hist(phat)
qqplot(phat);qqline(phat)

set.seed(15144127)
for(i in 1:simreps){phat[i] =mean(rbinom(n=50, size=1, prob=.05))}
hist(phat)
qqnorm(phat);qqline(phat)
set.seed(15144127)
for(i in 1:simreps){phat[i] = mean(rbinom(n=40, size=1, prob=.5))}
hist(phat)
qqnorm(phat);qqline(phat)
set.seed(15144127)
for(i in 1:simreps){phat[i] =mean(rbinom(n=30, size=1, prob=.5)) }
hist(phat)
qqnorm(phat);qqline(phat)
set.seed(15144127)
for(i in 1:simreps){phat[i] = mean(rbinom(n=5, size=1, prob=.5)) }
hist(phat)
qqnorm(phat);qqline(phat)


round(pbinom(5, size=10, prob=0.65, lower=F), digits =4)
1-(round(pbinom(29, size = 100, prob = 0.2, lower=F), digits = 4))
round((pbinom(14, size=50, prob = 0.32, lower=F))- (pbinom(29, size=50, prob = 0.32, lower=F)), digits=4)
round(dpois(8, lambda=6), digits =4)
round(ppois(35, lambda = 41), digits=4)
(round(ppois(4, lambda = 1), digits=4))-(round(ppois(1, lambda = 1),digits=4))
round(pnorm(12, mean=7, sd=2.5, lower=F),digits=4)
round(pnorm(9.8, mean=10, sd=1, lower=F),digits=4)
round(pnorm(38, mean=50, sd=5), digits=4)
(round(pnorm(4, mean=5, sd=3.6, lower=F), digits=4)) - (round(pnorm(8, mean=5, sd=3.6, lower=F), digits=4))


