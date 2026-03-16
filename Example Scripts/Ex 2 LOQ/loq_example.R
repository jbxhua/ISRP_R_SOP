
rm(list=ls(all=TRUE))

blank.mass <- read.csv('./Example Data/Ex 2/blanks_LOQ_20220919.csv', header = T)
congener.list <- read.csv('./Example Data/Ex 2/blanks_LOQ_20220919.csv', header = F)

blank.ids <- blank.mass[,1]
blank.mass <- blank.mass[,-1]
congener.list <- congener.list[1,-1]
congener.list <- as.data.frame(t(congener.list))

log.mass <- log10(blank.mass)

blank.mass[blank.mass == 0] <- 1e-6

log.mass <- log10(blank.mass)
log.mass <- as.data.frame(t(log.mass))
log.avg <- rowMeans(log.mass)
log.sd <- apply(log.mass, 1, sd)
n <- nrow(blank.mass)
log.loq <- log.avg + (2.325 * (log.sd/sqrt(n)))
loq <- as.data.frame(10^log.loq)

final.loq <- cbind(congener.list, loq)
colnames(final.loq) <- c('Congener', 'LOQ')
final.loq$Congener <- factor(final.loq$Congener, levels = final.loq$Congener)
summary(final.loq)

