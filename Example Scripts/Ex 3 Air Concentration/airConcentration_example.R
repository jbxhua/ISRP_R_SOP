
rm(list=ls(all=TRUE))

# FUNCTIONS -----

# kpuf calculations

kpuf.calc <- function(pcb, R, temp.c) {
  duoa <- data.frame(matrix(nrow = nrow(pcb), ncol = 1))
  koa <- data.frame(matrix(nrow = nrow(pcb), ncol = 1))
  for (i in 1:nrow(pcb)) {
    duoa[i,1] <- (-0.13 * pcb[i,3] + 2.9 * pcb[i,2] - 47.8) * 1000
  }
  for (j in 1:nrow(pcb)) {
    koa[j,1] <- pcb[j,5] - (duoa[j,1] / (2.303 * R)) * ((1 / (273.15 + temp.c)) - (1 / 298.15))
  }
  l.kpuf <- (0.6366 * koa) - 3.1774
  kpuf <- 10^(l.kpuf)
  return(kpuf)
}

# concentration calculation

conc.calc <- function(fsch.air,veff,flist){
  
  sch.conc <- data.frame(matrix(nrow = nrow(fsch.air), ncol = ncol(fsch.air)))
  for (c.i in 1:nrow(fsch.air)) {
    for (c.j in  1:ncol(fsch.air)) {
      sch.conc[c.i,c.j] <- fsch.air[c.i,c.j] / veff[c.j,2]
    }
  }
  sch.conc.t <- as.data.frame(rowSums(sch.conc))
  sch.conc.t <- signif(sch.conc.t,2)        # significant figures
  names(sch.conc.t) <- 'Concentration'
  sch.conc.t <- cbind(flist, sch.conc.t)   # final concentrations ng/m3
  
  return(sch.conc.t)
}

  
# read in example data -----

pcb <- read.csv('./Example Data/Ex 3/koaCalc.csv', header = T)      # pcb values for calculation koa
kpuf <- read.csv('./Example Data/Ex 3/kpuf.csv', header = T)
sample.mass <- read.csv('./Example Data/Ex 3/ogMass_practice.csv', header = T)
congener.list <- read.csv('./Example Data/Ex 3/ogMass_practice.csv', header = F)
sample.type <- read.csv('./Example Data/Ex 3/ogSampleType_20220927.csv', header = T)

congener.list <- as.data.frame(t(congener.list[1,-c(1:2)]))

# VEFF ASSUMPTIONS AND CONSTANTS - CHANGE AS NEEDED
##################################################

t.days <- 29
ws.on <- 0.11   #m/s
ws.off <- 0.07  #m/s
f.on <- 0.36
f.off <- 1 - f.on
c <- 1.326
puf.sa = 0.0153 #m2
v.puf <- 2.295e-04 #m3 
v.puf.t <- as.character(v.puf)
den.puf <- 21300 #g/m3 
temp.c = 23                # enter temperature in C (73 F used)
R = 8.3144               # gas constant
cong <- pcb[,1]
names(cong) <- 'Congener'


# koa to kpuf based on temperature

kpuf <- kpuf.calc(pcb,R,temp.c)
kpuf <- kpuf * den.puf

# SAMPLING RATE CALCULATION -----

rs <- data.frame(matrix(nrow = nrow(pcb), ncol = 1))

for (l in 1:nrow(pcb)) {
  rs[l,1] <- ((f.on * (sqrt(ws.on))) + (f.off * (sqrt(ws.off))))*(1/(pcb[l,3]^(1/3)))*(10^((0.0012*t.days)+c))
}

# EFFECTIVE VOLUME CALCULATION -----

v.eff <- data.frame(matrix(nrow = nrow(pcb), ncol = 1))

for (ii in 1:nrow(pcb)) {
  v.eff[ii,1] <- (v.puf * kpuf[ii,1]) * (1 - (exp((-(rs[ii,1] / (v.puf * kpuf[ii,1]))* t.days))))
}
names(v.eff) <- 'V_eff'

v.eff <- as.data.frame(v.eff)
V.eff <- v.eff[-c(13,28,29,30,33,47,53,62,65,69,70,71,74,75,76,97,100,101,109,113,116,119,124,125,138,140,149,
                  151,157,163,166,168,173,193,199,204),]
V.eff <- cbind(congener.list,V.eff)

# AIR CONCENTRATION CALCULATION -----

# pull out only air samples

sch.mass.idx <- sample.mass
sch.air.t <- merge.data.frame(sch.mass.idx, sample.type)
sch.air <- sch.air.t[sch.air.t$sample.type == 'Air',1:176]
sch.air.list <- as.data.frame(sch.air[,1])
names(sch.air.list) <- 'bid'
sch.air <- sch.air[,-(c(1,2,176))]

# concentration calculation - run function

sch.air.conc <- conc.calc(sch.air,V.eff,sch.air.list)


