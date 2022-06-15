#Chapter 2
#Exercise 1. The use of the c and sum functions.
LengthCT <- c (75, 85, 91.6, 95, NA, 105.5, 106)
Tb <- c (0,0,1,NA,0,0,0)
mean(LengthCT, na.rm = TRUE)
#Exercise 2. The use of the cbind function using epidemiological data
Farm <- c (rep("MO", each = 4),"LN","SE","QM")
Month <- c (11,07,07,NA,09,09,11)
Boar <- cbind (Farm, Month, LengthCT, Tb)
Boar
dim(Boar)
ncol(Boar)
nrow(Boar)
#Exercise 3. The use of the vector function using epidemiological data.
Tb2 <- as.vector(Tb)
#Exercise 4. Working with a matrix.
D <- matrix(c(1,2,3,4,2,1,2,3,0), nrow = 3, ncol = 3, byrow = TRUE)
tD <- t(D)
tD * D
iD <- solve(D)
#Exercise 5. The use of the data.frame and list functions using epidemiological data.
Year <- c (00,00,01,NA,03,03,02)
Sex <- c (1,2,2,2,1,2,2)
LengthClass <- c(rep(1, each = 7))
Ecervi <- c(rep(0, each = 7))
Ecervi[4] <- NA
sqrtlengh<-sqrt(LengthCT)
Zuur <- data.frame(Farm,Month,Year,Sex,LengthClass,LengthCT,Ecervi,Tb,sqrtlengh)
Zuur
Zuur <- list(Farm,Month,Year,Sex,LengthClass,LengthCT,Ecervi,Tb,sqrtlengh)
#Exercise 6. The use of the read.table and scan functions using deep sea research data.
ISIT <- read.table(file = "ISIT.txt", header = TRUE)
ISIT
save(ISIT, file="ISITASCII.txt",ascii = TRUE)
ISIT
is.matrix(ISIT)
is.data.frame(ISIT)
ISITSCAN<-scan(file="ISIT.txt",what="character")
ISITSCAN
save(ISITSCAN, file="ISITSCANASCII.txt",ascii = TRUE)
ISITSCAN
is.matrix(ISITSCAN)
is.data.frame(ISITSCAN)
is.vector(ISITSCAN)
#Exercise 7. The use of the read.table or scan function using epidemiological data.
Deer<-read.table("Deer.txt",header=TRUE)
Deer
save(Deer, file="DeerASCII.txt",ascii = TRUE)
Deerascii<-read.table("DeerASCII.txt",header=TRUE)
Deerascii

#Chapter 3
#Exercise 1. Using the read.table function and accessing variables from a data frame with epidemiological data.
birdflu <- read.table(file="BirdFluCases.txt", sep = "\t")
attach(birdflu)
str(birdflu)
birdflu
namecases=c('cases2003','cases2004','cases2005','cases2006','cases2007','cases2008')
print("Tổng số ca bệnh cúm của mỗi quốc gia: ")
for (i in Country){
  sumcases_country=0
  for(j in namecases ){
    sumcases=sumcases+birdflu[birdflu$Country==i,j]
  }
  print(paste(i,':',sumcases_country))
}
print('Tổng số ca bệnh mỗi năm: ')
for (i in namecases){
  sumcases_year=sum(birdflu[,i])
  print(paste('2003: ',sumcases_year))
}
#Exercise 2. Using the read.table function and accessing subsets of a data frame with deep sea research data.
ISIT <- read.table(file = "ISIT.txt", header= TRUE, dec = ".")
names(ISIT)
str(ISIT)

ISIT.Station1 <- ISIT[ISIT$Station ==1,]
ISIT.Station1
names(ISIT.Station1)
str(ISIT.Station1)

nrow(ISIT.Station1)

min(ISIT.Station1$SampleDepth)
median(ISIT.Station1$SampleDepth)
mean(ISIT.Station1$SampleDepth)
max(ISIT.Station1$SampleDepth)

ISIT.Station2 <- ISIT[ISIT$Station ==2,]
nrow(ISIT.Station2)
min(ISIT.Station2$SampleDepth)
median(ISIT.Station2$SampleDepth)
mean(ISIT.Station2$SampleDepth)
max(ISIT.Station2$SampleDepth)

ISIT.Station3 <- ISIT[ISIT$Station ==3,]
nrow(ISIT.Station3)
min(ISIT.Station3$SampleDepth)
median(ISIT.Station3$SampleDepth)
mean(ISIT.Station3$SampleDepth)
max(ISIT.Station3$SampleDepth)

tapply(ISIT$Station, INDEX = ISIT$Station,length)

ISIT2 <- ISIT[ISIT$Station!=4 & ISIT$Station!=5 ,]
ISIT2

Year2002 <- ISIT2[ISIT2$Year ==2002,]

AllYear <- ISIT2[ISIT2$Month ==4,]

ISIT2Depth2000 <- ISIT2[ISIT2$SampleDepth >2000,]

I1 <- order(ISIT2Depth2000$SampleDepth)
showdata <-ISIT2Depth2000[I1,]

E3 <- ISIT2[ISIT2$Month ==4 & ISIT2$SampleDepth >2000,]

#Exercise 3. Using the write.table function with deep sea research data.
write.table(E3, file="ISITDepth2000April.txt")

#Exercise 4. Using the factor function and accessing subsets of a data frame with deep sea research data.
ISIT$NewMoth <- NA
ISIT$NewMoth[ISIT$Station <=5] <- "April"
ISIT$NewMoth[ISIT$Station > 5 & ISIT$Station<=11] <- "August"
ISIT$NewMoth[ISIT$Station > 11 & ISIT$Station<=15] <- "March"
ISIT$NewMoth[ISIT$Station >= 16 & ISIT$Station<=19] <- "October"
ISIT$NewMoth
ISIT$NewMoth <- factor(ISIT$NewMoth)

ISIT$NewYear <- NA
ISIT$NewYear[ISIT$Station <=11] <- 2001
ISIT$NewYear[ISIT$Station  >=12 & ISIT$Station<=19] <- 2002
ISIT$NewYear
ISIT$NewYear <- factor(ISIT$NewYear)
#Chapter4
#Exercise 1. The use of the tapply, sapply, and lapply functions to calculate mean temperature per month.
temp <- read.table(file = "Temperature.txt", header = TRUE)
mean <- matrix(nrow = 16, ncol = 12)
std <- matrix(nrow = 16, ncol = 12)
count <- matrix(nrow = 16, ncol = 12)
allyear <- unique(temp$Year)
for (i in 1:16) {
	year = allyear[i]
	val = temp[temp$Year == year,]
	mean[,i] = tapply(val$Temperature, 
			val$Month, mean,
			na.rm = TRUE)
	std[i,] = tapply(val$Temperature,
			val$Month, sd,
			na.rm = TRUE)
	count[i,] = tapply(val$Temperature,
			val$Month, length)
}
#Exercise 2. The use of the table function for the temperature data.
table(temp$Station)
table(temp$Year)
table(temp$Year, temp$Station)
#Chapter 5
#Exercise 1. Use of the plot function using terrestrial ecology data.
rk<- read.table('Amphibian_road_Kills.txt', header = TRUE)
rk
names(rk)
str(rk)
plot(x=rk$D_PARK,y=rk$TOT_N,
     xlab="Distance to park",
     ylab="Number of dead animals")
M.Loess <- loess(TOT_N~D_PARK, data =rk)
Fit <- fitted(M.Loess)
Ord1<- order(rk$D_PARK)
lines(rk$D_PARK[Ord1],Fit[Ord1], lwd=3,lty=2)

plot(x=rk$D_PARK,y=rk$TOT_N,
     cex = 0.5+3* rk$OLIVE/max(rk$OLIVE),
     xlab="Distance to park",
     ylab=" Number of dead animals")
