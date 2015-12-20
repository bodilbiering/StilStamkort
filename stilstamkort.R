#read data
data <- read.csv("data/Stilquiz-report.csv")
#clean data

library("lubridate")
navn <- paste(data[,"Fornavn"], data[,"Efternavn"]) #spm 1+2
email <- data[,4] #spm 3
alder <- sapply(data[,5], function(x) return(year(Sys.Date()) -x)) #spm 4
job <- data[, 134] #spm 45
bhStr <- paste(data[,6], data[,7]) #5+6
tidydata <- data.frame(navn, email, alder, job, bhStr)

#højde og vægt, spm 10 + 11
højde <- data[, 38]
vægt <- data[,39]
tidydata <- cbind(tidydata, højde, vægt)

#functions
myprint <- function(r) {r2 <- r[!is.na(r)]; r2 <- r2[r2 != ""]; paste(r2, collapse = ", ")}
myprintRes <- function(f) {unlist(apply(f,1, myprint))}
trim <- function(x) {if(is.na(x) || x == "") return("[NA]") else return(x)}

#kjole, spm 7
kjole <- data.frame(data[, 8:17], stringsAsFactors = T) 
tidydata <- cbind(tidydata, kjole = myprintRes(kjole))

#overdel, spm 8
overdel <- data.frame(data[, 18:27], stringsAsFactors = T)
tidydata <- cbind(tidydata, overdel = myprintRes(overdel))

#underdel, spm 9
underdel <- data.frame(data[, 28:37], stringsAsFactors = T)
tidydata <- cbind(tidydata, underdel = myprintRes(underdel))

#kropsform spm 12-14
tidydata <- cbind(tidydata, skuldre = data[,40])
tidydata <- cbind(tidydata, ben = data[,41])
tidydata <- cbind(tidydata, særligt = data[,42])

#make styleDNA card, billeder spm 15-32 og kommentar til billeder spm 33
#billeder starter ved 15 i stilquiz, kolonne 43 i data

#afslappet = 15, 20, 24
#boheme = 28, 29, 31
#enkel = 16, 17, 32
#feminin = 19, 21, 23
#klassisk = 22, 25, 26
#rå = 18, 27, 30

afslappet <- c(15, 20, 24) + 28
boheme <- c(28, 29, 31) + 28
enkel <- c(16, 17, 32) + 28
feminin <- c(19, 21, 23) + 28
klassisk <- c(22, 25, 26) + 28
rå <- c(18, 27, 30) + 28

#translate
trans <- function(t) {
    if(t == "Hader det!"|| t == "" || t == "Det er slet ikke mig!") {return(0)}
    else if(t == "Jeg er ikke så vild med det.") {return(0.5)}
    else if(t == "Hm, måske.") {return(1)}
    else if(t == "Det er lige mig.") {return(2)}
    else return("fejl!")
}

a <- apply(data[, afslappet], 1, function(x) sum(sapply(x, trans)))
b <- apply(data[, boheme], 1, function(x) sum(sapply(x, trans)))
e <- apply(data[, enkel], 1, function(x) sum(sapply(x, trans)))
f <- apply(data[, feminin], 1, function(x) sum(sapply(x, trans)))
k <- apply(data[, klassisk], 1, function(x) sum(sapply(x, trans)))
r <- apply(data[, rå], 1, function(x) sum(sapply(x, trans)))

tidydata <- cbind(tidydata, Afslappet = unlist(a))
tidydata <- cbind(tidydata, Boheme = unlist(b))
tidydata <- cbind(tidydata, Enkel = unlist(e))
tidydata <- cbind(tidydata, Feminin = unlist(f))
tidydata <- cbind(tidydata, Klassisk = unlist(k))
tidydata <- cbind(tidydata, Rå = unlist(r))

tidydata <- cbind(tidydata, billedKommentar = data[,61])

#Det tøj jeg har det bedst i row 138 eller billede, spm 48 a= foto og b= tekst
tidydata <- cbind(tidydata, favorittøj = data[,138])

#Mærker i skabet: spm 34
mærker <- data.frame(data[, 62:88], stringsAsFactors = T)
tidydata <- cbind(tidydata, mærker = myprintRes(mærker))

#Eventyrlyst: spm 35
tidydata <- cbind(tidydata, eventyrlyst = data[,89])

#UNDGÅ: spm 36-38 + 44
undgåMaterialer <- data.frame(data[, 90:94], stringsAsFactors = T) 
tidydata <- cbind(tidydata, undgåMaterialer = myprintRes(undgåMaterialer))

undgåFarver <- data.frame(data[, 95:111], stringsAsFactors = T) 
tidydata <- cbind(tidydata, undgåFarver = myprintRes(undgåFarver))

undgåPrint <- data.frame(data[, 112:117], stringsAsFactors = T) 
tidydata <- cbind(tidydata, undgåPrint = myprintRes(undgåPrint))

tidydata <- cbind(tidydata, aldrig = data[,133])

#Kjoler eller bukser: spm43
tidydata <- cbind(tidydata, kjolerBukser = data[,132])

#Dresscode: spm 46
tidydata <- cbind(tidydata, dresscode = data[,135])

#Budget (pr md (tidligere år) uden sko): spm 47
tidydata <- cbind(tidydata, budget = data[,136])

#Hvordan skal tøjet sidde

#spm 39-42.
tidydata <- cbind(tidydata, overkropFit = data[,118])
tidydata <- cbind(tidydata, underkropFit = data[,119])

vise <- data.frame(data[, 120:125], stringsAsFactors = T) 
tidydata <- cbind(tidydata, vise = myprintRes(vise))

dække <- data.frame(data[, 126:131], stringsAsFactors = T) 
tidydata <- cbind(tidydata, dække = myprintRes(dække))

#pinterest, spm 50
tidydata <- cbind(tidydata, pinterest = data[,140])
#kommentar, spm 51
tidydata <- cbind(tidydata, slutkommentar = data[,141])

