
#MUUTTUJAMUUNNOKSET REPLIKAATIO-ANALYYSIA VARTEN


#Aineiston lataus kahdesta erillisestä tiedostosta, ja tiedostojen yhdistäminen yhdeksi aineistoksi "BHPS2000"
if (!require("pacman")) install.packages("pacman")
pacman::p_load(haven, ggplot2,knitr, pander,descr,foreign,dplyr,Hmisc,psych,GPArotation,plyr, dplyr, rmarkdown)
library(tidyverse)
jindresp <- read_sav("/Applications/replikaatio/data/jindresp.sav")
jhhresp <- read_sav("/Applications/replikaatio/data/jhhresp.sav")
BHPS2000 <- merge(jindresp, jhhresp, by="JHID")

# Vain niiden muuttujien valitseminen aineistosta BHPS2000, joita Anand et al. käyttivät analyysissaan, ja BHPS_data - aineiston luominen. 
keep_columns <- c("JHLLT","JHSCANE","JHSCNTE","JLKMOVE", "JCARUSE", "JHSPRBQ","JQFEDHI","JGHQA","JGHQB", "JGHQE", "JGHQI", "JGHQJ", 
                  "JGHQD","JGHQF", "JGHQH", "JHSCANB", "JXPMOVE", "JHSCNTB", "JHSCAND", "JHSCNTD","JHSCANF","JHSCNTF","JGHQK", "JGHQC", 
                  "JGHQL", "JGHQG", "JVOTE7", "JHLENDW", "JHLLTWA", "JLFSATO", "JLFSAT1", "JLFSAT2", "JLFSAT3", "JLFSAT4", "JLFSAT5", 
                  "JLFSAT6", "JLFSAT7", "JLFSAT8")
BHPS_data <- dplyr::select(BHPS2000, one_of(keep_columns))
describe(BHPS_data)

#Muuttujien uudelleen nimeäminen ja koodaaminen Anand et. al.:in analyysin pohjalta
#Anand et al. eivät maininneet, mitä he tekivät puuttuville arvoille (-1 - (-7)) analyysissaan, mikä luonnollisesti heikentää tutkimuksen toistettavuutta.
#Oletan, että puuttuvat arvot ovat merkitty NA, eli non available - muotoon, joten uudelleen koodaan -1 -(-7) muotoon NA 

#TERVEYS: Being able to have good health – variable S_HL_LIMIT BHPS variable (JHLLT) and question –
#‘‘Does your health in any way limit your daily activities compared to most people of your age?’’ Yes coded as one. No coded as 0.
BHPS_data$S_HL_Limit <- BHPS_data$JHLLT
BHPS_data$S_HL_Limit[BHPS_data$S_HL_Limit < 0] <-NA
BHPS_data$S_HL_Limit[BHPS_data$S_HL_Limit == 1] <- 1
BHPS_data$S_HL_Limit[BHPS_data$S_HL_Limit == 2] <- 0

#RAVINTO: Being able to be adequately nourished – variable S_HL_NOURISH BHPS variable (JHSCANE) and question –‘‘Here is a list of things which people might have or do. 
#Please look at this card and tell me which things you (and your household) have or do? Eat meat, chicken, fish every second day.’’Yes coded as one.
#Those who answer no are asked (BHPS Variable JHSCNTE) –‘‘Would you like to be able to eat meat, chicken, fish at least every second day, 
#but must do without because you cannot afford it?’’No coded as one. Yes coded as 0.
BHPS_data$S_HL_NOURISH <- BHPS_data$JHSCANE
BHPS_data$S_HL_NOURISH[BHPS_data$S_HL_NOURISH < 0] <- NA
BHPS_data$S_HL_NOURISH[BHPS_data$JHSCNTE < 0] <- NA
BHPS_data$S_HL_NOURISH[BHPS_data$S_HL_NOURISH == 1] <- 1 
BHPS_data$S_HL_NOURISH[BHPS_data$S_HL_NOURISH == 2 & BHPS_data$JHSCNTE == 1] <- 0
BHPS_data$S_HL_NOURISH[BHPS_data$S_HL_NOURISH == 2 & BHPS_data$JHSCNTE == 2] <- 1

#MAHDOLLISUUS MUUTTAA. Being able to have . . . adequate shelter –variable S_H_LAC_MOVE BHPS variable (JLKMOVE) and question 
#‘‘If you could choose, would you stay here in your present home or would you prefer to move somewhere else?’’ ‘Stay here’ coded as 0. 
#For those answering ‘Prefer to move’ those answering to question (BHPS Variable XPMOVE) 
#‘‘(Even though you may not want to move) Do you expect you will move in the coming year?’’Yes are coded 0. No are coded one.
BHPS_data$S_H_LAC_MOVE <- BHPS_data$JLKMOVE
BHPS_data$S_H_LAC_MOVE[BHPS_data$S_H_LAC_MOVE< 0] <- NA
BHPS_data$S_H_LAC_MOVE[BHPS_data$S_H_LAC_MOVE == 1] <- 0
BHPS_data$JXPMOVE[BHPS_data$JXPMOVE < 0] <- NA
BHPS_data$S_H_LAC_MOVE[BHPS_data$S_H_LAC_MOVE == 2 & BHPS_data$JXPMOVE == 1] <- 0
BHPS_data$S_H_LAC_MOVE[BHPS_data$S_H_LAC_MOVE == 2 & BHPS_data$JXPMOVE == 2] <- 1

#MAHDOLLISUUS LIIKKUA.Bodily Integrity. Being able to move freely from place to place – variable S_S_CAR BHPS variable (JCARUSE ) 
#and question ‘‘Do you normally have access to a car or van that you can use whenever you want to?’’ Yes coded as one. No and ‘Don’t drive’ coded as 0.
BHPS_data$S_S_CAR <- BHPS_data$JCARUSE
BHPS_data$S_S_CAR[BHPS_data$S_S_CAR< 0] <- NA
BHPS_data$S_S_CAR[BHPS_data$S_S_CAR == 1] <- 1
BHPS_data$S_S_CAR[BHPS_data$S_S_CAR > 1] <- 0

#TURVALLISUUS. Being secure against violent assault – variable S_H_CRIME BHPS variable (JHSPRBQ) and question 
#‘‘Does your accommodation have any of the following problems? Vandalism or crime in the area’’ Yes coded as 0. No coded as 1.
BHPS_data$S_H_CRIME <- BHPS_data$JHSPRBQ
BHPS_data$S_H_CRIME[BHPS_data$S_H_CRIME< 0] <- NA
BHPS_data$S_H_CRIME[BHPS_data$S_H_CRIME == 1] <- 0
BHPS_data$S_H_CRIME[BHPS_data$S_H_CRIME == 2] <- 1

#KOULUTUS. Being able to imagine, think and reason, cultivated by an adequate education – variable S_S_EDUCATE BHPS variable (JQFEDHI) 
#is a derived variable giving the highest educational qualification. Those coded ‘A’ level and above are coded as one. The remainder are coded as 0.
BHPS_data$S_S_EDUCATE <- BHPS_data$JQFEDHI
BHPS_data$S_S_EDUCATE[BHPS_data$S_S_EDUCATE< 0] <- NA
BHPS_data$S_S_EDUCATE[BHPS_data$S_S_EDUCATE > 5] <- 1
BHPS_data$S_S_EDUCATE[BHPS_data$S_S_EDUCATE < 6] <- 0

#BHPS variable (JGHQA) and question‘‘Have you recently. . . been able to concentrate on whatever you’re doing?’’
#S_W_CONCB has value one for those answering ‘Better than usual’, 
#S_W_CONCL for those answering ‘Less than usual’ and 
#S_W_CONCML for those answering ‘Much less than usual’. The base is those answering ‘Same as usual’.
BHPS_data$S_W_CONCB <- BHPS_data$JGHQA[1]
BHPS_data$S_W_CONCL <- BHPS_data$JGHQA[3]
BHPS_data$S_W_CONCML <- BHPS_data$JGHQA[4]

#BHPS variable (JGHQB) and question ‘‘Have you recently. . .. lost much sleep over worry?’’ 
#S_W_SLEEPN has value one for those answering ‘Not at all’, 
#S_W_SLEEPM for those answering ‘Rather more than usual’, and 
#S_W_SLEEPMM for those answering ‘Much more than usual’. The base is those answering ‘No more than usual’.
BHPS_data$S_W_SLEEPN <- BHPS_data$JGHQB[1]
BHPS_data$S_W_SLEEPM <- BHPS_data$JGHQB[3]
BHPS_data$S_W_SLEEPMM <- BHPS_data$JGHQB[4]

#BHPS variable (JGHQE) and question ‘‘Have you recently. . .. felt constantly under strain?’’ 
#S_W_STRAINN has value one for those answering ‘Not at all’, 
#S_W_STRAINM for those answering ‘Rather more than usual’, and 
#S_W_STRAINMM for those answering ‘Much More than usual’. The base is those answering ‘No more than usual’.
BHPS_data$S_W_STRAINN <- BHPS_data$JGHQE[1]
BHPS_data$S_W_STRAINM <- BHPS_data$JGHQE[3]
BHPS_data$S_W_STRAINMM <- BHPS_data$JGHQE[4]

#BHPS variable (JGHQI) and question ‘‘Have you recently. . . been feeling unhappy or depressed?’’ 
#S_W_DEPRESSN has value one for those answering ‘Not at all’, 
#S_W_DEPRESSM for those answering ‘Rather more than usual’, and 
#S_W_DEPRESSMM for those answering ‘Much more than usual’. The base is those answering ‘No more than usual’
BHPS_data$S_W_DEPRESSN <- BHPS_data$JGHQI[1]
BHPS_data$S_W_DEPRESSM <- BHPS_data$JGHQI[3]
BHPS_data$S_W_DEPRESSMM <- BHPS_data$JGHQI[4]

#BHPS variable (JGHQJ) and question ‘‘Have you recently. . .been losing confidence in yourself?’’ 
#S_W_CONFIDENTN has value one for those answering ‘Not at all’, 
#S_W_CONFIDENTM for those answering ‘Rather more than usual’, and 
#S_W_CONFIDENTMM for those answering ‘Much more than usual’. The base is those answering ‘No more than usual’
BHPS_data$S_W_CONFIDENTN <- BHPS_data$JGHQJ[1]
BHPS_data$S_W_CONFIDENTM <- BHPS_data$JGHQJ[3]
BHPS_data$S_W_CONFIDENTMM <- BHPS_data$JGHQJ[4]

#BHPS variable (JGHQD) and question ‘‘Have you recently. . . felt capable of making decisions about things?’’
#S_W_DECIDEM has a value one for those answering ‘More so than usual’, 
#S_W_DECIDEL for those answering ‘Less so than usual’ and 
#S_W_DECIDEML for those answering ‘Much less capable than usual’. The base is those answering ‘Same as usual’
BHPS_data$S_W_DECIDEM <- BHPS_data$JGHQD[1]
BHPS_data$S_W_DECIDEL <- BHPS_data$JGHQD[3]
BHPS_data$S_W_DECIDEML <- BHPS_data$JGHQD[4]

#BHPS variable (JGHQF) and question ‘‘Have you recently... felt you couldn’t overcome your difficulties?’’ 
#S_W_DIFICULTN has a value one for those answering ‘Not at all’, 
#S_W_DIFICULTM for those answering ‘Rather more than usual’ and 
#S_W_DIFICULTMM for those answering ‘Much more than usual’. The base is those answering ‘No more than usual’
BHPS_data$S_W_DIFICULTN <- BHPS_data$JGHQF[1]
BHPS_data$S_W_DIFICULTM <- BHPS_data$JGHQF[3]
BHPS_data$S_W_DIFICULTMM <- BHPS_data$JGHQF[4]

#BHPS variable (JGHQH) and question ‘‘Have you recently. . . been able to face up to problems?’’ 
#S_W_FACEUPM has a value one for those answering ‘More so than usual’, 
#S_W_FACEUPL for those answering ‘Less so than usual’ and 
#S_W_FACEUPMML for those answering ‘Much less than usual’. The base is those answering ‘Same as usual’
BHPS_data$S_W_FACEUPM <- BHPS_data$JGHQH[1]
BHPS_data$S_W_FACEUPL <- BHPS_data$JGHQH[3]
BHPS_data$S_W_FACEUPMML <- BHPS_data$JGHQH[4]

#BHPS variable (JHSCANB) and question – ‘‘Here is a list of things which people might have or do. Please look at this card and tell me which things 
#you (and your household) have or do? Pay for a week’s annual holiday away from home.’’ S_S_HOLIDAY is coded as one for those answering yes. 
#Those who answer no are asked (BHPS Variable JHSCNTB) – ‘‘Would you like to be able to pay for a week’s annual holiday away from home, but must do 
#without because you cannot afford it?’’ S_S_HOLIDAY is coded as one for those answering No and 0 for those answering Yes.
BHPS_data$S_S_HOLIDAY <- BHPS_data$JHSCANB
BHPS_data$S_S_HOLIDAY[BHPS_data$S_S_HOLIDAY< 0] <- NA
BHPS_data$S_S_HOLIDAY[BHPS_data$S_S_HOLIDAY == 1] <- 0
BHPS_data$JHSCNTB[BHPS_data$JHSCNTB < 0] <- NA
BHPS_data$S_S_HOLIDAY[BHPS_data$S_S_HOLIDAY == 2 & BHPS_data$JHSCNTB == 1] <- 0
BHPS_data$S_S_HOLIDAY[BHPS_data$S_S_HOLIDAY == 2 & BHPS_data$JHSCNTB == 2] <- 1

#BHPS variable (JHSCAND) and question –‘‘Here is a list of things which people might have or do. Please look at this card and tell me which things you 
#(and your household) have or do? Buy new, rather than second hand, clothes.’’ S_S_CLOTHES is coded as one for those answering yes. 
#Those who answer no are asked (BHPS Variable JHSCNTD) – ‘‘Would you like to be able to buy new, rather than second hand, clothes, 
#but must do without because you cannot afford it?’’S_S_CLOTHES is coded as one for those answering ‘No’ and 0 for those answering ‘Yes’.
BHPS_data$S_S_CLOTHES <- BHPS_data$JHSCAND
BHPS_data$S_S_CLOTHES[BHPS_data$S_S_CLOTHES< 0] <- NA
BHPS_data$S_S_CLOTHES[BHPS_data$S_S_CLOTHES == 1] <- 0
BHPS_data$JHSCNTD[BHPS_data$JHSCNTD < 0] <- NA
BHPS_data$S_S_CLOTHES[BHPS_data$S_S_CLOTHES == 2 & BHPS_data$JHSCNTD == 1] <- 0
BHPS_data$S_S_CLOTHES[BHPS_data$S_S_CLOTHES == 2 & BHPS_data$JHSCNTD == 2] <- 1

#BHPS variable (JHSCANF) and question – ‘‘Here is a list of things which people might have or do.
#Please look at this card and tell me which things you (and your household) have or do? Have friends or family for a drink or meal at least once a month’’.
#S_S_MEAL is coded as one for those answering yes. Those who answer no are asked 
#(BHPS Variable JHSCNTF) –‘‘Would you like to be able to have friends or family for a drink or meal at least once a month, but must do without because you cannot afford it?’’
#S_S_MEAL is coded as one for those answering ‘No’ and 0 for those answering ‘Yes’.
BHPS_data$S_S_MEAL <- BHPS_data$JHSCANF
BHPS_data$S_S_MEAL[BHPS_data$S_S_MEAL< 0] <- NA
BHPS_data$S_S_MEAL[BHPS_data$S_S_MEAL == 1] <- 0
BHPS_data$JHSCNTF[BHPS_data$JHSCNTF < 0] <- NA
BHPS_data$S_S_MEAL[BHPS_data$S_S_MEAL == 2 & BHPS_data$JHSCNTF == 1] <- 0
BHPS_data$S_S_MEAL[BHPS_data$S_S_MEAL == 2 & BHPS_data$JHSCNTF == 2] <- 1

#KOKEMUS HYÖDYLLISYYDESTÄ. BHPS variable (JGHQK) and question ‘‘Have you recently. . . been thinking of yourself as a worthless person?
#S_W_WORTHN has a value one for those answering ‘Not at all’, 
#S_W_WORTHM for those answering ‘Rather more than usual’, 
#S_W_WORTHMM for those answering ‘Much more than usual’. The base is those answering ‘No more than usual’
BHPS_data$S_W_WORTHN <- BHPS_data$JGHQK[1]
BHPS_data$S_W_WORTHM <- BHPS_data$JGHQK[3]
BHPS_data$S_W_WORTHMM <- BHPS_data$JGHQK[4]

#KORVAAMATTOMUUS. BHPS variable (JGHQC) and question –‘‘Have you recently... felt that you were playing a useful part in things?’’
#S_W_ROLEM has a value one for those answering ‘More than usual’, 
#S_W_ROLEL for those answering ‘Less so than usual’ and 
#S_W_ROLEML for those answering ‘Much less than usual’. The base is those answering ‘Same as usual’
BHPS_data$S_W_ROLEM <- BHPS_data$JGHQC[1]
BHPS_data$S_W_ROLEL <- BHPS_data$JGHQC[3]
BHPS_data$S_W_ROLEML <- BHPS_data$JGHQC[4]

#ONNELLISUUS BHPS variable (JGHQL) and question –‘‘Have you recently.... been feeling reasonably happy, all things considered??’’
#S_W_HAPPYM has a value one for those answering ‘More so than usual’, 
#S_W_HAPPYL for those answering ‘Less so than usual’ and 
#S_W_HAPPYML for those answering ‘Much less than usual’. The base is those answering ‘Same as usual’
BHPS_data$S_W_HAPPYM <- BHPS_data$JGHQL[1]
BHPS_data$S_W_HAPPYL <- BHPS_data$JGHQL[3]
BHPS_data$S_W_HAPPYML <- BHPS_data$JGHQL[4]

#ELÄMÄSTÄ NAUTTIMINEN. BHPS variable (JGHQG) and question – ‘‘Have you recently. . . been able to enjoy your normal day- to-day activities?’’
#S_W_EACTIVEM has a value one for those answering ‘More so than usual’, 
#S_W_EACTIVEL for those answering ‘Less so than usual’ and
#S_W_EACTIVEML for those answering ‘Much less than usual’. The base is those answering ‘Same as usual’.
BHPS_data$S_W_EACTIVEM <- BHPS_data$JGHQG[1]
BHPS_data$S_W_EACTIVEL <- BHPS_data$JGHQG[3]
BHPS_data$S_W_EACTIVEML <- BHPS_data$JGHQG[4]

#ÄÄNESTÄMINEN.Being able to participate effectively in political choices – variable S_VOTE BHPS variable (JVOTE7) and question –
#‘‘Did you vote in this (past) year’s general election?’’ Those who couldn’t vote are coded one others are coded 0.
BHPS_data$S_VOTE <- BHPS_data$JVOTE7
BHPS_data$S_VOTE[BHPS_data$S_VOTE< 0] <- NA
BHPS_data$S_VOTE[BHPS_data$S_VOTE < 3] <- 0
BHPS_data$S_VOTE[BHPS_data$S_VOTE == 3] <- 1

#Having the right to seek employment on an equal basis –BHPS variable (JHLENDW) and question –‘‘Does your health keep you from doing some types of work?’’ and 
#BHPS variable (JHLLTWA) and question –‘‘For work you can do, how much does your health limit the amount of work you can do?’’
#S_HL_PWORK is coded as one for those answering Yes’ to JHLENDW and 
#S_HL_NAWORK for those answering ‘Can do nothing’. The base is those answering ‘No’.
#S_HL_AWORKL is coded as one for those answering ‘A lot’ to JHLLTWA, 
#S_HLAWORKLTL for those answering ‘Just a little’,
#S_HLAWORKS for those answering ‘Somewhat’. The base is those answering ‘Not at all’.
BHPS_data$S_HL_PWORK <- BHPS_data$JHLENDW[1]
BHPS_data$S_HL_NAWORK <- BHPS_data$JHLENDW[3]
BHPS_data$S_HL_AWORKL <- BHPS_data$JHLLTWA[1]
BHPS_data$S_HLAWORKLTL <- BHPS_data$JHLLTWA[3]
BHPS_data$S_HLAWORKS <- BHPS_data$JHLLTWA[4]

#TYYTYVÄISYYS ELÄMÄÄN KOKONAISUUTENA. BHPS variable (JLFSATO) and question
#‘‘How dissatisfied or satisfied are you with your life overall?’’ S_OALL coded 1 -7
BHPS_data$S_OALL <- BHPS_data$JLFSATO
BHPS_data$S_OALL[BHPS_data$S_OALL < 0] <- NA

#TYYTYVÄISYYS TERVEYTEEN. BHPS variable JLFSAT1 ) and question
#‘‘How dissatisfied or satisfied are you with your health?’’ S_HEALTH coded 1 = Not satisfied at all – 7 = Completely satisfied
BHPS_data$S_HEALTH <- BHPS_data$JLFSAT1
BHPS_data$S_HEALTH[BHPS_data$S_HEALTH < 0] <- NA

#TYYTYVÄISYYS TULOIHIN. JBHPS variable (JLFSAT2) and question
#‘‘How dissatisfied or satisfied are you with the income of your household?’’S_H_INCOME coded 1 = Not satisfied at all – 7 = Completely satisfied
BHPS_data$JLFSAT2[BHPS_data$JLFSAT2 < 0] <- NA
BHPS_data$S_H_INCOME <- BHPS_data$JLFSAT2

#TYYTYVÄISYYS ASUMISEEN. BHPS variable (JLFSAT3) and question
#‘‘How dissatisfied or satisfied are you with your house/flat?’’ S_HOUSE coded 1 = Not satisfied at all – 7 = Completely satisfied
BHPS_data$S_HOUSE <- BHPS_data$JLFSAT3
BHPS_data$S_HOUSE[BHPS_data$S_HOUSE< 0] <- NA

#TYYTYVÄISYYS PUOLISOON. BHPS variable (JLFSAT4) and question‘‘How dissatisfied or satisfied are 
#you with your husband/ wife/partner?’’S_PARTNER2 coded 0 = no partner, 1 = Not satisfied atall – 7 = Completely satisfied
BHPS_data$S_PARTNER2 <- BHPS_data$JLFSAT4
BHPS_data$S_PARTNER2[BHPS_data$S_PARTNER2< 0] <- NA

#TYYTYVÄISYYS TYÖHÖN. BHPS variable (JLFSAT5) and question
#‘‘How dissatisfied or satisfied are you with your job?’’ S_JOB2 coded 0 = no job, 1 = Not satisfied at all –7 = Completely satisfied
BHPS_data$S_JOB2 <- BHPS_data$JLFSAT5
BHPS_data$S_JOB2[BHPS_data$S_JOB2< 0] <- NA

#TYYTYVÄISYYS SOSIAALIELÄMÄÄN. BHPS variable (JLFSAT6) and question
#‘‘How dissatisfied or satisfied are you with your social life?’’ S_SOCIAL coded 1 = Not satisfied at all – 7 = Completelysatisfied
BHPS_data$S_SOCIAL <- BHPS_data$JLFSAT6
BHPS_data$S_SOCIAL[BHPS_data$S_SOCIAL< 0] <- NA
  
#TYYTYVÄISYYS VAPAA-AJAN MÄÄRÄÄN. BHPS variable (JLFSAT7) and question
#‘‘How dissatisfied or satisfied are you with the amount of leisure time you have?’’S_QLEISURE coded 1 = Not satisfied at all – 7 = Completely satisfied
BHPS_data$S_QLEISURE <- BHPS_data$JLFSAT7
BHPS_data$S_QLEISURE[BHPS_data$S_QLEISURE< 0] <- NA

#TYYTYVÄISYYS VAPAA-AJAN VIETTOTAPOHIN. BHPS variable (JLFSAT8) and question 
#‘‘How dissatisfied or satisfied are you with the way youspend your leisure?’’ S_LEISURE 1 = Not satisfied at all – 7 = Completelysatisfied
BHPS_data$S_LEISURE <- BHPS_data$JLFSAT8
BHPS_data$S_LEISURE[BHPS_data$S_LEISURE< 0]<- NA


write.csv(BHPS_data, file = "/Applications/replikaatio/data/BHPS_data.csv")
BHPS_data <- read.csv(file = "/Applications/replikaatio/data/BHPS_data.csv", sep=",", header =TRUE)
summary(BHPS_data)




