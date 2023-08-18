#### Setup #### 
library(dplyr)
library(scales)
library(ggplot2)
library(viridis)
library(Polychrome)
library(data.table)
library(RColorBrewer)
#### End #### 

runEnrollment <- function(year1, level1, hideLevel){
  
  hd.file <- paste("hd", year1, ".csv", sep="")
  effy.file <- paste("effy", year1, ".csv", sep="")
  
  if(year1=="2018"){effy.file <- "effy2018_rv.csv"}
  if(year1=="2019"){effy.file <- "effy2019_rv.csv"}
  
  if(year1=="2021"){
    hd <- fread(hd.file, header=TRUE, select=c(
      "UNITID", 
      "INSTNM", 
      "STABBR",
      "CONTROL", 
      "C21BASIC"
    )) %>% filter(`CONTROL`==1)
  }else{
    hd <- fread(hd.file, header=TRUE, select=c(
      "UNITID", 
      "INSTNM", 
      "STABBR",
      "CONTROL", 
      "C18BASIC"
    )) %>% filter(`CONTROL`==1) %>% rename(C21BASIC = C18BASIC)
  }
  
  hd$STABBR[hd$UNITID==105297] <- "NM" # Adjustment for Dine College
  
  if(year1 %in% c("2021", "2020")){
    effy <- fread(effy.file, header=TRUE, select=c(
      "UNITID",
      "EFFYALEV", 
      "EFYTOTLT"
    )) %>% filter(EFFYALEV==level1) 
  }else{
    effy <- fread(effy.file, header=TRUE, select=c(
      "UNITID",
      "EFFYLEV", 
      "EFYTOTLT"
    )) %>% filter(EFFYLEV==level1) %>% rename(EFFYALEV = EFFYLEV)
  }
  
  if(hideLevel==TRUE){effy <- effy %>% select(`UNITID`, `EFYTOTLT`)}
  
  data1 <- left_join(x=hd, y=effy, by="UNITID")

  Arkansas <- data1 %>% filter(`STABBR`=="AR") %>% filter(`C21BASIC` %in% (1:14))

  California <- data1 %>% filter(`STABBR`=="CA") %>% filter(`C21BASIC` %in% (1:14))

  Hawaii <- data1 %>% filter(`STABBR`=="HI") %>% filter(`C21BASIC` %in% (1:14))

  Kentucky <- data1 %>% filter(`STABBR`=="KY") %>% filter(`C21BASIC` %in% (1:14))

  Maryland <- data1 %>% filter(`STABBR`=="MD") %>% filter(`C21BASIC` %in% (1:14))

  Michigan <- data1 %>% filter(`STABBR`=="MI") %>% filter(`C21BASIC` %in% (1:14))

  Missouri <- data1 %>% filter(`STABBR`=="MO") %>% filter(`C21BASIC` %in% (1:14))

  Nevada <- data1 %>% filter(`STABBR`=="NV") %>% filter((`C21BASIC` %in% (1:14)) | (`UNITID`==182306))

  NewJersey <- data1 %>% filter(`STABBR`=="NJ") %>% filter(`C21BASIC` %in% (1:14))

  NewMexico <- data1 %>% filter((`STABBR`=="NM") | (`UNITID`==105297)) %>% filter(`C21BASIC` %in% (1:14) | (`UNITID` %in% c(
    105297, # Dine College
    187596  # Navajo Technical University
  )))

  Oklahoma <- data1 %>% filter(`STABBR`=="OK") %>% filter(`C21BASIC` %in% (1:14))

  Oregon <- data1 %>% filter(`STABBR`=="OR") %>% filter(`C21BASIC` %in% (1:14))

  RhodeIsland <- data1 %>% filter(`STABBR`=="RI") %>% filter(`C21BASIC` %in% (1:14))

  Tennessee <- data1 %>% filter(`STABBR`=="TN") %>% filter(`C21BASIC` %in% (1:14))

  Washington <- data1 %>% filter(`STABBR`=="WA") %>% filter(`C21BASIC` %in% (1:14))

  WestVirginia <- data1 %>% filter(`STABBR`=="WV") %>% filter(`C21BASIC` %in% (1:14) | (`UNITID`==237686)) %>% filter(`UNITID` != 430795)

  data1 <- rbind(Arkansas, California, Hawaii, Kentucky, Maryland, Michigan, Missouri, Nevada, NewJersey, NewMexico, Oklahoma, Oregon, RhodeIsland, Tennessee, Washington, WestVirginia)
  
  data1$Year <- rep(year1, nrow(data1))
  
  return(data1) 
}

#### Run enrollment numbers #### 

enrollment <- rbind(runEnrollment("2018", 2, TRUE), runEnrollment("2019", 2, TRUE), runEnrollment("2020", 3, TRUE), runEnrollment("2021", 3, TRUE))
enrollment <- aggregate(data=enrollment, EFYTOTLT ~ STABBR + Year, FUN=sum)

dcs2021 <- runEnrollment("2021", 3, FALSE)
ovr2021 <- runEnrollment("2021", 2, FALSE)

dcs2020 <- runEnrollment("2020", 3, FALSE)
ovr2020 <- runEnrollment("2020", 2, FALSE)

dcs <- rbind(dcs2021, dcs2020)
ovr <- rbind(ovr2021, ovr2020)

dcs.ovr <- aggregate(data=rbind(dcs, ovr), EFYTOTLT ~ STABBR + EFFYALEV, FUN=sum) 
dcs.ovr <- dcs.ovr %>% pivot_wider(names_from=EFFYALEV, values_from=EFYTOTLT)
dcs.ovr <- dcs.ovr %>% mutate(`DCS Share` = `3` / `2`)
dcs.ovr.shares <- dcs.ovr %>% select(`STABBR`, `DCS Share`)

enrollment <- left_join(x=enrollment, y=dcs.ovr.shares, by="STABBR")
enrollment <- enrollment %>% mutate(`EFYTOTLT.adj` = ifelse(`Year` %in% c("2018", "2019"), `EFYTOTLT` * `DCS Share`, `EFYTOTLT`))

enrollment <- enrollment %>% select(`STABBR`, `Year`, `EFYTOTLT.adj`) %>% pivot_wider(names_from=`Year`, values_from=`EFYTOTLT.adj`)

enrollment$`2018` <- round(enrollment$`2018`, 0)
enrollment$`2019` <- round(enrollment$`2019`, 0)
enrollment$`2020` <- round(enrollment$`2020`, 0)
enrollment$`2021` <- round(enrollment$`2021`, 0)

#### End #### 

#### State-level participation rates ####

# AR
participationAR <- sum(
  467,
  564,
  543,
  604
) / sum(
  44800,
  43563,
  43189,
  38859
)

# CA
participationCA <- sum(
  973748,
  945168,	
  920426,	
  800122
) / sum(
  1894238,
  1901709,
  1879074,
  1735712
)

# HI
participationHI <- sum(
  1443,
  1378,
  1711,
  1800
) / sum(
  22053,
  21487,
  21965,
  21175
)

# KY
participationKY <- sum(
  3063,
  3033
) / sum(
  66763,
  60831
)

# MD
participationMD <- sum(
  966,
  2025
) / sum(
  126654,
  119789
)

# MI
participationMI <- 17085 / 170615

# MO
participationMO <- sum(
  12865,
  13039,
  13493,
  14809
) / sum(
  95557,
  91678,
  87321,
  80436
)

# NV
participationNV <- sum(
  812,
  1130,
  1395
) / sum(
  59358,
  57236,
  53075
)

# NJ
participationNJ <- sum(
  10313,
  12855
) / sum(
  151683,
  134115
)

# NM
participationNM <- 3011 / 59128

# OK
participationOK <- sum(
  5381,
  4622,
  4607,
  4307
) / sum(
  66737,
  64943,
  61845,
  59706
)

# OR
participationOR <- sum(
  9457,
  10529,
  11625,
  8949
) / sum(
  111739,
  108029,
  98238,
  89661
)

# RI
participationRI <- sum(
  1567,
  1627
) / sum(
  17437,
  15902
)

# TN
participationTN <- 42870 / 85157

# WA
participationWA <- 55203 / 112610

# WV
participationWV <- sum(
  1168,
  1431
) / sum(
  15628,
  13331
)

participation <- data.frame("State" = c(
  "AR", 
  "CA", 
  "HI", 
  "KY", 
  "MD", 
  "MI", 
  "MO",
  "NV", 
  "NJ", 
  "NM",
  "OK", 
  "OR", 
  "RI", 
  "TN", 
  "WA", 
  "WV"
), "ParticipationRate"=c(
  participationAR, 
  participationCA, 
  participationHI, 
  participationKY, 
  participationMD, 
  participationMI, 
  participationMO,
  participationNV, 
  participationNJ, 
  participationNM,
  participationOK, 
  participationOR, 
  participationRI, 
  participationTN, 
  participationWA, 
  participationWV
))

participation$ParticipationRateShort <- percent(participation$ParticipationRate, accuracy=0.1)

#### End #### 

#### Calculating the aggregate participation rate #### 
sum(
  # Numerators: 
  604,    # AR num
  800122, # CA num
  1800,   # HI num
  3033,   # KY num
  2025,   # MD num
  17085,  # MI num 
  14809,  # MO num 
  1395,   # NV num
  12855,  # NJ num
  3011,   # NM num 
  4307,   # OK num
  8949,   # OR num
  1627,   # RI num
  42870,  # TN num 
  55203,  # WA num
  1431    # WV num
) / sum(
  
  # Denominators
  38859,   # AR den
  1735712, # CA den
  21175,   # HI den
  60831,   # KY den
  119789,  # MD den
  170615,  # MI den
  80436,   # MO den
  53075,   # NV den
  134115,  # NJ den
  59128,   # NM den
  59706,   # OK den
  89661,   # OR den
  15902,   # RI den
  85157,   # TN den
  112610,  # WA den
  13331    # WV den
)

# If california is removed ... 
sum(
  # Numerators: 
  604,    # AR num
  1800,   # HI num
  3033,   # KY num
  2025,   # MD num
  17085,  # MI num 
  14809,  # MO num 
  1395,   # NV num
  12855,  # NJ num
  3011,   # NM num 
  4307,   # OK num
  8949,   # OR num
  1627,   # RI num
  42870,  # TN num 
  55203,  # WA num
  1431    # WV num
) / sum(
  
  # Denominators
  38859,   # AR den
  21175,   # HI den
  60831,   # KY den
  119789,  # MD den
  170615,  # MI den
  80436,   # MO den
  53075,   # NV den
  134115,  # NJ den
  59128,   # NM den
  59706,   # OK den
  89661,   # OR den
  15902,   # RI den
  85157,   # TN den
  112610,  # WA den
  13331    # WV den
)

#### End #### 

#### Average annual spending ####
avgAR <- mean(c(
  501794,
  1165626,
  1589304,
  1975052
))
avgCA <- mean(c(
  764271303,
  757296824,
  723007790,
  654267518
))
avgHI <- mean(c(
  1730774,
  1640463,
  2400496,
  2932719
))
avgKY <- mean(c(
  7645397,
  7979655
))
avgMD <- mean(c(
  3090362,
  6301319
))
avgMI <- 14166236
avgMO <- mean(c(
  36326413,
  38956869,
  42873454,
  50040693
))
avgNV <- mean(c(
  1756610,
  2386065, 
  2794157
))
avgNJ <- mean(c(
  19448999,
  26412180
))
avgNM <- 2272050
avgOK <- mean(c(
  22382000,
  19836000,
  20040000,
  18618000
))
avgOR <- mean(c(
  14525158,
  17594633,
  19932280,
  14650507
))
avgRI <- mean(c(
  6814513,
  7030366
))
avgTN <- 64434242
avgWA <- 125533548
avgWV <- mean(c(
  3269353,
  4139513
))

averageFunding <- data.frame("State" = c(
  "AR", 
  "CA", 
  "HI", 
  "KY", 
  "MD", 
  "MI", 
  "MO",
  "NV", 
  "NJ", 
  "NM",
  "OK", 
  "OR", 
  "RI", 
  "TN", 
  "WA", 
  "WV"
), "AnnualSpending"=c(
  avgAR, 
  avgCA, 
  avgHI, 
  avgKY, 
  avgMD, 
  avgMI, 
  avgMO,
  avgNV, 
  avgNJ, 
  avgNM,
  avgOK, 
  avgOR, 
  avgRI, 
  avgTN, 
  avgWA, 
  avgWV
))

averageFunding$AnnualSpendingFormatted <- dollar(averageFunding$AnnualSpending, accuracy=1)
#### End #### 

#### Figure 1 #### 
participants <- participation %>% arrange(desc(`ParticipationRate`)) 

statecols <- rep(c('#855C75', '#D9AF6B', '#AF6458', '#736F4C', '#526A83', '#625377', '#68855C'), 3)

participants$`Program` <- rep(NA, nrow(participants))
participants$`Program`[participants$`State`=="AR"] <- "Arkansas Future Grant"
participants$`Program`[participants$`State`=="CA"] <- "CA College Promise Program & Promise Grant"
participants$`Program`[participants$`State`=="HI"] <- "Hawai'i Promise"
participants$`Program`[participants$`State`=="KY"] <- "Work Ready Kentucky Scholarship"
participants$`Program`[participants$`State`=="MD"] <- "Maryland Community College Promise"
participants$`Program`[participants$`State`=="MI"] <- "Michigan Reconnect"
participants$`Program`[participants$`State`=="MO"] <- "Missouri A+ Scholarship"
participants$`Program`[participants$`State`=="NV"] <- "Nevada Promise"
participants$`Program`[participants$`State`=="NJ"] <- "NJ Community College Opportunity Grant"
participants$`Program`[participants$`State`=="NM"] <- "New Mexico Opportunity Grant"
participants$`Program`[participants$`State`=="OK"] <- "Oklahoma's Promise"
participants$`Program`[participants$`State`=="OR"] <- "Oregon Promise"
participants$`Program`[participants$`State`=="RI"] <- "Rhode Island Promise"
participants$`Program`[participants$`State`=="TN"] <- "Tennessee Promise & Reconnect"
participants$`Program`[participants$`State`=="WA"] <- "WA College Grant & College Bound"
participants$`Program`[participants$`State`=="WV"] <- "West Virginia Invests Grant"

ggplot(data=participants, mapping=aes(x=`ParticipationRate`, y=reorder(`Program`, `ParticipationRate`), fill=`State`)) + geom_bar(stat="identity") + scale_x_continuous(labels=scales::percent_format(accuracy=1)) + labs(x="Share of Community College Students Receiving State Promise Grant", y="") + scale_fill_manual(values=statecols) + theme(legend.position = "none")
#### End #### 

#### AGI data ####
AGI <- read.csv("PowerStats_lctycr.csv", skip=11, header=FALSE, nrow=7) %>% select(V1, V2)
AGI.tuition <- read.csv("PowerStats_lctycr.csv", skip=9, header=FALSE, nrow=1) %>% select(V3)
AGI.tuition.value <- AGI.tuition$V3[1]
AGI$V2 <- gsub("!", "", AGI$V2)
AGI$V2 <- gsub(" ", "", AGI$V2)
AGI$V2 <- as.numeric(AGI$V2)
AGI$V2[is.na(AGI$V2)] <- 0
names(AGI) <- c("AGI Group1", "Pell Grant")

income <- data.frame("AGI Group1"=c(
  "0 <= X <= 15000", 
  "15001 <= X <= 30000", 
  "30001 <= X <= 45000",
  "45001 <= X <= 60000", 
  "60001 <= X <= 90000", 
  "90001 <= X <= 120000", 
  "120001 <= X <= 2500000"
), "AGI Group"=c(
  "$0 to $15K", 
  "$15K to $30K", 
  "$30K to $45K", 
  "$45K to $60K", 
  "$60K to $90K", 
  "$90K to $120K", 
  "More than $120K"
))
names(income) <- c("AGI Group1", "AGI Group")
AGI <- left_join(x=AGI, y=income, by="AGI Group1")
AGI <- AGI %>% select(`AGI Group`, `Pell Grant`)

AGI <- AGI %>% mutate(`Last-Dollar Promise` = AGI.tuition.value - `Pell Grant`)
AGI$`Last-Dollar Promise`[AGI$`Last-Dollar Promise` < 0] <- 0
AGI <- AGI %>% mutate(`First-Dollar Promise` = AGI.tuition.value)
AGI <- reshape2::melt(data=AGI, id.vars=c("AGI Group"))
names(AGI) <- c("AGI Group", "Type", "Amount")

# Inflation adjustment 
# January 2020 to July 2023 
# Adjustment factor: 1.1849820

AGI$Amount <- AGI$Amount * 1.1849820
AGI.tuition.value <- AGI.tuition.value * 1.1849820
#### End #### 

#### Figure 2 #### 
statecols2A <- c('#D9AF6B', '#526A83')

AGI1 <- AGI %>% filter(`Type` %in% c("Pell Grant", "Last-Dollar Promise"))
AGI1$Type <- factor(AGI1$Type, levels=c("Last-Dollar Promise", "Pell Grant"))
ggplot(data=AGI1, mapping=aes(x=`AGI Group`, y=`Amount`, fill=`Type`)) + geom_bar(stat="identity", position="stack") + geom_hline(yintercept=AGI.tuition.value, linetype="dashed") + geom_text(x=6.5, y=4500, label="Dashed line indicates average tuition", color='gray45', size=2.8) + scale_fill_manual(values=statecols2A) + scale_y_continuous(labels=scales::dollar_format(accuracy=1), limits=c(0, 9200)) + theme(legend.title=element_blank())
#### End #### 

#### Figure 3 ####
statecols2B <- c('#526A83', 'lightsalmon3')

AGI2 <- AGI %>% filter(`Type` %in% c("Pell Grant", "First-Dollar Promise"))
AGI2$Type <- factor(AGI2$Type, levels=rev(c("First-Dollar Promise", "Pell Grant")))
ggplot(data=AGI2, mapping=aes(x=`AGI Group`, y=`Amount`, fill=`Type`)) + geom_bar(stat="identity", position="stack") + geom_hline(yintercept=AGI.tuition.value, linetype="dashed") + geom_text(x=6.5, y=4500, label="Dashed line indicates average tuition", color='gray45', size=2.8) + scale_fill_manual(values=statecols2B) + scale_y_continuous(labels=scales::dollar_format(accuracy=1), limits=c(0, 9200)) + theme(legend.title=element_blank())
#### End #### 

#### Figure 4 #### 
npsas <- read.csv("NPSAS ACP Total Grants.csv", header=TRUE)

# Aggregate the increase in Figure 4 
total4 <- sum(npsas$Value[(npsas$Group=="All promise-eligible students") & (npsas$Measure=="Estimated number of students")])
agg4A <- npsas %>% filter(Measure=="Estimated number of students") %>% filter(Group=="All promise-eligible students")
agg4A$`Share of total` <- agg4A$`Value` / total4
agg4A <- agg4A %>% select(`Program`, `Share of total`)
agg4B <- npsas %>% filter(Measure != "Estimated number of students") %>% filter(Group=="All promise-eligible students")
agg4B <- left_join(x=agg4B, y=agg4A, by="Program")
agg4B$`Weighted value` <- agg4B$Value * agg4B$`Share of total`
agg4C <- aggregate(data=agg4B, `Weighted value` ~ Measure, FUN=sum)
names(agg4C) <- c("Measure", "Value")
agg4C$`Program` <- rep("Aggregated", nrow(agg4C))
agg4C$`Group` <- rep("All promise-eligible students", nrow(agg4C))
agg4C <- agg4C %>% select(`Program`, `Measure`, `Group`, `Value`)

# Figures 4: All Promise Eligible Students 
fig4 <- npsas %>% filter(`Group` == "All promise-eligible students") %>% filter(`Measure` != "Estimated number of students")
fig4 <- rbind(fig4, agg4C)

fig4$`Measure` <- as.factor(fig4$`Measure`)
fig4$`Measure` <- factor(fig4$`Measure`, levels=c("Estimated number of students", "Total grants under state promise", "Total grants under ACP", "Total grants under ACP if promise program converted to stipends"))
ggplot(data=fig4, mapping=aes(x=`Program`, y=`Value`, fill=`Measure`)) + geom_bar(stat="identity", width=0.6, position=position_dodge(0.7)) + theme(legend.position='bottom') + scale_fill_manual(values=statecols[7:9]) + scale_y_continuous(limits=c(0, 15000), labels=scales::dollar_format(accuracy=1)) + labs(y="", fill="")

# Increases by state in Figure 4 
fig4.MI <- fig4 %>% filter(Program=="MI Reconnect")
fig4.NJ <- fig4 %>% filter(Program=="NJ Opportunity")
fig4.NV <- fig4 %>% filter(Program=="NV Promise")
fig4.NY <- fig4 %>% filter(Program=="NY Excelsior")
fig4.OR <- fig4 %>% filter(Program=="OR Promise")
fig4.TN <- fig4 %>% filter(Program=="TN Promise")
fig4.AG <- fig4 %>% filter(Program=="Aggregated")

fig4.MI$Value[fig4.MI$Measure=="Total grants under ACP"] / fig4.MI$Value[fig4.MI$Measure=="Total grants under state promise"]
fig4.NJ$Value[fig4.NJ$Measure=="Total grants under ACP"] / fig4.NJ$Value[fig4.NJ$Measure=="Total grants under state promise"]
fig4.NV$Value[fig4.NV$Measure=="Total grants under ACP"] / fig4.NV$Value[fig4.NV$Measure=="Total grants under state promise"]
fig4.NY$Value[fig4.NY$Measure=="Total grants under ACP"] / fig4.NY$Value[fig4.NY$Measure=="Total grants under state promise"]
fig4.OR$Value[fig4.OR$Measure=="Total grants under ACP"] / fig4.OR$Value[fig4.OR$Measure=="Total grants under state promise"]
fig4.TN$Value[fig4.TN$Measure=="Total grants under ACP"] / fig4.TN$Value[fig4.TN$Measure=="Total grants under state promise"]

fig4.AG$Value[fig4.AG$Measure=="Total grants under ACP"] / fig4.AG$Value[fig4.AG$Measure=="Total grants under state promise"]
fig4.AG$Value[fig4.AG$Measure=="Total grants under ACP if promise program converted to stipends"] / fig4.AG$Value[fig4.AG$Measure=="Total grants under state promise"]
#### End #### 

#### Figure 5 #### 
total5 <- sum(npsas$Value[(npsas$Group=="Promise-eligible, Pell recipient ") & (npsas$Measure=="Estimated number of students")])
agg5A <- npsas %>% filter(Measure=="Estimated number of students") %>% filter(Group=="Promise-eligible, Pell recipient ")
agg5A$`Share of total` <- agg5A$`Value` / total5
agg5A <- agg5A %>% select(`Program`, `Share of total`)
agg5B <- npsas %>% filter(Measure != "Estimated number of students") %>% filter(Group=="Promise-eligible, Pell recipient ")
agg5B <- left_join(x=agg5B, y=agg5A, by="Program")
agg5B$`Weighted value` <- agg5B$Value * agg5B$`Share of total`
agg5C <- aggregate(data=agg5B, `Weighted value` ~ Measure, FUN=sum)
names(agg5C) <- c("Measure", "Value")
agg5C$`Program` <- rep("Aggregated", nrow(agg5C))
agg5C$`Group` <- rep("Promise-eligible, Pell recipient ", nrow(agg5C))
agg5C <- agg5C %>% select(`Program`, `Measure`, `Group`, `Value`)

# Figure 5: Promise-eligible, Pell recipient 
fig5 <- npsas %>% filter(`Group` == "Promise-eligible, Pell recipient ") %>% filter(`Measure` != "Estimated number of students")
fig5 <- rbind(fig5, agg5C)

fig5$`Measure` <- as.factor(fig5$`Measure`)
fig5$`Measure` <- factor(fig5$`Measure`, levels=c("Estimated number of students", "Total grants under state promise", "Total grants under ACP", "Total grants under ACP if promise program converted to stipends"))
ggplot(data=fig5, mapping=aes(x=`Program`, y=`Value`, fill=`Measure`)) + geom_bar(stat="identity", width=0.6, position=position_dodge(0.7)) + theme(legend.position='bottom') + scale_fill_manual(values=statecols[7:9]) + scale_y_continuous(limits=c(0, 15000), labels=scales::dollar_format(accuracy=1)) + labs(y="", fill="")

# Increases by state in Figure 5 
fig5.MI <- fig5 %>% filter(Program=="MI Reconnect")
fig5.NJ <- fig5 %>% filter(Program=="NJ Opportunity")
fig5.NV <- fig5 %>% filter(Program=="NV Promise")
fig5.NY <- fig5 %>% filter(Program=="NY Excelsior")
fig5.OR <- fig5 %>% filter(Program=="OR Promise")
fig5.TN <- fig5 %>% filter(Program=="TN Promise")
fig5.AG <- fig5 %>% filter(Program=="Aggregated")

fig5.MI$Value[fig5.MI$Measure=="Total grants under ACP"] / fig5.MI$Value[fig5.MI$Measure=="Total grants under state promise"]
fig5.NJ$Value[fig5.NJ$Measure=="Total grants under ACP"] / fig5.NJ$Value[fig5.NJ$Measure=="Total grants under state promise"]
fig5.NV$Value[fig5.NV$Measure=="Total grants under ACP"] / fig5.NV$Value[fig5.NV$Measure=="Total grants under state promise"]
fig5.NY$Value[fig5.NY$Measure=="Total grants under ACP"] / fig5.NY$Value[fig5.NY$Measure=="Total grants under state promise"]
fig5.OR$Value[fig5.OR$Measure=="Total grants under ACP"] / fig5.OR$Value[fig5.OR$Measure=="Total grants under state promise"]
fig5.TN$Value[fig5.TN$Measure=="Total grants under ACP"] / fig5.TN$Value[fig5.TN$Measure=="Total grants under state promise"]

fig5.AG$Value[fig5.AG$Measure=="Total grants under ACP"] / fig5.AG$Value[fig5.AG$Measure=="Total grants under state promise"]
fig5.AG$Value[fig5.AG$Measure=="Total grants under ACP if promise program converted to stipends"] / fig5.AG$Value[fig5.AG$Measure=="Total grants under state promise"]

#### End #### 

#### Figure 6 #### 
npsas2 <- read.csv("NPSAS ACP Total Nontuition.csv", header=TRUE)

# Aggregate the increase in Figure 6 
total6 <- sum(npsas2$Value[(npsas2$Group=="All promise-eligible students") & (npsas2$Measure=="Estimated number of students")])
agg6A <- npsas2 %>% filter(Measure=="Estimated number of students") %>% filter(Group=="All promise-eligible students")
agg6A$`Share of total` <- agg6A$`Value` / total6
agg6A <- agg6A %>% select(`Program`, `Share of total`)
agg6B <- npsas2 %>% filter(Measure != "Estimated number of students") %>% filter(Group=="All promise-eligible students")
agg6B <- left_join(x=agg6B, y=agg6A, by="Program")
agg6B$`Weighted value` <- agg6B$Value * agg6B$`Share of total`
agg6C <- aggregate(data=agg6B, `Weighted value` ~ Measure, FUN=sum)
names(agg6C) <- c("Measure", "Value")
agg6C$`Program` <- rep("Aggregated", nrow(agg6C))
agg6C$`Group` <- rep("All promise-eligible students", nrow(agg6C))
agg6C <- agg6C %>% select(`Program`, `Measure`, `Group`, `Value`)

# Figure 6: All Promise Eligible Students 
fig6 <- npsas2 %>% filter(`Group` == "All promise-eligible students") %>% filter(`Measure` != "Estimated number of students")
fig6 <- rbind(fig6, agg6C)

fig6$`Measure` <- as.factor(fig6$`Measure`)
fig6$`Measure` <- factor(fig6$`Measure`, levels=c("Non-tuition grants under state promise", "Non-tuition grants under ACP", "Non-tuition grants under ACP if state promise converted to stipends"))
ggplot(data=fig6, mapping=aes(x=`Program`, y=`Value`, fill=`Measure`)) + geom_bar(stat="identity", width=0.6, position=position_dodge(0.7)) + theme(legend.position='bottom') + scale_fill_manual(values=statecols[10:12]) + scale_y_continuous(limits=c(0, 8000), labels=scales::dollar_format(accuracy=1)) + labs(y="", fill="")

# Increases by state in Figure 6 
fig6.MI <- fig6 %>% filter(Program=="MI Reconnect")
fig6.NJ <- fig6 %>% filter(Program=="NJ Opportunity")
fig6.NV <- fig6 %>% filter(Program=="NV Promise")
fig6.AG <- fig6 %>% filter(Program=="Aggregated")

fig6.MI$Value[fig6.MI$Measure=="Non-tuition grants under ACP"] / fig6.MI$Value[fig6.MI$Measure=="Non-tuition grants under state promise"]
fig6.NJ$Value[fig6.NJ$Measure=="Non-tuition grants under ACP"] / fig6.NJ$Value[fig6.NJ$Measure=="Non-tuition grants under state promise"]
fig6.NV$Value[fig6.NV$Measure=="Non-tuition grants under ACP"] / fig6.NV$Value[fig6.NV$Measure=="Non-tuition grants under state promise"]

fig6.AG$Value[fig6.AG$Measure=="Non-tuition grants under ACP"] / fig6.AG$Value[fig6.AG$Measure=="Non-tuition grants under state promise"]
fig6.AG$Value[fig6.AG$Measure=="Non-tuition grants under ACP if promise program converted to stipends"] / fig6.AG$Value[fig6.AG$Measure=="Non-tuition grants under state promise"]
#### End #### 

#### Figure 7 #### 
# Aggregate the increase in Figure 7 
total7 <- sum(npsas2$Value[(npsas2$Group=="Promise-eligible, Pell recipient ") & (npsas2$Measure=="Estimated number of students")])
agg7A <- npsas2 %>% filter(Measure=="Estimated number of students") %>% filter(Group=="Promise-eligible, Pell recipient ")
agg7A$`Share of total` <- agg7A$`Value` / total7
agg7A <- agg7A %>% select(`Program`, `Share of total`)
agg7B <- npsas2 %>% filter(Measure != "Estimated number of students") %>% filter(Group=="Promise-eligible, Pell recipient ")
agg7B <- left_join(x=agg7B, y=agg7A, by="Program")
agg7B$`Weighted value` <- agg7B$Value * agg7B$`Share of total`
agg7C <- aggregate(data=agg7B, `Weighted value` ~ Measure, FUN=sum)
names(agg7C) <- c("Measure", "Value")
agg7C$`Program` <- rep("Aggregated", nrow(agg7C))
agg7C$`Group` <- rep("Promise-eligible, Pell recipient ", nrow(agg7C))
agg7C <- agg7C %>% select(`Program`, `Measure`, `Group`, `Value`)

# Figure 7: Promise-eligible, Pell recipient 
fig7 <- npsas2 %>% filter(`Group` == "Promise-eligible, Pell recipient ") %>% filter(`Measure` != "Estimated number of students")
fig7 <- rbind(fig7, agg7C)

fig7$`Measure` <- as.factor(fig7$`Measure`)
fig7$`Measure` <- factor(fig7$`Measure`, levels=c("Non-tuition grants under state promise", "Non-tuition grants under ACP", "Non-tuition grants under ACP if state promise converted to stipends"))
ggplot(data=fig7, mapping=aes(x=`Program`, y=`Value`, fill=`Measure`)) + geom_bar(stat="identity", width=0.6, position=position_dodge(0.7)) + theme(legend.position='bottom') + scale_fill_manual(values=statecols[10:12]) + scale_y_continuous(limits=c(0, 8000), labels=scales::dollar_format(accuracy=1)) + labs(y="", fill="")

# Increases by state in Figure 7 
fig7.MI <- fig7 %>% filter(Program=="MI Reconnect")
fig7.NJ <- fig7 %>% filter(Program=="NJ Opportunity")
fig7.NV <- fig7 %>% filter(Program=="NV Promise")
fig7.AG <- fig7 %>% filter(Program=="Aggregated")

fig7.MI$Value[fig7.MI$Measure=="Non-tuition grants under ACP"] / fig7.MI$Value[fig7.MI$Measure=="Non-tuition grants under state promise"]
fig7.NJ$Value[fig7.NJ$Measure=="Non-tuition grants under ACP"] / fig7.NJ$Value[fig7.NJ$Measure=="Non-tuition grants under state promise"]
fig7.NV$Value[fig7.NV$Measure=="Non-tuition grants under ACP"] / fig7.NV$Value[fig7.NV$Measure=="Non-tuition grants under state promise"]

fig7.AG$Value[fig7.AG$Measure=="Non-tuition grants under ACP"] / fig7.AG$Value[fig7.AG$Measure=="Non-tuition grants under state promise"]
fig7.AG$Value[fig7.AG$Measure=="Non-tuition grants under ACP if promise program converted to stipends"] / fig7.AG$Value[fig7.AG$Measure=="Non-tuition grants under state promise"]

#### End #### 

#### Figure 8 #### 
statecols2C <- c('lightsteelblue3', '#526A83', 'lightsalmon3')

AGI2$Type <- factor(AGI2$Type, levels=rev(c("First-Dollar Promise", "America's College Promise", "Pell Grant")))

AGI3 <- AGI2 
AGI4 <- AGI2 
AGI3$Type[AGI3$Type=="First-Dollar Promise"] <- "America's College Promise"
AGI4$Type[AGI4$Type=="First-Dollar Promise"] <- "America's College Promise"

AGI3 <- AGI3 %>% add_row(`AGI Group` = "$0 to $15K", `Type` = "Converted State Promise", `Amount` = (4525 * 0.6))
AGI3 <- AGI3 %>% add_row(`AGI Group` = "$15K to $30K", `Type` = "Converted State Promise", `Amount` = (4005 * 0.6))
AGI3 <- AGI3 %>% add_row(`AGI Group` = "$30K to $45K", `Type` = "Converted State Promise", `Amount` = (3716 * 0.6))
AGI3 <- AGI3 %>% add_row(`AGI Group` = "$45K to $60K", `Type` = "Converted State Promise", `Amount` = (2426 * 0.6))
AGI3 <- AGI3 %>% add_row(`AGI Group` = "$60K to $90K", `Type` = "Converted State Promise", `Amount` = (793 * 0.6))
AGI3 <- AGI3 %>% add_row(`AGI Group` = "$90K to $120K", `Type` = "Converted State Promise", `Amount` = (72 * 0.6))
AGI3 <- AGI3 %>% add_row(`AGI Group` = "More than $120K", `Type` = "Converted State Promise", `Amount` = 0)

AGI3$Type <- factor(AGI3$Type, levels=rev(c("America's College Promise", "Pell Grant", "Converted State Promise")))
ggplot(data=AGI3, mapping=aes(x=`AGI Group`, y=`Amount`, fill=`Type`)) + geom_bar(stat="identity", position="stack") + geom_hline(yintercept=AGI.tuition.value, linetype="dashed") + scale_fill_manual(values=statecols2C) + scale_y_continuous(labels=scales::dollar_format(accuracy=1), limits=c(0, 12400)) + theme(legend.title=element_blank())
# + geom_text(x=6.5, y=3300, label="Dashed line indicates average tuition", color='gray45', size=2.8)
#### End #### 

#### Figure 9 #### 
AGI4 <- AGI4 %>% add_row(`AGI Group` = "$0 to $15K", `Type` = "Converted State Promise", `Amount` = 0)
AGI4 <- AGI4 %>% add_row(`AGI Group` = "$15K to $30K", `Type` = "Converted State Promise", `Amount` = 0)
AGI4 <- AGI4 %>% add_row(`AGI Group` = "$30K to $45K", `Type` = "Converted State Promise", `Amount` = 0)
AGI4 <- AGI4 %>% add_row(`AGI Group` = "$45K to $60K", `Type` = "Converted State Promise", `Amount` = (3000 - 2426))
AGI4 <- AGI4 %>% add_row(`AGI Group` = "$60K to $90K", `Type` = "Converted State Promise", `Amount` = (3000 - 793))
AGI4 <- AGI4 %>% add_row(`AGI Group` = "$90K to $120K", `Type` = "Converted State Promise", `Amount` = (3000 - 72))
AGI4 <- AGI4 %>% add_row(`AGI Group` = "More than $120K", `Type` = "Converted State Promise", `Amount` = 3000)

AGI4$Type <- factor(AGI4$Type, levels=rev(c("America's College Promise", "Pell Grant", "Converted State Promise")))
ggplot(data=AGI4, mapping=aes(x=`AGI Group`, y=`Amount`, fill=`Type`)) + geom_bar(stat="identity", position="stack") + geom_hline(yintercept=AGI.tuition.value, linetype="dashed") + geom_hline(yintercept=AGI.tuition.value + 3000, linetype="dotted") + geom_text(x=6.04, y=7600, label="Dotted line indicates $3,000 living stipend guarantee", color='gray45', size=2.8) + scale_fill_manual(values=statecols2C) + scale_y_continuous(labels=scales::dollar_format(accuracy=1), limits=c(0, 12400)) + theme(legend.title=element_blank())
#### End #### 

#### Create Fed-state model: Part 1 #### 
hd2021 <- fread("hd2021.csv", header=TRUE, select=c(
  "UNITID",    # Unique identification number of the institution
  "INSTNM",    # Institution (entity) name
  "CONTROL",   # Control of institution
  "SECTOR",    # Sector of institution 
  "TRIBAL",    # Tribal college
  "DEGGRANT",  # Degree-granting status
  "HDEGOFR1",  # Highest degree offered
  "STABBR"     # State abbreviation
))

hd2021$STABBR2 <- ifelse(hd2021$TRIBAL==1, "Tribal Colleges", hd2021$STABBR) 
hd2021 <- hd2021 %>% filter(CONTROL==1 | TRIBAL==1) 
hd2021 <- hd2021 %>% filter(SECTOR>0)
hd2021 <- hd2021 %>% filter(DEGGRANT==1) # All community colleges must be degree-granting, and there are no tribal colleges in IPEDS that are not degree-granting

effy2021 <- fread("effy2021.csv", header=TRUE, select=c(
  "UNITID",   # Unique identification number of the institution
  "EFFYALEV", # Level and degree/certificate-seeking status of student
  "EFYTOTLT" # Grand total
))
effy2021 <- effy2021 %>% filter(EFFYALEV == 2) # All students, Undergraduate total
names(effy2021) <- c("UNITID", "EFFYALEV21", "EFYTOTLT21")

effy2020 <- fread("effy2020.csv", header=TRUE, select=c(
  "UNITID",   # Unique identification number of the institution
  "EFFYLEV",  # Level and degree/certificate-seeking status of student
  "EFYTOTLT" # Grand total
))
effy2020 <- effy2020 %>% filter(EFFYLEV == 2) # All students, Undergraduate total
names(effy2020) <- c("UNITID", "EFFYALEV20", "EFYTOTLT20")

effy <- full_join(x=effy2021, y=effy2020, by="UNITID")
effy$EFYTOTLT <- ifelse(is.na(effy$EFYTOTLT21), effy$EFYTOTLT20, effy$EFYTOTLT21)
effy <- effy %>% select(UNITID, EFYTOTLT)
fullData <- left_join(x=hd2021, y=effy, by="UNITID")
fullData$EFYTOTLT <- as.numeric(fullData$EFYTOTLT)

f2021_f1a <- fread("f2021_f1a.csv", header=TRUE, select=c(
  "UNITID",    # Unique identification number of the institution
  "F1E03",     # Grants by state government
  "F1B01",     # Tuition and fees, after deducting discounts and allowances
  "F1E08"      # Discounts and allowances applied to tuition and fees
))
f2021_f1a$F1E08[is.na(f2021_f1a$F1E08)] <- 0   # All institutions with NAs are system offices
f2021_f1a$`Tuition and fee revenue` <- f2021_f1a$F1B01 + f2021_f1a$F1E08
f2021_f1a <- f2021_f1a %>% select(UNITID, F1E03, `Tuition and fee revenue`)

f2021_f2 <- fread("f2021_f2.csv", header=TRUE, select=c(
  "UNITID",    # Unique identification number of the institution
  "F2C03",     # State grants 
  "F2D01",     # Tuition and fees - Total
  "F2C08"      # Allowances applied to tuition and fees
))
f2021_f2$F2C08[is.na(f2021_f2$F2C08)] <- 0      # All institutions with NAs are system offices
f2021_f2$`Tuition and fee revenue` <- f2021_f2$F2D01 + f2021_f2$F2C08
f2021_f2 <- f2021_f2 %>% select(UNITID, F2C03, `Tuition and fee revenue`)

names(f2021_f1a) <- c("UNITID", "State financial aid", "Tuition and fee revenue")
names(f2021_f2) <- c("UNITID", "State financial aid", "Tuition and fee revenue")

f2021_f1a$`State financial aid`[is.na(f2021_f1a$`State financial aid`)] <- 0 # All institutions with NAs are system offices
f2021_f2$`State financial aid`[is.na(f2021_f2$`State financial aid`)] <- 0   # All institutions with NAs are system offices

f2021 <- rbind(f2021_f1a, f2021_f2)
fullData <- left_join(x=fullData, y=f2021, by="UNITID")

ic2021_ay <- fread("ic2021_ay.csv", header=TRUE, select=c(
  "UNITID",    # Unique identification number of the institution
  "TUITION1",  # In-district average tuition for full-time undergraduates 
  "FEE1",      # In-district required fees for full-time undergraduates
  "TUITION2",  # In-state average tuition for full-time undergraduates 
  "FEE2",      # In-state required fees for full-time undergraduates
  "TUITION3",  # Out-of-state average tuition for full-time undergraduates
  "FEE3"       # Out-of-state required fees for full-time undergraduates
))

ic2021_ay$TUITION1 <- ifelse(ic2021_ay$TUITION1 == ".", NA, ic2021_ay$TUITION1)
ic2021_ay$FEE1 <- ifelse(ic2021_ay$FEE1 == ".", NA, ic2021_ay$FEE1)
ic2021_ay$TUITION2 <- ifelse(ic2021_ay$TUITION2 == ".", NA, ic2021_ay$TUITION2)
ic2021_ay$FEE2 <- ifelse(ic2021_ay$FEE2 == ".", NA, ic2021_ay$FEE2)
ic2021_ay$TUITION3 <- ifelse(ic2021_ay$TUITION3 == ".", NA, ic2021_ay$TUITION3)
ic2021_ay$FEE3 <- ifelse(ic2021_ay$FEE3 == ".", NA, ic2021_ay$FEE3)

ic2021_ay$TUITION1 <- as.numeric(ic2021_ay$TUITION1)
ic2021_ay$FEE1 <- as.numeric(ic2021_ay$FEE1)
ic2021_ay$TUITION2 <- as.numeric(ic2021_ay$TUITION2)
ic2021_ay$FEE2 <- as.numeric(ic2021_ay$FEE2)
ic2021_ay$TUITION3 <- as.numeric(ic2021_ay$TUITION3)
ic2021_ay$FEE3 <- as.numeric(ic2021_ay$FEE3)

ic2021_ay$`Different pricing` <- ifelse((ic2021_ay$TUITION1 != ic2021_ay$TUITION2) | (ic2021_ay$TUITION3 != ic2021_ay$TUITION2), "Yes", "No")

ic2021_ay$`In-district tuition and fees` <- ic2021_ay$TUITION1 + ic2021_ay$FEE1
ic2021_ay$`In-state tuition and fees` <- ic2021_ay$TUITION2 + ic2021_ay$FEE2 
ic2021_ay$`Out-of-state tuition and fees` <- ic2021_ay$TUITION3 + ic2021_ay$FEE3

ic2021_ay <- ic2021_ay %>% select(UNITID, `Different pricing`, `In-district tuition and fees`, `In-state tuition and fees`, `Out-of-state tuition and fees`)

fullData <- left_join(x=fullData, y=ic2021_ay, by="UNITID")


c2021_a <- fread("c2021_a.csv", header=TRUE, select=c(
  "UNITID", 
  "CIPCODE", 
  "MAJORNUM", 
  "AWLEVEL", 
  "CTOTALT"
))
c2021_a <- c2021_a %>% filter(CIPCODE==99)
c2021_a <- c2021_a %>% filter(MAJORNUM==1)
c2021_a <- c2021_a %>% filter(AWLEVEL %in% c(3, 5, 7, 17, 18, 19))

c2020_a <- fread("c2020_a.csv", header=TRUE, select=c(
  "UNITID", 
  "CIPCODE", 
  "MAJORNUM", 
  "AWLEVEL", 
  "CTOTALT"
))
c2020_a <- c2020_a %>% filter(CIPCODE==99)
c2020_a <- c2020_a %>% filter(MAJORNUM==1)
c2020_a <- c2020_a %>% filter(AWLEVEL %in% c(3, 5, 7, 17, 18, 19))

c2019_a <- fread("c2019_a_rv.csv", header=TRUE, select=c(
  "UNITID", 
  "CIPCODE", 
  "MAJORNUM", 
  "AWLEVEL", 
  "CTOTALT"
))
c2019_a <- c2019_a %>% filter(CIPCODE==99)
c2019_a <- c2019_a %>% filter(MAJORNUM==1)
c2019_a <- c2019_a %>% filter(AWLEVEL %in% c(3, 5, 7, 17, 18, 19))

c20xx_a <- rbind(c2021_a, c2020_a, c2019_a)

tbl1All <- aggregate(data=c20xx_a, CTOTALT ~ UNITID, FUN=sum)
names(tbl1All) <- c("UNITID", "All Degrees")
associateDegreesOnly <- c20xx_a %>% filter(AWLEVEL==3)
tbl1Assoc <- aggregate(data=associateDegreesOnly, CTOTALT ~ UNITID, FUN=sum)
names(tbl1Assoc) <- c("UNITID", "Associate Degrees")
fullData <- left_join(x=fullData, y=tbl1All, by="UNITID")
fullData <- left_join(x=fullData, y=tbl1Assoc, by="UNITID")
fullData$`Associate Degrees` <- ifelse(is.na(fullData$`Associate Degrees`), 0, fullData$`Associate Degrees`)
fullData$`Associate Share` <- fullData$`Associate Degrees` / fullData$`All Degrees`
fullData <- fullData %>% filter(HDEGOFR1==40 | `Associate Share` >= 0.5)

#### End #### 

#### Create Fed-state model: Part 2 #### 

ic2021_ay <- fread("ic2021_ay.csv", header=TRUE, select=c(
  "UNITID",    # Unique identification number of the institution
  "TUITION1",  # In-district average tuition for full-time undergraduates, 2020-21 
  "FEE1",      # In-district required fees for full-time undergraduates, 2020-21
  "TUITION2",  # In-state average tuition for full-time undergraduates, 2020-21 
  "FEE2",      # In-state required fees for full-time undergraduates, 2020-21
  "TUITION3",  # Out-of-state average tuition for full-time undergraduates
  "FEE3"       # Out-of-state required fees for full-time undergraduates
))

ic2021_ay$TUITION1 <- ifelse(ic2021_ay$TUITION1 == ".", NA, ic2021_ay$TUITION1)
ic2021_ay$FEE1 <- ifelse(ic2021_ay$FEE1 == ".", NA, ic2021_ay$FEE1)
ic2021_ay$TUITION2 <- ifelse(ic2021_ay$TUITION2 == ".", NA, ic2021_ay$TUITION2)
ic2021_ay$FEE2 <- ifelse(ic2021_ay$FEE2 == ".", NA, ic2021_ay$FEE2)
ic2021_ay$TUITION3 <- ifelse(ic2021_ay$TUITION3 == ".", NA, ic2021_ay$TUITION3)
ic2021_ay$FEE3 <- ifelse(ic2021_ay$FEE3 == ".", NA, ic2021_ay$FEE3)

ic2021_ay$TUITION1 <- as.numeric(ic2021_ay$TUITION1)
ic2021_ay$FEE1 <- as.numeric(ic2021_ay$FEE1)
ic2021_ay$TUITION2 <- as.numeric(ic2021_ay$TUITION2)
ic2021_ay$FEE2 <- as.numeric(ic2021_ay$FEE2)
ic2021_ay$TUITION3 <- as.numeric(ic2021_ay$TUITION3)
ic2021_ay$FEE3 <- as.numeric(ic2021_ay$FEE3)

ic2021_ay$`Different pricing` <- ifelse((ic2021_ay$TUITION1 != ic2021_ay$TUITION2) | (ic2021_ay$TUITION3 != ic2021_ay$TUITION2), "Yes", "No")

ic2021_ay$`In-district tuition and fees` <- ic2021_ay$TUITION1 + ic2021_ay$FEE1
ic2021_ay$`In-state tuition and fees` <- ic2021_ay$TUITION2 + ic2021_ay$FEE2 
ic2021_ay$`Out-of-state tuition and fees` <- ic2021_ay$TUITION3 + ic2021_ay$FEE3

ic2021_ay <- ic2021_ay %>% select(UNITID, `Different pricing`, `In-district tuition and fees`, `In-state tuition and fees`, `Out-of-state tuition and fees`)

fullData$`In-district tuition and fees`[fullData$UNITID==223524] <- ic2021_ay$`In-district tuition and fees`[ic2021_ay$UNITID==223524] # Brookhaven College (TX)
fullData$`In-district tuition and fees`[fullData$UNITID==223773] <- ic2021_ay$`In-district tuition and fees`[ic2021_ay$UNITID==223773] # Cedar Valley College (TX)
fullData$`In-district tuition and fees`[fullData$UNITID==224572] <- ic2021_ay$`In-district tuition and fees`[ic2021_ay$UNITID==224572] # Eastfield College (TX)
fullData$`In-district tuition and fees`[fullData$UNITID==226930] <- ic2021_ay$`In-district tuition and fees`[ic2021_ay$UNITID==226930] # Mountain View College (TX)
fullData$`In-district tuition and fees`[fullData$UNITID==227191] <- ic2021_ay$`In-district tuition and fees`[ic2021_ay$UNITID==227191] # North Lake College (TX)
fullData$`In-district tuition and fees`[fullData$UNITID==227766] <- ic2021_ay$`In-district tuition and fees`[ic2021_ay$UNITID==227766] # Richland College (TX)
fullData$`In-district tuition and fees`[fullData$UNITID==214740] <- ic2021_ay$`In-district tuition and fees`[ic2021_ay$UNITID==214740] # Pennsylvania State University-Penn State DuBois (PA)
fullData$`In-district tuition and fees`[fullData$UNITID==214795] <- ic2021_ay$`In-district tuition and fees`[ic2021_ay$UNITID==214795] # Pennsylvania State University-Penn State Mont Alto (PA)
fullData$`In-district tuition and fees`[fullData$UNITID==439145] <- ic2021_ay$`In-district tuition and fees`[ic2021_ay$UNITID==439145] # Pierce College-Puyallup (WA)

fullData$`In-state tuition and fees`[fullData$UNITID==223524] <- ic2021_ay$`In-state tuition and fees`[ic2021_ay$UNITID==223524] # Brookhaven College (TX)
fullData$`In-state tuition and fees`[fullData$UNITID==223773] <- ic2021_ay$`In-state tuition and fees`[ic2021_ay$UNITID==223773] # Cedar Valley College (TX)
fullData$`In-state tuition and fees`[fullData$UNITID==224572] <- ic2021_ay$`In-state tuition and fees`[ic2021_ay$UNITID==224572] # Eastfield College (TX)
fullData$`In-state tuition and fees`[fullData$UNITID==226930] <- ic2021_ay$`In-state tuition and fees`[ic2021_ay$UNITID==226930] # Mountain View College (TX)
fullData$`In-state tuition and fees`[fullData$UNITID==227191] <- ic2021_ay$`In-state tuition and fees`[ic2021_ay$UNITID==227191] # North Lake College (TX)
fullData$`In-state tuition and fees`[fullData$UNITID==227766] <- ic2021_ay$`In-state tuition and fees`[ic2021_ay$UNITID==227766] # Richland College (TX)
fullData$`In-state tuition and fees`[fullData$UNITID==214740] <- ic2021_ay$`In-state tuition and fees`[ic2021_ay$UNITID==214740] # Pennsylvania State University-Penn State DuBois (PA)
fullData$`In-state tuition and fees`[fullData$UNITID==214795] <- ic2021_ay$`In-state tuition and fees`[ic2021_ay$UNITID==214795] # Pennsylvania State University-Penn State Mont Alto (PA)
fullData$`In-state tuition and fees`[fullData$UNITID==439145] <- ic2021_ay$`In-state tuition and fees`[ic2021_ay$UNITID==439145] # Pierce College-Puyallup (WA)

fullData$`Out-of-state tuition and fees`[fullData$UNITID==223524] <- ic2021_ay$`Out-of-state tuition and fees`[ic2021_ay$UNITID==223524] # Brookhaven College (TX)
fullData$`Out-of-state tuition and fees`[fullData$UNITID==223773] <- ic2021_ay$`Out-of-state tuition and fees`[ic2021_ay$UNITID==223773] # Cedar Valley College (TX)
fullData$`Out-of-state tuition and fees`[fullData$UNITID==224572] <- ic2021_ay$`Out-of-state tuition and fees`[ic2021_ay$UNITID==224572] # Eastfield College (TX)
fullData$`Out-of-state tuition and fees`[fullData$UNITID==226930] <- ic2021_ay$`Out-of-state tuition and fees`[ic2021_ay$UNITID==226930] # Mountain View College (TX)
fullData$`Out-of-state tuition and fees`[fullData$UNITID==227191] <- ic2021_ay$`Out-of-state tuition and fees`[ic2021_ay$UNITID==227191] # North Lake College (TX)
fullData$`Out-of-state tuition and fees`[fullData$UNITID==227766] <- ic2021_ay$`Out-of-state tuition and fees`[ic2021_ay$UNITID==227766] # Richland College (TX)
fullData$`Out-of-state tuition and fees`[fullData$UNITID==214740] <- ic2021_ay$`Out-of-state tuition and fees`[ic2021_ay$UNITID==214740] # Pennsylvania State University-Penn State DuBois (PA)
fullData$`Out-of-state tuition and fees`[fullData$UNITID==214795] <- ic2021_ay$`Out-of-state tuition and fees`[ic2021_ay$UNITID==214795] # Pennsylvania State University-Penn State Mont Alto (PA)
fullData$`Out-of-state tuition and fees`[fullData$UNITID==439145] <- ic2021_ay$`Out-of-state tuition and fees`[ic2021_ay$UNITID==439145] # Pierce College-Puyallup (WA)

fullData$`Different pricing`[fullData$UNITID==223524] <- ic2021_ay$`Different pricing`[ic2021_ay$UNITID==223524] # Brookhaven College (TX)
fullData$`Different pricing`[fullData$UNITID==223773] <- ic2021_ay$`Different pricing`[ic2021_ay$UNITID==223773] # Cedar Valley College (TX)
fullData$`Different pricing`[fullData$UNITID==224572] <- ic2021_ay$`Different pricing`[ic2021_ay$UNITID==224572] # Eastfield College (TX)
fullData$`Different pricing`[fullData$UNITID==226930] <- ic2021_ay$`Different pricing`[ic2021_ay$UNITID==226930] # Mountain View College (TX)
fullData$`Different pricing`[fullData$UNITID==227191] <- ic2021_ay$`Different pricing`[ic2021_ay$UNITID==227191] # North Lake College (TX)
fullData$`Different pricing`[fullData$UNITID==227766] <- ic2021_ay$`Different pricing`[ic2021_ay$UNITID==227766] # Richland College (TX)
fullData$`Different pricing`[fullData$UNITID==214740] <- ic2021_ay$`Different pricing`[ic2021_ay$UNITID==214740] # Pennsylvania State University-Penn State DuBois (PA)
fullData$`Different pricing`[fullData$UNITID==214795] <- ic2021_ay$`Different pricing`[ic2021_ay$UNITID==214795] # Pennsylvania State University-Penn State Mont Alto (PA)
fullData$`Different pricing`[fullData$UNITID==439145] <- ic2021_ay$`Different pricing`[ic2021_ay$UNITID==439145] # Pierce College-Puyallup (WA)


collegeScorecard <- fread("Most-Recent-Cohorts-Institution.csv", header=TRUE, select=c(
  "UNITID", 
  "TUITFTE"
))

fullData$`In-state tuition and fees`[fullData$UNITID == 155618] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==155618]  # Northwest Kansas Technical College (KS)
fullData$`In-state tuition and fees`[fullData$UNITID == 369668] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==369668]  # Central Pennsylvania Institute of Science and Technology (PA)
fullData$`In-state tuition and fees`[fullData$UNITID == 383084] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==383084]  # Hacienda La Puente Adult Education (CA)
fullData$`In-state tuition and fees`[fullData$UNITID == 413802] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==413802]  # East San Gabriel Valley Regional Occupational Program (CA)
fullData$`In-state tuition and fees`[fullData$UNITID == 418533] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==418533]  # Lancaster County Career and Technology Center (PA)
fullData$`In-state tuition and fees`[fullData$UNITID == 430795] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==430795]  # Carver Career Center (WV)
fullData$`In-state tuition and fees`[fullData$UNITID == 491844] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==491844]  # Red Lake Nation College (MN)

fullData$`In-district tuition and fees`[fullData$UNITID == 155618] <- fullData$`In-state tuition and fees`[fullData$UNITID == 155618] # Northwest Kansas Technical College (KS)
fullData$`In-district tuition and fees`[fullData$UNITID == 369668] <- fullData$`In-state tuition and fees`[fullData$UNITID == 369668] # Central Pennsylvania Institute of Science and Technology (PA)
fullData$`In-district tuition and fees`[fullData$UNITID == 383084] <- fullData$`In-state tuition and fees`[fullData$UNITID == 383084] # Hacienda La Puente Adult Education (CA)
fullData$`In-district tuition and fees`[fullData$UNITID == 413802] <- fullData$`In-state tuition and fees`[fullData$UNITID == 413802] # East San Gabriel Valley Regional Occupational Program (CA)
fullData$`In-district tuition and fees`[fullData$UNITID == 418533] <- fullData$`In-state tuition and fees`[fullData$UNITID == 418533] # Lancaster County Career and Technology Center (PA)
fullData$`In-district tuition and fees`[fullData$UNITID == 430795] <- fullData$`In-state tuition and fees`[fullData$UNITID == 430795] # Carver Career Center (WV)
fullData$`In-district tuition and fees`[fullData$UNITID == 491844] <- fullData$`In-state tuition and fees`[fullData$UNITID == 491844] # Red Lake Nation College (MN)

fullData$`Out-of-state tuition and fees`[fullData$UNITID == 155618] <- fullData$`In-state tuition and fees`[fullData$UNITID == 155618] # Northwest Kansas Technical College (KS)
fullData$`Out-of-state tuition and fees`[fullData$UNITID == 369668] <- fullData$`In-state tuition and fees`[fullData$UNITID == 369668] # Central Pennsylvania Institute of Science and Technology (PA)
fullData$`Out-of-state tuition and fees`[fullData$UNITID == 383084] <- fullData$`In-state tuition and fees`[fullData$UNITID == 383084] # Hacienda La Puente Adult Education (CA)
fullData$`Out-of-state tuition and fees`[fullData$UNITID == 413802] <- fullData$`In-state tuition and fees`[fullData$UNITID == 413802] # East San Gabriel Valley Regional Occupational Program (CA)
fullData$`Out-of-state tuition and fees`[fullData$UNITID == 418533] <- fullData$`In-state tuition and fees`[fullData$UNITID == 418533] # Lancaster County Career and Technology Center (PA)
fullData$`Out-of-state tuition and fees`[fullData$UNITID == 430795] <- fullData$`In-state tuition and fees`[fullData$UNITID == 430795] # Carver Career Center (WV)
fullData$`Out-of-state tuition and fees`[fullData$UNITID == 491844] <- fullData$`In-state tuition and fees`[fullData$UNITID == 491844] # Red Lake Nation College (MN)

fullData$`Different pricing`[fullData$UNITID == 155618] <- "No" # Northwest Kansas Technical College (KS)
fullData$`Different pricing`[fullData$UNITID == 369668] <- "No" # Central Pennsylvania Institute of Science and Technology (PA)
fullData$`Different pricing`[fullData$UNITID == 383084] <- "No" # Hacienda La Puente Adult Education (CA)
fullData$`Different pricing`[fullData$UNITID == 413802] <- "No" # East San Gabriel Valley Regional Occupational Program (CA)
fullData$`Different pricing`[fullData$UNITID == 418533] <- "No" # Lancaster County Career and Technology Center (PA)
fullData$`Different pricing`[fullData$UNITID == 430795] <- "No" # Carver Career Center (WV)
fullData$`Different pricing`[fullData$UNITID == 491844] <- "No" # Red Lake Nation College (MN)

fullData$`In-district tuition and fees` <- as.numeric(fullData$`In-district tuition and fees`)
fullData$`In-state tuition and fees` <- as.numeric(fullData$`In-state tuition and fees`)
fullData$`Out-of-state tuition and fees` <- as.numeric(fullData$`Out-of-state tuition and fees`)

#### End #### 

#### Create fed-state model: Part 3 #### 

efia2021 <- fread("efia2021.csv", header=TRUE, select=c(
  "UNITID",    # Unique identification number of the institution
  "EFTEUG"     # Estimated full-time equivalent (FTE) undergraduate enrollment, 20120-21
))
fullData <- left_join(x=fullData, y=efia2021, by="UNITID")

efia2020 <- fread("efia2020.csv", header=TRUE, select=c(
  "UNITID",    # Unique identification number of the institution
  "EFTEUG"     # Estimated full-time equivalent (FTE) undergraduate enrollment, 2017-18
))

fullData$EFTEUG[fullData$UNITID==214740] <- as.numeric(efia2020$EFTEUG[efia2020$UNITID==214740]) # Pennsylvania State University-Penn State DuBois
fullData$EFTEUG[fullData$UNITID==223524] <- as.numeric(efia2020$EFTEUG[efia2020$UNITID==223524]) # Brookhaven College
fullData$EFTEUG[fullData$UNITID==223773] <- as.numeric(efia2020$EFTEUG[efia2020$UNITID==223773]) # Cedar Valley College
fullData$EFTEUG[fullData$UNITID==224572] <- as.numeric(efia2020$EFTEUG[efia2020$UNITID==224572]) # Eastfield College
fullData$EFTEUG[fullData$UNITID==226930] <- as.numeric(efia2020$EFTEUG[efia2020$UNITID==226930]) # Mountain View College
fullData$EFTEUG[fullData$UNITID==227191] <- as.numeric(efia2020$EFTEUG[efia2020$UNITID==227191]) # North Lake College
fullData$EFTEUG[fullData$UNITID==227766] <- as.numeric(efia2020$EFTEUG[efia2020$UNITID==227766]) # Richland College
fullData$EFTEUG[fullData$UNITID==413802] <- as.numeric(efia2020$EFTEUG[efia2020$UNITID==413802]) # East San Gabriel Valley Regional Occupational Program
fullData$EFTEUG[fullData$UNITID==439145] <- as.numeric(efia2020$EFTEUG[efia2020$UNITID==439145]) # Pierce College-Puyallup

fullData$EFTEUG <- as.numeric(fullData$EFTEUG)

fullData$`Tuition and fee revenue`[fullData$UNITID == 125499] <- as.numeric(collegeScorecard$TUITFTE[collegeScorecard$UNITID == 125499]) * as.numeric(fullData$EFTEUG[fullData$UNITID == 125499]) # West Valley College (CA)
fullData$`Tuition and fee revenue`[fullData$UNITID == 180081] <- as.numeric(collegeScorecard$TUITFTE[collegeScorecard$UNITID == 180081]) * as.numeric(fullData$EFTEUG[fullData$UNITID == 180081]) # Highlands College of Montana Tech (MT)
fullData$`Tuition and fee revenue`[fullData$UNITID == 200846] <- as.numeric(collegeScorecard$TUITFTE[collegeScorecard$UNITID == 200846]) * as.numeric(fullData$EFTEUG[fullData$UNITID == 200846]) # University of Akron Wayne College (OH)
fullData$`Tuition and fee revenue`[fullData$UNITID == 201432] <- as.numeric(collegeScorecard$TUITFTE[collegeScorecard$UNITID == 201432]) * as.numeric(fullData$EFTEUG[fullData$UNITID == 201432]) # Bowling Green State University-Firelands (OH)
fullData$`Tuition and fee revenue`[fullData$UNITID == 214740] <- as.numeric(collegeScorecard$TUITFTE[collegeScorecard$UNITID == 214740]) * as.numeric(fullData$EFTEUG[fullData$UNITID == 214740]) # Pennsylvania State University-Penn State DuBois (PA)
fullData$`Tuition and fee revenue`[fullData$UNITID == 237701] <- as.numeric(collegeScorecard$TUITFTE[collegeScorecard$UNITID == 237701]) * as.numeric(fullData$EFTEUG[fullData$UNITID == 237701]) # Potomac State College of West Virginia University (WV)
fullData$`Tuition and fee revenue`[fullData$UNITID == 382911] <- as.numeric(collegeScorecard$TUITFTE[collegeScorecard$UNITID == 382911]) * as.numeric(fullData$EFTEUG[fullData$UNITID == 382911]) # Southwest Collegiate Institute for the Deaf (TX)

#### End #### 

#### Create fed-state model: Part 4 ####
ef2021b <- fread("ef2021b.csv", header=TRUE, select=c(
  "UNITID", 
  "EFBAGE", 
  "LSTUDY", 
  "EFAGE09"
)) %>% filter(`EFBAGE` %in% c(1, 3)) %>% filter(`LSTUDY`==1)
ef2021b <- reshape2::dcast(data=ef2021b, UNITID ~ EFBAGE, value.var="EFAGE09")
ef2021b$`3`[is.na(ef2021b$`3`)] <- 0 
ef2021b$`Share who are 18 or older` <- (ef2021b$`1` - ef2021b$`3`) / ef2021b$`1`
ef2021b <- ef2021b %>% select(`UNITID`, `Share who are 18 or older`)
fullData <- left_join(x=fullData, y=ef2021b, by="UNITID")

# Adjusting for dual enrollment: 
fullData$EFTEUG <- fullData$EFTEUG * fullData$`Share who are 18 or older`
fullData$EFYTOTLT <- fullData$EFYTOTLT * fullData$`Share who are 18 or older`
fullData$`Tuition and fee revenue` <- fullData$`Tuition and fee revenue` * fullData$`Share who are 18 or older`

#### End #### 

#### Create fed-state model: Part 5 #### 

ef2021c <- fread("ef2021c.csv", header=TRUE, select=c(
  "UNITID",    # Unique identification number of the institution
  "EFCSTATE",  # State of residence when student was first admitted: Fall 2021
  "EFRES01"    # First-time degree/certificate-seeking undergraduate students: Fall 2021
))
anotherhd2021 <- fread("hd2021.csv", header=TRUE, select=c(
  "UNITID",    # Unique identification number of the institution
  "STABBR"     # State abbreviation
))
statesDF <- read.csv("statesForEF2019C.csv", header=TRUE)
states <- left_join(x=ef2021c, y=anotherhd2021, by="UNITID")
states <- left_join(x=states, y=statesDF, by="EFCSTATE")
states$`In-state enrollment` <- ifelse(states$STABBR == states$BriefStateName, states$EFRES01, 0)
states$`Total enrollment` <- ifelse(states$BriefStateName == "ALLTOT", states$EFRES01, 0)
`In-state enrollment` <- aggregate(data=states, `In-state enrollment` ~ UNITID, FUN=sum)
`Total enrollment` <- aggregate(data=states, `Total enrollment` ~ UNITID, FUN=sum)
enrollmentCalcs <- inner_join(x=`In-state enrollment`, y=`Total enrollment`, by="UNITID")
enrollmentCalcs$`In-state pct` <- enrollmentCalcs$`In-state enrollment` / enrollmentCalcs$`Total enrollment`
fullData <- left_join(x=fullData, y=enrollmentCalcs, by="UNITID")


sfa2021 <- fread("sfa2021.csv", header=TRUE, select=c(
  "UNITID",   # Unique identification number of the institution
  "SCFA11N",  # Number of students in fall cohort who are paying in-district tuition rates, 2020-21
  "SCFA12N",  # Number of students in fall cohort who are paying in-state tuition rates, 2020-21
  "SCFA13N"   # Number of students in fall cohort who are paying out-of-state tuition rates, 2020-21
))

# All ACP-eligible institutions with NA for one of these variables has NA for all of them. 
# Because we only use SCFA11N, SCFA12N, and SCFA13N in relation to one another, imputing 0s does not create issues down the line. 
sfa2021$SCFA11N[is.na(sfa2021$SCFA11N)] <- 0
sfa2021$SCFA12N[is.na(sfa2021$SCFA12N)] <- 0
sfa2021$SCFA13N[is.na(sfa2021$SCFA13N)] <- 0

sfa2021$SCFA11N <- as.numeric(sfa2021$SCFA11N)
sfa2021$SCFA12N <- as.numeric(sfa2021$SCFA12N)
sfa2021$SCFA13N <- as.numeric(sfa2021$SCFA13N)
sfa2021$`Resident pct` <- (sfa2021$SCFA11N + sfa2021$SCFA12N) / (sfa2021$SCFA11N + sfa2021$SCFA12N + sfa2021$SCFA13N)
sfa2021$`Resident pct` <- as.numeric(sfa2021$`Resident pct`)
sfa2021$`Resident pct`[is.nan(sfa2021$`Resident pct`)] <- NA
fullData <- left_join(x=fullData, y=sfa2021, by="UNITID")

# Penn State-DuBois is missing from SFA2021, so we impute 0 values for its SCFA values. 
fullData$SCFA11N[fullData$UNITID==214740] <- 0
fullData$SCFA12N[fullData$UNITID==214740] <- 0
fullData$SCFA13N[fullData$UNITID==214740] <- 0

fullData$`Theoretical resident pct` <- ifelse(is.na(fullData$`Resident pct`)==FALSE, 
                                          ifelse(is.na(fullData$`In-state pct`)==FALSE, 
                                                 ((fullData$`Resident pct` + fullData$`In-state pct`)*0.5), 
                                                 fullData$`Resident pct`), 
                                          ifelse(is.na(fullData$`In-state pct`)==FALSE, 
                                                 fullData$`In-state pct`, 1))

fullData$`New resident pct` <- ifelse(fullData$`Different pricing` == "Yes", fullData$`Theoretical resident pct`, 1)

fullData$`Resident FTE` <- fullData$`New resident pct` * fullData$EFTEUG

fullData$EFYTOTLT <- fullData$EFYTOTLT * fullData$`New resident pct`

fullData$`Resident tuition revenue pct` <- ((fullData$`In-district tuition and fees` * fullData$SCFA11N) + (fullData$`In-state tuition and fees` * fullData$SCFA12N)) / ((fullData$`In-district tuition and fees` * fullData$SCFA11N) + (fullData$`In-state tuition and fees` * fullData$SCFA12N)+ (fullData$`Out-of-state tuition and fees` * fullData$SCFA13N))

fullData$`Resident tuition revenue pct` <- ifelse(is.nan(fullData$`Resident tuition revenue pct`), 1, fullData$`Resident tuition revenue pct`)

fullData$`Resident tuition and fee revenue` <- fullData$`Tuition and fee revenue` * fullData$`Resident tuition revenue pct`
fullData$`Resident tuition and fee revenue per FTE` <- fullData$`Resident tuition and fee revenue` / fullData$`Resident FTE`

fullData$`Adj resident FTE` <- fullData$`Resident FTE` * 0.9459854
fullData$`Adj resident tuition and fee revenue` <- fullData$`Resident tuition and fee revenue` * 0.9459854

fullData$EFYTOTLT <- fullData$EFYTOTLT * 0.852

#### End #### 

#### Create fed-state model: Part 6 ####

fullData$SCFA1AN <- fullData$SCFA11N + fullData$SCFA12N + fullData$SCFA13N

fullData$`Weighted resident tuition and fees` <- ifelse(fullData$`Different pricing`=="No", (fullData$`In-state tuition and fees`), (((fullData$`In-district tuition and fees`) * (fullData$SCFA11N / (fullData$SCFA11N + fullData$SCFA12N))) + ((fullData$`In-state tuition and fees`) * (fullData$SCFA12N / (fullData$SCFA11N + fullData$SCFA12N)))))

fullData$`Weighted all tuition and fees` <- ifelse(fullData$`Different pricing`=="No", (fullData$`In-state tuition and fees`), (((fullData$`In-district tuition and fees`) * (fullData$SCFA11N / fullData$SCFA1AN)) + ((fullData$`In-state tuition and fees`) * (fullData$SCFA12N / fullData$SCFA1AN)) + ((fullData$`Out-of-state tuition and fees`) * (fullData$SCFA13N / fullData$SCFA1AN))))

fullData$`Tuition FTE product` <- fullData$`Adj resident FTE` * fullData$`Weighted resident tuition and fees`

fullStateData <- fullData %>% filter(TRIBAL==2 & (STABBR2 %in% c("AS",	# American Samoa
                                                                 "DC",  # District of Columbia
                                                                 "FM",	# Federated States of Micronesia
                                                                 "GU",	# Guam
                                                                 "MH",	# Marshall Islands
                                                                 "MP",	# Northern Marianas
                                                                 "PW",	# Palau
                                                                 "PR",	# Puerto Rico
                                                                 "VI"	# Virgin Islands
)==FALSE))


medianTuition <- median(fullStateData$`In-state tuition and fees`, na.rm=TRUE)
fullStateData$`Partnership cost` <- fullStateData$`Adj resident FTE` * medianTuition

stateModel <- aggregate(data=fullStateData, cbind(
  `Adj resident FTE`, 
  `EFTEUG`, 
  `Partnership cost`, 
  `Tuition FTE product`, 
  `Tuition and fee revenue`, 
  `Adj resident tuition and fee revenue`
  ) ~ STABBR2, FUN=sum)

#### End #### 

#### Figure 10 ####
stateModel0 <- stateModel %>% select(`STABBR2`, `Partnership cost`) %>% rename(`State` = `STABBR2`)

fig10 <- left_join(x=averageFunding, y=stateModel0, by="State")

fig10 <- fig10 %>% select(`State`, `AnnualSpending`, `Partnership cost`)

fig10 <- fig10 %>% rename(`State Promise` = `AnnualSpending`) %>% rename(`America's College Promise` = `Partnership cost`)
  
fig10 <- fig10 %>% pivot_longer(cols=c(`State Promise`, `America's College Promise`))

fig10$`State2` <- rep(NA, nrow(fig10))
fig10$`State2`[fig10$`State`=="CA"] <- "California"
fig10$`State2`[fig10$`State`=="WA"] <- "Washington"
fig10$`State2`[fig10$`State`=="NJ"] <- "New Jersey"
fig10$`State2`[fig10$`State`=="MI"] <- "Michigan"
fig10$`State2`[fig10$`State`=="MD"] <- "Maryland"
fig10$`State2`[fig10$`State`=="MO"] <- "Missouri"
fig10$`State2`[fig10$`State`=="OR"] <- "Oregon"
fig10$`State2`[fig10$`State`=="TN"] <- "Tennessee"
fig10$`State2`[is.na(fig10$`State2`)] <- "Other states"

fig10 <- aggregate(data=fig10, value ~ `State2` + name, FUN=sum)

fig10$`State2` <- as.factor(fig10$`State2`)
fig10$`State2` <- factor(fig10$`State2`, levels=c("California", "Maryland", "Michigan", "Missouri", "New Jersey", "Other states", "Oregon", "Tennessee", "Washington"))

ggplot(data=fig10, mapping=aes(x=`name`, y=`value`, fill=`State2`)) + geom_bar(position="stack", stat="identity") + scale_fill_manual(values=statecols) + labs(x="Program", y="Annual Grants", fill="State") + scale_y_continuous(labels=dollar_format(accuracy=1))
ggplot(data=fig10, mapping=aes(x=`name`, y=`value`, fill=`name`)) + geom_bar(position="stack", stat="identity") + scale_fill_manual(values=statecols) + labs(x="Program", y="Annual Grants", fill="State") + scale_y_continuous(labels=dollar_format(accuracy=1)) + theme(legend.position = "none")

# Stats: 
aggregate(data=fig10, value ~ name, FUN=sum)

#### End #### 

#### Table 1 #### 
promiseCosts <- data.frame("STABBR2" = c("CA"), 
                           "Promise Program Cost" = c(654267518))
names(promiseCosts) <- c("STABBR2", "Promise Program Cost")
promiseCosts <- promiseCosts %>% add_row(`STABBR2` = "WA", `Promise Program Cost` = 122376961)
promiseCosts <- promiseCosts %>% add_row(`STABBR2` = "RI", `Promise Program Cost` = 7030366)
promiseCosts <- promiseCosts %>% add_row(`STABBR2` = "OR", `Promise Program Cost` = 19932080)
promiseCosts <- promiseCosts %>% add_row(`STABBR2` = "MI", `Promise Program Cost` = 14166236)
promiseCosts <- promiseCosts %>% add_row(`STABBR2` = "NJ", `Promise Program Cost` = 26412180)
promiseCosts <- promiseCosts %>% add_row(`STABBR2` = "WV", `Promise Program Cost` = 3929899)
promiseCosts <- promiseCosts %>% add_row(`STABBR2` = "OK", `Promise Program Cost` = 10310609)
promiseCosts <- promiseCosts %>% add_row(`STABBR2` = "MO", `Promise Program Cost` = 48828874)
promiseCosts <- promiseCosts %>% add_row(`STABBR2` = "HI", `Promise Program Cost` = 2932719)
promiseCosts <- promiseCosts %>% add_row(`STABBR2` = "NM", `Promise Program Cost` = 3869504)
promiseCosts <- promiseCosts %>% add_row(`STABBR2` = "KY", `Promise Program Cost` = 9257247)
promiseCosts <- promiseCosts %>% add_row(`STABBR2` = "NV", `Promise Program Cost` = 2794157)
promiseCosts <- promiseCosts %>% add_row(`STABBR2` = "MD", `Promise Program Cost` = 6301319)
promiseCosts <- promiseCosts %>% add_row(`STABBR2` = "AR", `Promise Program Cost` = 2084297)
promiseAnalysis <- inner_join(x=stateModel, y=promiseCosts, by="STABBR2")

promiseAnalysis$`State share` <- promiseAnalysis$`Partnership cost` * 0.2   # Assume an 80% federal share of costs

# How much money is leftover after applying the partnership money to cover tuition? 
promiseAnalysis$`Leftover` <- promiseAnalysis$`Partnership cost` - promiseAnalysis$`Adj resident tuition and fee revenue`

# Formatting: 
promiseAnalysis2 <- promiseAnalysis %>% select(`STABBR2`, `Partnership cost`, `Adj resident tuition and fee revenue`, `Leftover`, `Promise Program Cost`)
promiseAnalysis2$`Partnership cost` <- dollar(round(promiseAnalysis2$`Partnership cost`, -5))
promiseAnalysis2$`Adj resident tuition and fee revenue` <- dollar(round(promiseAnalysis2$`Adj resident tuition and fee revenue`, -5))
promiseAnalysis2$`Leftover` <- dollar(round(promiseAnalysis2$`Leftover`, -5))
promiseAnalysis2$`Promise Program Cost` <- dollar(round(promiseAnalysis2$`Promise Program Cost`, -5))

#### End #### 
