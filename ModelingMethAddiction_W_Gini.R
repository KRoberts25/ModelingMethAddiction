
# Comparing Meeting locations and Poverty Levels

CountyData <- read.csv("C:/Users/Karen/OneDrive/Desktop/SCHOOL/Dissertation/Fall24/CountyData3.csv")
CountyData
dim(CountyData)
CountyData2 <- CountyData[,1:13]
CountyData2 <- data.frame(CountyData2,CountyData[, 21:24])
CountyData2
dim(CountyData2)

# Create classes for poverty levels
which.min(CountyData2$Poverty.Percentage._.P)
minPov <- CountyData2$Poverty.Percentage._.P[9]    # Returns 8.2
minPov

which.max(CountyData$Poverty.Percentage._.P)
maxPov <- CountyData$Poverty.Percentage._.P[1]     # Returns 28.7
maxPov

range <- maxPov - minPov
range     # Returns 20.5

NumCounties <- length(CountyData2$County)
NumCounties  # Returns 77 which is correct amount of counties

TotPov <- sum(CountyData2$Poverty)  # This is the number of people in poverty in 2016



hist(CountyData2$Poverty.Percentage._.P, col= "grey80", xlab = "Percent Poverty", 
     ylab = "Count of Counties", main = "Poverty Histogram")

range/11

# Create poverty groups

bin1 <- minPov +  0.01863636
bin2 <- bin1 +  0.01863636
bin3 <- bin2 +  0.01863636
bin4 <- bin3 +  0.01863636
bin5 <- bin4 +  0.01863636
bin6 <- bin5 +  0.01863636
bin7 <- bin6 +  0.01863636
bin8 <- bin7 +  0.01863636
bin9 <- bin8 +  0.01863636
bin10 <- bin9 +  0.01863636
bin11 <- bin10 +  0.01863636

# Create new column that tells which poverty group a county falls in
CountyData2$PovGroup = 0
CountyData2$PovGroup[CountyData2$Poverty.Percentage._.P <= bin11] <- 1
CountyData2$PovGroup[CountyData2$Poverty.Percentage._.P <= bin10] <- 2
CountyData2$PovGroup[CountyData2$Poverty.Percentage._.P <= bin9] <- 3
CountyData2$PovGroup[CountyData2$Poverty.Percentage._.P <= bin8] <- 4
CountyData2$PovGroup[CountyData2$Poverty.Percentage._.P <= bin7] <- 5
CountyData2$PovGroup[CountyData2$Poverty.Percentage._.P <= bin6] <- 6
CountyData2$PovGroup[CountyData2$Poverty.Percentage._.P <= bin5] <- 7
CountyData2$PovGroup[CountyData2$Poverty.Percentage._.P <= bin4] <- 8
CountyData2$PovGroup[CountyData2$Poverty.Percentage._.P <= bin3] <- 9
CountyData2$PovGroup[CountyData2$Poverty.Percentage._.P <= bin2] <- 10
CountyData2$PovGroup[CountyData2$Poverty.Percentage._.P <= bin1] <- 11

CountyData2
dim(CountyData2)


# Correlations

plot(CountyData2)

CountyData2$Addicted <- as.numeric((0.4)*(as.numeric(CountyData2$Population._.Phi)))
CountyData2$Addicted

Poverty.Addicted <- cor(CountyData2$Poverty.Percentage._.P, CountyData2$Addicted)
Poverty.DrugCourt <-cor(CountyData2$Poverty.Percentage._.P, CountyData2$Number.in.DC)
PerCapitaIncome.Addicted <-cor(CountyData2$Per.Capital.Income, CountyData2$Addicted)
PerCapitaIncome.DrugCourt <-cor(CountyData2$Per.Capital.Income, CountyData2$Number.in.DC)
PercentRural.Addicted <-cor(CountyData2$Percent.Rural, CountyData2$Addicted)
PercentRural.DrugCourt <-cor(CountyData2$Percent.Rural, CountyData2$Number.in.DC)

Correlations <- data.frame(Poverty.Addicted, Poverty.DrugCourt, PerCapitaIncome.Addicted, 
                           PerCapitaIncome.DrugCourt, PercentRural.Addicted, PercentRural.DrugCourt)
Correlations <- t(Correlations)
Correlations

library(lattice)
CorChart <- barchart(Correlations, xlab = "Correlation", main="Correlations")
CorChart



######################### TEST MAPS CODE ########################



library(choroplethr)
library(choroplethrMaps)





# Data cleanup to isolate the different information to be plotted into new smaller
#  useable data tables for choropleth format


# Poverty Percentage by County

PovertyRegVal <- CountyData2[, c(2,6)]
PovertyRegVal
PovertyRegVal <- PovertyRegVal[1:77,]
na.omit(PovertyRegVal)

names(PovertyRegVal) <- c("region", "value")
PovertyRegVal

PovMap <- county_choropleth(PovertyRegVal,
                            title      = "2016 Percentage of Population in Poverty",
                            legend     = "Poverty Percentage",
                            num_colors = 4,
                            state_zoom = "oklahoma")



# Number of Available AA Meetings by County

MeetingsRegVal <- CountyData2[, c(2,8)]
MeetingsRegVal
MeetingsRegVal <- MeetingsRegVal[1:77,]
na.omit(MeetingsRegVal)

names(MeetingsRegVal) <- c("region", "value")
MeetingsRegVal

MeetingMap <- county_choropleth(MeetingsRegVal,
                                title      = "2018 Alcoholics Anonymous Meeting Locations",
                                legend     = "Number of Available Meetings",
                                num_colors = 4,
                                state_zoom = "oklahoma")

# Number of Available AA Meetings by County per square footage area

MeetingsAreaRegVal <- CountyData2[, c(2,9)]
MeetingsAreaRegVal
MeetingsAreaRegVal <- MeetingsAreaRegVal[1:77,]
na.omit(MeetingsAreaRegVal)

names(MeetingsAreaRegVal) <- c("region", "value")
MeetingsAreaRegVal

MeetingAreaMap <- county_choropleth(MeetingsAreaRegVal,
                                    title      = "2018 Alcoholics Anonymous Meeting Availability by Area",
                                    legend     = "Number of Available Meetings per Square Foot",
                                    num_colors = 4,
                                    state_zoom = "oklahoma")

# Poverty group by county

PovGroupRegVal <- CountyData2[, c(2,17)]
PovGroupRegVal
PovGroupRegVal <- PovGroupRegVal[1:77,]
na.omit(PovGroupRegVal)

names(PovGroupRegVal) <- c("region", "value")
PovGroupRegVal

PovGroupMap <- county_choropleth(PovGroupRegVal,
                                 title      = "Poverty Group by County 2016",
                                 legend     = "Poverty Group",
                                 num_colors = 4,
                                 state_zoom = "oklahoma")



# Percent Rural by County

RuralRegVal <- CountyData2[, c(2,15)]
RuralRegVal
RuralRegVal <- RuralRegVal[1:77,]
na.omit(RuralRegVal)

names(RuralRegVal) <- c("region", "value")
RuralRegVal

RuralMap <- county_choropleth(RuralRegVal,
                              title      = "Percent Rural by County 2016",
                              legend     = "Percent Rural",
                              num_colors = 4,
                              state_zoom = "oklahoma")



# Rank by Per Capita Income

RankRegVal <- CountyData2[, c(2,16)]
RankRegVal
RankRegVal <- RankRegVal[1:77,]
na.omit(RankRegVal)

names(RankRegVal) <- c("region", "value")
RankRegVal

RankMap <- county_choropleth(RankRegVal,
                             title      = "Rank by Per Capita Income by County 2016",
                             legend     = "Rank by Per Capita Income",
                             num_colors = 4,
                             state_zoom = "oklahoma")


# Per Capita Income

PCIncomeRegVal <- CountyData2[, c(2,14)]
PCIncomeRegVal
PCIncomeRegVal <- PCIncomeRegVal[1:77,]
na.omit(PCIncomeRegVal)

names(PCIncomeRegVal) <- c("region", "value")
class(PCIncomeRegVal$value)

PCIncomeMap <- county_choropleth(PCIncomeRegVal,
                                 title      = "Per Capita Income by County 2016",
                                 legend     = "Per Capita Income",
                                 num_colors = 4,
                                 state_zoom = "oklahoma")



# Number in DC

DCRegVal <- CountyData2[, c(2,13)]
DCRegVal
DCRegVal <- DCRegVal[1:77,]
DCRegVal <- na.omit(DCRegVal)

names(DCRegVal) <- c("region", "value")

DCMap <- county_choropleth(DCRegVal,
                           title      = "Number in Drug Court by County 2016",
                           legend     = "People in Drug Court",
                           num_colors = 9,
                           state_zoom = "oklahoma")







# Per Capita Income

AddictedRegVal <- CountyData2[, c(2,18)]
AddictedRegVal
AddictedRegVal <- AddictedRegVal[1:77,]
AddictedRegVal <- na.omit(AddictedRegVal)

names(AddictedRegVal) <- c("region", "value")

AddictedMap <- county_choropleth(AddictedRegVal,
                                 title      = "Number Addicted to Meth by County 2016",
                                 legend     = "People Addicted",
                                 num_colors = 9,
                                 state_zoom = "oklahoma")


# Overdose Rate

ODRegVal <- CountyData2[, c(2,12)]
ODRegVal
ODRegVal <- ODRegVal[1:77,]
ODRegVal <- na.omit(ODRegVal)

names(ODRegVal) <- c("region", "value")

ODMap <- county_choropleth(ODRegVal,
                           title      = "Drug Overdose Death Rate 2016",
                           legend     = " Overdose Death Rate",
                           num_colors = 5,
                           state_zoom = "oklahoma")


# Per Capita Income

Gini_Index2022 <- CountyData2[, c(2,17)]
Gini_Index2022
Gini_Index2022 <- Gini_Index2022[1:77,]
na.omit(Gini_Index2022)

names(Gini_Index2022) <- c("region", "value")
class(Gini_Index2022$value)

Gini_Index2022Map <- county_choropleth(Gini_Index2022,
                                 title      = "Gini Index by County 2022",
                                 legend     = "Gini Index",
                                 num_colors = 4,
                                 state_zoom = "oklahoma")



# Plot Maps
PovMap
PovGroupMap
RuralMap
RankMap
PCIncomeMap
DCMap
AddictedMap
MeetingMap
MeetingAreaMap
ODMap
Gini_Index2022Map


# Import Data file for 3D surface plot
ThreeDData <- read.csv("C:/Users/Karen/OneDrive/Laptop Transfer/PDE/Project/Data/3DPlots.csv")
#ThreeDData <- write.csv("C:/Users/Karen/OneDrive/Desktop/SCHOOL/Dissertation/Fall24/ThreeDData.csv")

plot(ThreeDData)

library(lattice)

SusceptibleClass <- wireframe(Susceptible ~ Poverty*Time, data = ThreeDData,
                              xlab = "People", ylab = "Time",
                              main = "Susceptible Population",
                              drape = TRUE,
                              colorkey = TRUE,
                              screen = list(z = -60, x = -60)
)


InfectedClass <- wireframe(Infected ~ Poverty*Time, data = ThreeDData,
                           xlab = "People", ylab = "Time",
                           main = "Infected Population",
                           drape = TRUE,
                           colorkey = TRUE,
                           screen = list(z = -60, x = -60)
)


RecoveredClass <- wireframe(Recovered ~ Poverty*Time, data = ThreeDData,
                            xlab = "People", ylab = "Time",
                            main = "Recovered Population",
                            drape = TRUE,
                            colorkey = TRUE,
                            screen = list(z = -60, x = -60)
)

SusceptibleClass
InfectedClass
RecoveredClass

