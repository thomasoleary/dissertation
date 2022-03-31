# Install packages - only needs to be run once
#install.packages("readr")
#install.packages("psych")
#install.packages("pwr")
#install.packages("ggplot2", dependencies = TRUE)

# Load Libraries
library(readr)
library(pwr)
library(ggplot2)

# Load data from Stage One from CSV
stageOneData <- read.csv("D:/R/testr/StageOneData.csv")

# Create empty vector for all Pair picks to go into
s1Picks <- vector()

# Add either Artefact or Human to the vector depending on pick
for (x in stageOneData$ARTEFACTPICKED){
  if (x == " True"){
    s1Picks <- append(s1Picks, "Artefact")
  }
  if (x == " False"){
    s1Picks <- append(s1Picks, "Human")
  }
}

# Turn vector to Factor
s1PicksFactor <- factor(s1Picks)

# Turn Factor to table object
s1PicksTable <- table(s1PicksFactor)

# Apply table values to integers
artefactPicks <- s1PicksTable[1]
humanPicks <- s1PicksTable[2]

# Get proportions from results
s1Prop <- s1PicksTable / sum(s1PicksTable)
s1Prop

# This is our expected proportion (as there are only 2 options)
expProp <- 0.5

# Cohens H to calculate effect size
cohensH <- ES.h(s1Prop[1], expProp)

# For interpretation
h <- abs(cohensH * sqrt(2))

if (h < 0.2){
  print("Negligible Effect Size")
} else if (h < 0.5){
  print("Small Effect Size")
} else if (h < 0.8){
  print("Medium Effect Size")
} else{
  print("Large Effect Size")
}
paste("Effect Size:",h)

##### Z TEST APPROXIMATION FROM A BINOMIAL TEST #####

# Calculating the Standard Deviation
sD <- sqrt((artefactPicks + humanPicks) * expProp * (1 - expProp))

# Calculating the population mean for a large sample size
populationMean <- (artefactPicks + humanPicks) / 2

# Calculating the Z score
zScore <- (artefactPicks - populationMean) / sD
paste("Z Score:",zScore)

# Rounding Z score to 2 decimal places (standard)
zScore <- round(zScore, 2)

# Calculating P Value from Z Score
pValue <- pnorm(zScore, lower.tail = FALSE)
# Rounding P Value to 4 decimal places
pValue <- round(pValue, 4)
paste("P Value:",pValue)


##### BAR CHART #####

# Create Bar Chart
df <- as.data.frame(s1PicksTable)
chartColours <- c("Artefact","Human")
df <- cbind(df, chartColours)

barChart <- ggplot(data=df, aes(x=s1PicksFactor, y=Freq, fill=chartColours)) + 
  geom_bar(colour="black", stat="identity", width = 0.75) +
  theme(text=element_text(size=20)) +
  guides(fill=FALSE) +
  xlab("Rooms") + ylab("Picks") +
  ggtitle("Stage One: Pick which room you prefer")

barChart