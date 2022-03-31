# Install packages - only needs to be run once
#install.packages("readr")
#install.packages("psych")
#install.packages("pwr")
#install.packages("ggplot2", dependencies = TRUE)

# Load Libraries
library(readr)
library(pwr)
library(ggplot2)

# Load data from Stage Two from CSV
stageTwoData <- read.csv("D:/R/testr/StageTwoData.csv")

# Create empty vector for all Pair picks to go into
s2Picks <- vector()

# Add either Artefact or Human to the vector depending on pick
for (x in stageTwoData$ARTEFACTPICKED){
  if (x == " True"){
    s2Picks <- append(s2Picks, "Artefact")
  }
  if (x == " False"){
    s2Picks <- append(s2Picks, "Human")
  }
}

# Turn vector to Factor
s2PicksFactor <- factor(s2Picks)

# Turn Factor to table object
s2PicksTable <- table(s2PicksFactor)

# Apply table values to integers
artefactPicks <- s2PicksTable[1]
humanPicks <- s2PicksTable[2]

# Get proportions from results
s2Prop <- s2PicksTable / sum(s2PicksTable)

# This is our expected proportion (as there are only 2 options)
expProp <- 0.5

# Cohens H to calculate effect size
cohensH <- ES.h(s2Prop[1], expProp)

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


# Run Binomial Test - to check if Artefact picks are sufficiently less
binom.test(artefactPicks, artefactPicks + humanPicks, alternative = "less")

# Create Bar Chart
df <- as.data.frame(s2PicksTable)
df

chartColours <- c("Artefact","Human")
df <- cbind(df, chartColours)
barChart <- ggplot(data=df, aes(x=s2PicksFactor, y=Freq, fill=chartColours)) + 
  geom_bar(colour="black", stat="identity", width = 0.75) +
  theme(text=element_text(size=20)) +
  guides(fill=FALSE) +
  xlab("Rooms") + ylab("Picks") +
  ggtitle("Stage Two: Pick which one you believe is the Artefact")

barChart
