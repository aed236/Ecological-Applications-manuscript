################
#Nestedness analysis used in the 'Both feeding stages of global, crop pollinating Diptera require diverse diet and habitat needs' manuscript
################

library(vegan)
library(bipartite) #this package contains the "nested" stat which looks at all possible ways to analyze nestedness
library(ggplot2)
library(gridGraphics)
library(readr)

#Data is in the form of a presence/absence matrix:
# "1" indicates a species lives in a particular habitat, and '0' means the species was not found living in this particular habitat

# Insert raw csv file for species data (found as rawhabitatdata1.csv in the Github repository) #
#We will not include the 'laboratory' habitat, as these fly species were not found in natural conditions
rawhabitatdata1 <- read_csv("~/Desktop/rawhabitatdata1.csv", col_types = cols(Laboratory = col_skip()))

#Change all the NAs to "0", as this means the species were not found in that habitat and data must be binary
rawhabitatdata1[is.na(rawhabitatdata1)] <- 0  

View(rawhabitatdata1)
#the actual species names are NOT included in this analysis
#we are not looking at which species are nested within habitats, just the community as a whole
#Therefore: 
# The rows (x) corresponds to a particular species
# The columns (y) correspond to a habitat as recorded from the literature

                             ########### Calculating nestedness ############

##### First, we will see if there is nestedness between SPECIES and habitats #####

#We will be using temperature as a measure of nestedness
# Temperature measures the order in which species' extinction or colonization would occur in the system 
#The community is nested if the temperature metric is 'colder' (closer to zero) - which indicates there is a fixed order of extinction in the system

nested(rawhabitatdata1, method = "binmatnest", rescale = FALSE, normalised = TRUE)
?nested
#The temperature is 1.144752 and is "cold", so the data IS nested
#From this result, we can discuss that there is a fixed order of extinction
#Which likely means that species that use a specialized (one) habitat will become extinct first
#While generalists (utilizing two or more habitat substrates) will live longer in the system

#### Another method of testing nestedness in the communitity
out1 <- nestedtemp(rawhabitatdata1)
plot(out1, kind = "incid")
#This incidence matrix follows the nestedness boundary line

nestedness(rawhabitatdata1)
#The temperature is 1.149184 and is also "cold", so the data IS nested


                  ##### Comparing our nestedness model to null models #####

#To further see if our nestedness model is accurate, we will compare our results to null models
#Null models are constructed to test the hypothesis that the existence of a pattern (in this case, a nested community) is the result of random processes or can be expected by chance alone

#To do this, we will use the 'oecosimu' function, which takes a statistic or a vector of statistics in community and evaluates its significance in a series of simulated random communities.

#First, we will maintain the recorded number of presences in the matrix, but completely randomized the species – habitat distributions.
oecosimu(rawhabitatdata1, nestedtemp, "r00") #original data is significantly nested compared to null communities

#Second, we will maintain the species (rows) presences within the matrix but randomized the habitat distributions
oecosimu(rawhabitatdata1, nestedtemp, "r0")

#Third, we will maintain the habitat (column) presences in the matrix but randomized the species distributions.
oecosimu(rawhabitatdata1, nestedtemp, "c0")






                                  ##### nestedness between FAMILIES and habitats #####

#Download the fhabitat.csv file from Github
fhabitat <- read.csv("~/Desktop/fhabitat.csv")
View(fhabitat)

#Next we will add the family names to the matrix
row.names(fhabitat) <- c("Anthomyiidae", 
                         "Bibionidae", 
                         "Bombyliidae",
                         "Calliphoridae", 
                         "Ceciomyiidae",
                         "Ceratopogonidae", 
                         "Drosophilidae", 
                         "Ephydridae", 
                         "Fanniidae", 
                         "Milichiidae",
                         "Muscidae", 
                         "Opomyzidae", 
                         "Rhiniidae", 
                         "Rhinophoridae",
                         "Sarcophagidae",
                         "Scathophagidae", 
                         "Sciomyzidae", 
                         "Sepsidae", 
                         "Simuliidae", 
                         "Stratiomyidae",
                         "Syrphidae", 
                         "Tabanidae", 
                         "Tachinidae", 
                         "Tephritidae", 
                         "Ulidiidae")


fhabitat <- t(fhabitat)
View(fhabitat)

#### Another method of testing nestedness in the communitity
out2 <- nestedtemp(fhabitat)
plot(out2, kind = "incid")
#This incidence matrix follows the nestedness boundary line

nestedtemp(fhabitat)
#The temperature is 5.471937    and is also "cold", so the data IS nested


                  ##### Comparing our nestedness model to null models #####

#To further see if our nestedness model is accurate, we will compare our results to null models
#Null models are constructed to test the hypothesis that the existence of a pattern (in this case, a nested community) is the result of random processes or can be expected by chance alone

#To do this, we will use the 'oecosimu' function, which takes a statistic or a vector of statistics in community and evaluates its significance in a series of simulated random communities.

#First, we will maintain the recorded number of presences in the matrix, but completely randomized the species – habitat distributions.
oecosimu(fhabitat, nestedtemp, "r00") #original data is significantly nested compared to null communities

#Second, we will maintain the species (rows) presences within the matrix but randomized the habitat distributions
oecosimu(fhabitat, nestedtemp, "r0") #original data is significantly nested compared to null communities

#Third, we will maintain the habitat (column) presences in the matrix but randomized the species distributions.
oecosimu(fhabitat, nestedtemp, "c0") #original data is significantly nested compared to null communities


##### making the figure ######
library(gridGraphics)
library(bipartite)

new_fhabitat <- grid.grabExpr(grid.echo(function() visweb(fhabitat, labsize = 3.4, square = "interaction", text = "interaction", textsize = 2, textcol = "#1d908c")))
# shift the left axis labels to the right
new_fhabitat[["children"]][["graphics-plot-1-left-axis-labels-1"]][["x"]] <- unit(0.75, units = "in")
# shift the bottom axis labels upwards
new_fhabitat[["children"]][["graphics-plot-1-bottom-axis-labels-1"]][["y"]] <- unit(0.1, units = "in")

grid.newpage(); grid.draw(new_fhabitat)

