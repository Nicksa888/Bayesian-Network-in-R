######################
## Source Documents ##
######################

source("C:/R Portfolio/Bayesian_Network_R_Jupyter/Functions_Bayes_Net_SA.R")
source("C:/R Portfolio/Bayesian_Network_R_Jupyter/South_Asia_Data_Objects.R")

################
## South Asia ##
################
Region_Name <- "South Asia"
SA <- Region_Prep_Group(GTD_WD, Region_Name)
SA

SA %>% 
  map(~count(data.frame(x=.x), x, sort = T))

glimpse(SA)

####################
####################
# One Hot Encoding # 
####################
####################

# One Hot Encoding (excluding 'Lethal')

# Store the 'Lethal' column separately
Lethal_column <- SA[["Lethal"]]

# One Hot Encoding (excluding 'Lethal')
SA_Binary <- one_hot(as.data.table(SA[, !names(SA) %in% c('Lethal')]))

# Add 'Lethal' back to the dataset
SA_Binary$Lethal <- Lethal_column

# Remove certain columns

SA_Binary <- SA_Binary %>% select(-c("Year", "Month", "Day", "Dead"))

names(SA_Binary)

glimpse(SA_Binary)

# Assuming 'your_data' is your dataset
SA_Binary$Lethal <- as.integer(SA_Binary$Lethal)

# Recode some variables #

SA_Binary <- SA_Binary %>% 
  rename(
    SriLanka = `Country_Sri Lanka`,
    OtherCountry = Country_OtherCountry,
    Afghanistan_Nationality = Nationality_Afghanistan,
    India_Nationality = Nationality_India,
    Pakistan_Nationality = Nationality_Pakistan,
    Sri_Lanka_Nationality = `Nationality_Sri Lanka`,
    OtherNationality = Nationality_OtherNationality,
    Balochistan_Province = Province_Balochistan,
    Jammu_Kashmir_Province = `Province_Jammu and Kashmir`,
    Khyber_Pakhtunkhwa_Province = Province_Khyber_Pakhtunkhwa,
    Sindh_Province = Province_Sindh,
    OtherProvince = Province_OtherProvince,
    Karachi_City = City_Karachi,
    OtherCity = City_OtherCity,
    CPI_Maoist = `Group_Communist Party of India - Maoist (CPI-Maoist)`,
  )

# Remove unneeded parts of column names

names(SA_Binary) = gsub(pattern = "Group_", replacement = "", x = names(SA_Binary))
names(SA_Binary) = gsub(pattern = "Target_", replacement = "", x = names(SA_Binary))
names(SA_Binary) = gsub(pattern = "Attack_", replacement = "", x = names(SA_Binary))
names(SA_Binary) = gsub(pattern = "Weapon_", replacement = "", x = names(SA_Binary))
names(SA_Binary) = gsub(pattern = "Nationality_", replacement = "", x = names(SA_Binary))
names(SA_Binary) = gsub(pattern = "Country_", replacement = "", x = names(SA_Binary))
names(SA_Binary) = gsub(pattern = "Province_", replacement = "", x = names(SA_Binary))
names(SA_Binary) = gsub(pattern = "City_", replacement = "", x = names(SA_Binary))

names(SA_Binary)

glimpse(SA_Binary)

SA_Binary <- as.data.frame(SA_Binary)

##################################
# Convert variables into factors #
##################################

names <- names(SA_Binary)
SA_Binary[names] <- lapply(SA_Binary[names], factor)
str(SA_Binary)

names(SA_Binary)

# Construct the network #
set.seed(123)
Tabu_SA <- tabu(SA_Binary, 
               blacklist = BN_BL)
Tabu_SA

# We first need to construct the network, which the following code achieves

df <- data.frame(
  col1 = c(),
  col2 = c(),
  stringsAsFactors = FALSE
)

relationships <- c(
  "[Pakistan|OtherGroup:TTP]",
  "[OtherWeapon|OtherGroup:Pakistan]",
  "[Maoists|OtherWeapon:Pakistan]",
  "[Balochistan_Province|OtherGroup:TTP:OtherWeapon]",
  "[Khyber_Pakhtunkhwa_Province|OtherGroup:TTP:OtherWeapon]",
  "[OtherCountry|Maoists:OtherGroup:OtherWeapon]",
  "[Firearms|OtherCountry:Pakistan:Khyber_Pakhtunkhwa_Province]",
  "[OtherProvince|OtherGroup:TTP:Firearms:OtherWeapon]",
  "[CPI_Maoist|OtherWeapon:OtherCountry:Pakistan:OtherProvince]",
  "[India|CPI_Maoist:Maoists:OtherGroup:OtherWeapon]",
  "[India_Nationality|CPI_Maoist:Maoists:OtherGroup:OtherWeapon]",
  "[LTTE|India:OtherCountry:Pakistan]",
  "[SriLanka|LTTE:OtherGroup:Firearms:OtherWeapon]",
  "[Taliban|OtherWeapon:India:OtherCountry:Pakistan:SriLanka]",
  "[Afghanistan|OtherGroup:Taliban:Firearms:OtherWeapon]",
  "[Transportation|Firearms:Afghanistan:Khyber_Pakhtunkhwa_Province]",
  "[HostageKidnapAttack|OtherGroup:Transportation:India]",
  "[Lethal|OtherGroup:HostageKidnapAttack:Firearms:Afghanistan:India_Nationality]",
  "[ArmedAssaultAttack|OtherGroup:India:OtherProvince:Lethal]",
  "[Pakistan_Nationality|OtherGroup:TTP:ArmedAssaultAttack:OtherWeapon:Lethal]",
  "[Sindh_Province|OtherGroup:TTP:ArmedAssaultAttack]",
  "[Karachi_City|OtherGroup:TTP:ArmedAssaultAttack:Firearms]",
  "[Explosives|OtherGroup:Transportation:Afghanistan:Pakistan_Nationality:Karachi_City:Lethal]",
  "[OtherTarget|OtherGroup:ArmedAssaultAttack:Explosives:India_Nationality:Lethal]",
  "[Sri_Lanka_Nationality|LTTE:OtherGroup:Explosives]",
  "[OtherCity|OtherGroup:TTP:ArmedAssaultAttack:Explosives]",
  "[Afghanistan_Nationality|OtherGroup:Taliban:OtherTarget:Explosives:Lethal]",
  "[OtherNationality|Maoists:OtherGroup:OtherTarget:OtherWeapon:Lethal]",
  "[Police|OtherGroup:ArmedAssaultAttack:Explosives:India:Afghanistan_Nationality:OtherProvince]",
  "[BombAttack|OtherGroup:OtherTarget:Transportation:Afghanistan:OtherNationality:Lethal]",
  "[Private|Taliban:BombAttack:HostageKidnapAttack:Afghanistan_Nationality:Lethal]",
  "[OtherAttack|OtherGroup:Police:Afghanistan:Pakistan_Nationality:Lethal]",
  "[Jammu_Kashmir_Province|OtherGroup:Police:Firearms:Lethal]",
  "[GovtGen|ArmedAssaultAttack:BombAttack:HostageKidnapAttack:OtherAttack:Pakistan]",
  "[Assassination|GovtGen:OtherTarget:Private:Afghanistan:Lethal]",
  "[Business|OtherGroup:Assassination:OtherAttack:Afghanistan]"
)

for (rel in relationships) {
  parts <- strsplit(gsub("\\[|\\]", "", rel), "\\|")
  child_var <- parts[[1]][1]
  if (length(parts[[1]]) > 1) {
    parent_vars <- unlist(strsplit(parts[[1]][2], ":"))
    for (parent_var in parent_vars) {
      df <- rbind(df, data.frame(col1 = parent_var, col2 = child_var))
    }
  }
}

# Rename columns
df <- df %>%
  rename(From = col1, To = col2)

print(df)

library(DiagrammeR)
library(grid)
uniquenodes <- unique(c(df$From,df$To))

nodes <- create_node_df(n = length(uniquenodes),
                        type = "number",
                        label = uniquenodes,
                        color = "black",
                        fillcolor = "steelblue",
                        fontcolor = "black", 
                        fontname = "Arial")

edges <- create_edge_df(from = match(df$From, uniquenodes),
                        to = match(df$To, uniquenodes),
                        rel = "related",
                        color = "silver",
                        length = rep(200, nrow(df)),  # Set length to 100 for all rows
                        arrowhead = "vee")

# Your data and processing steps for df, uniquenodes, nodes, and edges...


# Create the graph
g <- create_graph(
  nodes_df = nodes,
  edges_df = edges,
  graph_name = "South Asia Terrorism Network",
  
)

# Render the graph
rendered_graph <- render_graph(g, layout = "kk", title = "South Asia Terrorism Network",
            width = 1000,
            height = 800)

rendered_graph

###########################################################
# Estimating the parameters: Conditional Probability Tables
###########################################################

# Maximum Likelihood Estimates

Tabu_SA_bn.mle <- bn.fit(Tabu_SA, SA_Binary, method = "mle")
Tabu_SA_bn.mle

# Bayesian Estimation #

Tabu_SA_bn.bayes <- bn.fit(Tabu_SA, SA_Binary, method = "bayes", iss = 10)
Tabu_SA_bn.bayes
# The below tables can be interpreted in the same manner as above, the only difference being a Bayesian approach has been used 
# to determine the probabilities

# CPD's of specific variables:
# Lethal
Tabu_SA_bn.bayes$Lethal

# Pakistan
Tabu_SA_bn.bayes$Pakistan

arc.strength(Tabu_SA, data = SA_Binary, criterion = "x2")
# In the below list, we can see that all the arcs have very significant p-values, and so, their inclusion is supported

SA1 <- select(SA, Group, Target, Attack, Weapon, Country, Nationality, Province, City, Lethal)   
SA1_BL <- matrix(c("Attack", "Weapon",
                  "Weapon", "Attack",
                   "Country", "Nationality",
                  "Nationality", "Country",
                   "Country", "Province",
                  "Province", "Country",
                   "Country", "City",
                  "City", "Country",
                   "Nationality", "Province",
                   "Province", "Nationality",
                   "Nationality", "City",
                   "City", "Nationality",
                    "Province", "City",
                   "City", "Province"
                  ), ncol = 2, 
byrow = TRUE,
dimnames = list(NULL, c("from", "to")))

glimpse(SA1)

# Construct the network #
set.seed(123)
Tabu_SA1 <- tabu(SA1, 
               blacklist = SA1_BL)
Tabu_SA1

# Bayesian Estimation #

Tabu_SA1_bn.bayes <- bn.fit(Tabu_SA1, SA1, method = "bayes", iss = 10)
Tabu_SA1_bn.bayes

library(gRain)
Tabu_SA1_EI <- compile(as.grain(Tabu_SA1_bn.bayes))

# the below code reveals the probability of each state of Lethal. So, given the data as a whole, there is 52% probability an 
# attack will not be lethal
querygrain(Tabu_SA1_EI, nodes = "Lethal")$Lethal


# Let us now compare lethal probabilities in Pakistan
Pakistan1_EI <- setEvidence(Tabu_SA1_EI, nodes = "Pakistan", states = "1")
querygrain(Pakistan1_EI, nodes = "Lethal")$Lethal
# There is no difference with the data as a whole

BA_EI <- setEvidence(Tabu_SA1_EI, nodes = "Attack", states = "BombAttack")
querygrain(BA_EI, nodes = "Lethal")$Lethal
# When bomb attack is used, there is a 33% probability it will be lethal, compared with 47.62% for the data as a whole

# Now let us determine the probabilities of attacks against GovtGen targets by each attack type and in each province.
jedu <- setEvidence(Tabu_SA1_EI, nodes = "Target", states = "GovtGen")
SxT.cpt <- querygrain(jedu, nodes = c("Attack", "Province"), type = "joint")
SxT.cpt

# let us determine the distribution of the first node (Attack) in nodes conditional on
# the other nodes (Province) in nodes (and, of course, on the evidence we specified with setEvidence).
jedu <- setEvidence(Tabu_SA1_EI, nodes = "Target", states = "GovtGen")
SxT.cptC <- querygrain(jedu, nodes = c("Attack", "Province"), type = "conditional")
SxT.cptC
# Note how the probabilities in each column sum up to 1, as they are computed conditional on the value that Province 
# assumes in that particular column
# The probabilities are not all the same for each row. This means that the conditional probabilities are not identical across 
# all columns per row, regardless of the value of Province we are conditioning on. Hence Hence Attack type is not independent 
# from Province conditional on Target; knowing the type of attack is informative of the province location of attack if 
# we know the target is GovtGen

# Let us determine the probability of a bombattack in Sindh against a police target
#  The quality of the approximation can be improved using theargument n to increase the number of random observations from 
# the default 5000 * nparams(bn) to 106
cpquery(Tabu_SA1_bn.bayes, event = (Attack == "BombAttack") & (Province == "Sindh"), evidence = (Target == "Police"), n = 10^6)

# A better approach is likelihood weighting, which generates random observations in such a way that all of them match 
# the evidence, and re-weights them appropriately when computing the conditional probability for the query. It can be accessed
# from cpquery by setting method = "lw".
cpquery(Tabu_SA1_bn.bayes, event = (Attack == "BombAttack") & (Province == "Sindh"), evidence = list(Target == "Police"), method = "lw")

# Select rows where "Lethal" appears in the "From" or "To" columns
lethal_rows <- df %>%
  filter(From == "Lethal" | To == "Lethal")

print(lethal_rows)

uniquenodes <- unique(c(lethal_rows$From, lethal_rows$To))

nodes <- create_node_df(n = length(uniquenodes),
                        type = "number",
                        label = uniquenodes,
                        color = "black",
                        fillcolor = "steelblue",
                        fontcolor = "black", 
                        fontname = "Arial")

edges <- create_edge_df(from = match(lethal_rows$From, uniquenodes),
                        to = match(lethal_rows$To, uniquenodes),
                        rel = "related",
                        color = "silver",
                        length = rep(200, nrow(df)),  # Set length to 100 for all rows
                        arrowhead = "vee")

# Your data and processing steps for df, uniquenodes, nodes, and edges...


# Create the graph
lethal_graph <- create_graph(
  nodes_df = nodes,
  edges_df = edges,
  graph_name = "Lethal Terrorism Network in South Asia",
  
)

# Render the graph
render_graph(lethal_graph, layout = "fr", title = "Lethal Terrorism Network in South Asia",
            width = 800,
            height = 500)
