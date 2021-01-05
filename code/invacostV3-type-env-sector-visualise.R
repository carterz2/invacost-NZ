#-------------------------------------------------------------------------------
#Introduce data, setup for analysis, and total cost estimates

library(tidyverse)
library(invacost)

invacost <- data.frame(readxl::read_xlsx("data/invaCost_3.0_complete.xlsx", sheet = "NZ"))
unique_records <- nrow(invacost)
expanded <- invacost::expandYearlyCosts(costdb = invacost,
                                        startcolumn = "Probable_starting_year_adjusted",
                                        endcolumn = "Probable_ending_year_adjusted")
expanded<-expanded %>% filter(Impact_year <= "2017") 
expanded<-expanded %>% filter(Impact_year >= "1960")
expanded_records <- nrow(expanded)

#followed by transfering the costs into billions 
expanded$cost <- as.numeric(gsub(",", "", expanded$Cost_estimate_per_year_2017_USD_exchange_rate)) 
expanded <- expanded[!is.na(expanded$cost),] 
expanded$cost_bil <- (expanded$cost/1000000000) 
sum_total <- sum(expanded$cost_bil)
nz_sum  <- sum_total * 1.4


#filtering for observed costs 
expanded_obs <- expanded[expanded$Implementation %in% c("Observed"),]
observed_numb <-nrow(expanded_obs)
sum(expanded_obs$cost_bil) 
#filtering for highly reliable estimates 
expanded_obs2 <- expanded_obs[expanded_obs$Method_reliability %in% c("High"),] 
reliable_numb <- nrow(expanded_obs2)
sum(expanded_obs2$cost_bil) 

nz.raw.cost.reliable <- calculateRawAvgCosts(expanded_obs2,
                                             minimum.year = 1977, maximum.year = 2017)


nz.raw.cost.reliable<- calculateRawAvgCosts(expanded_obs2,
                                            minimum.year = 1977,
                                            maximum.year = 2017)
total_cost <- (nz.raw.cost.reliable$average.total.cost$total_cost)/1000
nz_total_cost <- (total_cost*1.4)

raw.avgCost.nz <- invacost::rawAvgCost(expanded_obs2,
                                       min.year = 1977,
                                       max.year = 2017)


#-------------------------------------------------------------------------------
#Environmental costs

invacost.nz.reliableObs = expanded_obs2

# (a) Environment
invacost.nz.env.aquatic <- invacost.nz.reliableObs[which(invacost.nz.reliableObs$
                                                           Environment_IAS == "aquatic"), ]
nz.raw.cost.aquatic <-calculateRawAvgCosts(invacost.nz.env.aquatic,
                                           minimum.year = 1977,
                                           maximum.year = 2017)
#Terrestrial
invacost.nz.env.terrestrial <- invacost.nz.reliableObs[which(invacost.nz.reliableObs$
                                                               Environment_IAS == "terrestrial"), ]
nz.raw.cost.terrestrial <-calculateRawAvgCosts(invacost.nz.env.terrestrial,
                                               minimum.year = 1977,
                                               maximum.year = 2017)

#Diverse/Unspecified
invacost.nz.env.diverse <- invacost.nz.reliableObs[which(invacost.nz.reliableObs$
                                                           Environment_IAS == "diverse/Unspecified"), ]
nz.raw.cost.diverse <-calculateRawAvgCosts(invacost.nz.env.diverse,
                                           minimum.year = 1977,
                                           maximum.year = 2017)

#place data in dataframe for subsequent visualisation
environment.array <- c(as.character(invacost.nz.env.aquatic$Environment[1]), 
                       as.character(invacost.nz.env.terrestrial$Environment[1]),
                       as.character(invacost.nz.env.diverse$Environment[1]))
environment.cost <- c(as.numeric(nz.raw.cost.aquatic$average.total.cost$total_cost)/1000,
                      as.numeric(nz.raw.cost.terrestrial$average.total.cost$total_cost)/1000,
                      as.numeric(nz.raw.cost.diverse$average.total.cost$total_cost)/1000)
env.cost.df <- data.frame(environment.array, environment.cost)

#visualise based on environment
env.plot <- ggplot(env.cost.df, aes(x = env.cost.df$environment.array,
                                    y = env.cost.df$environment.cost)) +
  geom_col(aes(fill = env.cost.df$environment), width = 0.95) +
  theme(legend.position = "none")
env.plot + coord_flip()

#-------------------------------------------------------------------------------
#Costing of biological invasions over MS descriptive categories using 
#highly reliable and observed cost data

#(b) Type of cost:

#Damage costs for:
#Aquatic organisms
nz.aquatic.damage <- invacost.nz.env.aquatic[which(invacost.nz.env.aquatic$
                                                     Type_of_cost_merged == "Damage_costs"), ]
aquatic.damage.cost <- calculateRawAvgCosts(nz.aquatic.damage,
                                            minimum.year = 1977,
                                            maximum.year = 2017)

#Terrestrial organisms
nz.terrestrial.damage <- invacost.nz.env.terrestrial[which(invacost.nz.env.terrestrial$
                                                             Type_of_cost_merged == "Damage_costs"), ]
terrestrial.damage.cost <- calculateRawAvgCosts(nz.terrestrial.damage,
                                                minimum.year = 1977,
                                                maximum.year = 2017)
#Diverse/Unspecified organisms
nz.diverse.damage <- invacost.nz.env.diverse[which(invacost.nz.env.diverse$
                                                     Type_of_cost_merged == "Damage_costs"), ]
diverse.damage.cost <- calculateRawAvgCosts(nz.diverse.damage,
                                            minimum.year = 1977,
                                            maximum.year = 2017)

#Management costs for:
#Aquatic organisms
nz.aquatic.management <- invacost.nz.env.aquatic[which(invacost.nz.env.aquatic$
                                                         Type_of_cost_merged == "Management_costs"), ]
aquatic.management.cost <- calculateRawAvgCosts(nz.aquatic.management,
                                                minimum.year = 1977,
                                                maximum.year = 2017)

#Terrestrial organisms
nz.terrestrial.management <- invacost.nz.env.terrestrial[which(invacost.nz.env.terrestrial$
                                                                 Type_of_cost_merged == "Management_costs"), ]
terrestrial.management.cost <- calculateRawAvgCosts(nz.terrestrial.management,
                                                    minimum.year = 1977,
                                                    maximum.year = 2017)
#diverse/unspecified organisms
nz.diverse.management <- invacost.nz.env.diverse[which(invacost.nz.env.diverse$
                                                         Type_of_cost_merged == "Management_costs"), ]
diverse.management.cost <- calculateRawAvgCosts(nz.diverse.management,
                                                minimum.year = 1977,
                                                maximum.year = 2017)

#Mixed costs for:
#Aquatic organisms
nz.aquatic.mixed <- invacost.nz.env.aquatic[which(invacost.nz.env.aquatic$
                                                    Type_of_cost_merged == "Mixed_costs"), ]
aquatic.mixed.cost <- calculateRawAvgCosts(nz.aquatic.mixed,
                                           minimum.year = 1977,
                                           maximum.year = 2017)

#Terrestrial organisms
nz.terrestrial.mixed <- invacost.nz.env.terrestrial[which(invacost.nz.env.terrestrial$
                                                            Type_of_cost_merged == "Mixed_costs"), ]
terrestrial.mixed.cost <- calculateRawAvgCosts(nz.terrestrial.mixed,
                                               minimum.year = 1977,
                                               maximum.year = 2017)
#diverse/unspecified organisms
nz.diverse.mixed <- invacost.nz.env.diverse[which(invacost.nz.env.diverse$
                                                    Type_2 == "Mixed_costs"), ]
diverse.mixed.cost <- calculateRawAvgCosts(nz.diverse.mixed,
                                           minimum.year = 1977,
                                           maximum.year = 2017)

#place data in dataframe for subsequent visualisation
env.array.type <- c(rep(c("Terrestrial"), 3),
                    rep(c("Aquatic"), 3), 
                    rep(c("Diverse/Unspecified"), 3))
cost.name.array <- c(rep(c("Damage costs", "Management costs", "Mixed costs"), 3))
cost.type.array <- c(terrestrial.damage.cost$average.total.cost$total_cost/1000,
                     terrestrial.management.cost$average.total.cost$total_cost/1000,
                     terrestrial.mixed.cost$average.total.cost$total_cost/1000,
                     aquatic.damage.cost$average.total.cost$total_cost/1000,
                     aquatic.management.cost$average.total.cost$total_cost/1000,
                     aquatic.mixed.cost$average.total.cost$total_cost/1000,
                     diverse.damage.cost$average.total.cost$total_cost/1000,
                     diverse.management.cost$average.total.cost$total_cost/1000,
                     diverse.mixed.cost$average.total.cost$total_cost/1000)
type.costing.df <- data.frame(env.array.type, cost.name.array, cost.type.array)

#Visualise based on type of cost:
type.plot <- ggplot(type.costing.df, aes(x = type.costing.df$env.array.type,
                                         y = type.costing.df$cost.type.array)) +
  geom_col(aes(fill = type.costing.df$cost.name.array), width = 0.95)+
  theme_classic() + theme(legend.position = "none")
type.plot

#-------------------------------------------------------------------------------

#(c) Sector costs based on environment for:
#Aquatic environments:
nz.aquatic.agriculture <- invacost.nz.env.aquatic[which(invacost.nz.env.aquatic$
                                                          Impacted_sector == "Agriculture"), ]
aquatic.agriculture.cost <- calculateRawAvgCosts(nz.aquatic.agriculture,
                                                 minimum.year = 1977,
                                                 maximum.year = 2017)

nz.aquatic.agAuthorStake <- invacost.nz.env.aquatic[which(invacost.nz.env.aquatic$
                                                        Impacted_sector == "Agriculture/Authorities-Stakeholders"), ]
aquatic.agAuthorStake.cost <- calculateRawAvgCosts(nz.aquatic.agAuthorStake,
                                               minimum.year = 1977,
                                               maximum.year = 2017)

nz.aquatic.fishPub <- invacost.nz.env.aquatic[which(invacost.nz.env.aquatic$
                                                          Impacted_sector == "Fishery/Public and social welfare"), ]
aquatic.fishPub.cost <- calculateRawAvgCosts(nz.aquatic.fishPub,
                                                 minimum.year = 1977,
                                                 maximum.year = 2017)

nz.aquatic.authorStake <- invacost.nz.env.aquatic[which(invacost.nz.env.aquatic$
                                                       Impacted_sector == "Authorities-Stakeholders"), ]
aquatic.authorStake.cost <- calculateRawAvgCosts(nz.aquatic.authorStake,
                                              minimum.year = 1977,
                                              maximum.year = 2017)

nz.aquatic.health <- invacost.nz.env.aquatic[which(invacost.nz.env.aquatic$
                                                     Impacted_sector == "Health"), ]
aquatic.health.cost <- calculateRawAvgCosts(nz.aquatic.health,
                                            minimum.year = 1977,
                                            maximum.year = 2017)

nz.aquatic.unspecified<- invacost.nz.env.aquatic[which(invacost.nz.env.aquatic$
                                                     Impacted_sector == "Unspecified"), ]
aquatic.unspecified.cost <- calculateRawAvgCosts(nz.aquatic.unspecified,
                                             minimum.year = 1977,
                                             maximum.year = 2017)
nz.aquatic.forestry<- invacost.nz.env.aquatic[which(invacost.nz.env.aquatic$
                                                         Impacted_sector_2 == "Forestry"), ]
aquatic.forestry.cost <- calculateRawAvgCosts(nz.aquatic.forestry,
                                                 minimum.year = 1977,
                                                 maximum.year = 2017)

nz.aquatic.authorStakeHealth<- invacost.nz.env.aquatic[which(invacost.nz.env.aquatic$
                                                      Impacted_sector == "Authorities-Stakeholders/Health"), ]
aquatic.authorStakeHealth.cost <- calculateRawAvgCosts(nz.aquatic.authorStakeHealth,
                                              minimum.year = 1977,
                                              maximum.year = 2017)

nz.aquatic.agFor<- invacost.nz.env.aquatic[which(invacost.nz.env.aquatic$
                                                               Impacted_sector == "Agriculture/Forestry"), ]
aquatic.agFor.cost <- calculateRawAvgCosts(nz.aquatic.agFor,
                                                       minimum.year = 1977,
                                                       maximum.year = 2017)

#Terrestrial environments:
nz.terrestrial.agriculture <- invacost.nz.env.terrestrial[which(invacost.nz.env.terrestrial$
                                                                  Impacted_sector == "Agriculture"), ]
terrestrial.agriculture.cost <- calculateRawAvgCosts(nz.terrestrial.agriculture,
                                                     minimum.year = 1977,
                                                     maximum.year = 2017)

nz.terrestrial.agAuthorStake <- invacost.nz.env.terrestrial[which(invacost.nz.env.terrestrial$
                                                                Impacted_sector == "Agriculture/Authorities-Stakeholders"), ]
terrestrial.agAuthorStake.cost <- calculateRawAvgCosts(nz.terrestrial.agAuthorStake,
                                                   minimum.year = 1977,
                                                   maximum.year = 2017)

nz.terrestrial.fishPub <- invacost.nz.env.terrestrial[which(invacost.nz.env.terrestrial$
                                                                  Impacted_sector == "Fishery/Public and social welfare"), ]
terrestrial.fishPub.cost <- calculateRawAvgCosts(nz.terrestrial.fishPub,
                                                     minimum.year = 1977,
                                                     maximum.year = 2017)

nz.terrestrial.authorStake <- invacost.nz.env.terrestrial[which(invacost.nz.env.terrestrial$
                                                               Impacted_sector == "Authorities-Stakeholders"), ]
terrestrial.authorStake.cost <- calculateRawAvgCosts(nz.terrestrial.authorStake,
                                                  minimum.year = 1977,
                                                  maximum.year = 2017)

nz.terrestrial.health <- invacost.nz.env.terrestrial[which(invacost.nz.env.terrestrial$
                                                             Impacted_sector == "Health"), ]
terrestrial.health.cost <- calculateRawAvgCosts(nz.terrestrial.health,
                                                minimum.year = 1977,
                                                maximum.year = 2017)

nz.terrestrial.unspecified<- invacost.nz.env.terrestrial[which(invacost.nz.env.terrestrial$
                                                             Impacted_sector == "Unspecified"), ]
terrestrial.unspecified.cost <- calculateRawAvgCosts(nz.terrestrial.unspecified,
                                                 minimum.year = 1977,
                                                 maximum.year = 2017)

nz.terrestrial.forestry <- invacost.nz.env.terrestrial[which(invacost.nz.env.terrestrial$
                                                                 Impacted_sector == "Forestry"), ]
terrestrial.forestry.cost <- calculateRawAvgCosts(nz.terrestrial.forestry,
                                                     minimum.year = 1977,
                                                     maximum.year = 2017)

nz.terrestrial.authorStakeHealth <- invacost.nz.env.terrestrial[which(invacost.nz.env.terrestrial$
                                                               Impacted_sector == "Authorities-Stakeholders/Health"), ]
terrestrial.authorStakeHealth.cost <- calculateRawAvgCosts(nz.terrestrial.authorStakeHealth,
                                                  minimum.year = 1977,
                                                  maximum.year = 2017)

nz.terrestrial.agFor <- invacost.nz.env.terrestrial[which(invacost.nz.env.terrestrial$
                                                               Impacted_sector == "Agriculture/Forestry"), ]
terrestrial.agFor.cost <- calculateRawAvgCosts(nz.terrestrial.agFor,
                                                  minimum.year = 1977,
                                                  maximum.year = 2017)

#diverse/unspecified environments:
nz.diverse.agriculture <- invacost.nz.env.diverse[which(invacost.nz.env.diverse$
                                                          Impacted_sector == "Agriculture"), ]
diverse.agriculture.cost <- calculateRawAvgCosts(nz.diverse.agriculture,
                                                 minimum.year = 1977,
                                                 maximum.year = 2017)

nz.diverse.agAuthorStake <- invacost.nz.env.diverse[which(invacost.nz.env.diverse$
                                                          Impacted_sector == "Agriculture/Authorities-Stakeholders"), ]
diverse.agAuthorStake.cost <- calculateRawAvgCosts(nz.diverse.agAuthorStake,
                                                 minimum.year = 1977,
                                                 maximum.year = 2017)

nz.diverse.fishPub <- invacost.nz.env.diverse[which(invacost.nz.env.diverse$
                                                            Impacted_sector == "Fishery/Public and social welfare"), ]
diverse.fishPub.cost <- calculateRawAvgCosts(nz.diverse.fishPub,
                                                   minimum.year = 1977,
                                                   maximum.year = 2017)

nz.diverse.authorStake <- invacost.nz.env.diverse[which(invacost.nz.env.diverse$
                                                          Impacted_sector == "Authorities-Stakeholders"), ]
diverse.authorStake.cost <- calculateRawAvgCosts(nz.diverse.authorStake,
                                                 minimum.year = 1977,
                                                 maximum.year = 2017)

nz.diverse.health <- invacost.nz.env.diverse[which(invacost.nz.env.diverse$
                                                     Impacted_sector == "Health"), ]
diverse.health.cost <- calculateRawAvgCosts(nz.diverse.health,
                                            minimum.year = 1977,
                                            maximum.year = 2017)

nz.diverse.unspecified<- invacost.nz.env.diverse[which(invacost.nz.env.diverse$
                                                         Impacted_sector == "Unspecified"), ]
diverse.unspecified.cost <- calculateRawAvgCosts(nz.diverse.unspecified,
                                                 minimum.year = 1977,
                                                 maximum.year = 2017)

nz.diverse.forestry <- invacost.nz.env.diverse[which(invacost.nz.env.diverse$
                                                       Impacted_sector == "Forestry"), ]
diverse.forestry.cost <- calculateRawAvgCosts(nz.diverse.forestry,
                                              minimum.year = 1977,
                                              maximum.year = 2017)

nz.diverse.authorStakeHealth <- invacost.nz.env.diverse[which(invacost.nz.env.diverse$
                                                       Impacted_sector == "Authorities-Stakeholders/Health"), ]
diverse.authorStakeHealth.cost <- calculateRawAvgCosts(nz.diverse.authorStakeHealth,
                                              minimum.year = 1977,
                                              maximum.year = 2017)

nz.diverse.agFor <- invacost.nz.env.diverse[which(invacost.nz.env.diverse$
                                                                Impacted_sector == "Agriculture/Forestry"), ]
diverse.agFor.cost <- calculateRawAvgCosts(nz.diverse.agFor,
                                                       minimum.year = 1977,
                                                       maximum.year = 2017)

#place data in dataframe for subsequent visualisation
sector.name.array <- c(rep(c("Agriculture",
                             "Agriculture/Authorities-Stakeholders",
                             "Fishery/Public and social welfare",
                             "Authorities-Stakeholders",
                             "Health",
                             "Unspecified",
                             "Forestry",
                             "Authorities-Stakeholders/Health",
                             "Agriculture/Forestry"),3))
env.sector.array <- c(rep(c("Terrestrial"), 9),
                      rep(c("Aquatic"), 9), 
                      rep(c("Diverse/Unspecified"), 9))
cost.sector.array <- c(terrestrial.agriculture.cost$average.total.cost$total_cost,
                       terrestrial.agAuthorStake.cost$average.total.cost$total_cost,
                       terrestrial.fishPub.cost$average.total.cost$total_cost,
                       terrestrial.authorStake.cost$average.total.cost$total_cost,
                       terrestrial.health.cost$average.total.cost$total_cost,
                       terrestrial.unspecified.cost$average.total.cost$total_cost,
                       terrestrial.forestry.cost$average.total.cost$total_cost,
                       terrestrial.authorStakeHealth.cost$average.total.cost$total_cost,
                       terrestrial.agFor.cost$average.total.cost$total_cost,
                       aquatic.agriculture.cost$average.total.cost$total_cost,
                       aquatic.agAuthorStake.cost$average.total.cost$total_cost,
                       aquatic.fishPub.cost$average.total.cost$total_cost,
                       aquatic.authorStake.cost$average.total.cost$total_cost,
                       aquatic.health.cost$average.total.cost$total_cost,
                       aquatic.unspecified.cost$average.total.cost$total_cost,
                       aquatic.forestry.cost$average.total.cost$total_cost,
                       aquatic.authorStakeHealth.cost$average.total.cost$total_cost,
                       aquatic.agFor.cost$average.total.cost$total_cost,
                       diverse.agriculture.cost$average.total.cost$total_cost,
                       diverse.agAuthorStake.cost$average.total.cost$total_cost,
                       diverse.fishPub.cost$average.total.cost$total_cost,
                       diverse.authorStake.cost$average.total.cost$total_cost,
                       diverse.health.cost$average.total.cost$total_cost,
                       diverse.unspecified.cost$average.total.cost$total_cost,
                       diverse.forestry.cost$average.total.cost$total_cost,
                       diverse.authorStakeHealth.cost$average.total.cost$total_cost,
                       diverse.agFor.cost$average.total.cost$total_cost)
sector.costing.df <- data.frame(env.sector.array,sector.name.array, cost.sector.array)

#visualise based on sector costs:
sector.plot <- ggplot(sector.costing.df, aes(x = sector.costing.df$env.sector.array,
                                             y = sector.costing.df$cost.sector.array)) +
  geom_col(aes(fill = sector.costing.df$sector.name.array), width = 0.95)+
  theme_classic() + theme(legend.position = "none")
sector.plot

#-------------------------------------------------------------------------------
#Type of cost for Aquatic environments (separate plot made due to small influence overall)
aquatic.name.array <- c(rep("Aquatic", 3))
aquatic.types <- c("damage", "management", "mixed")
aquatic.type.array <- c(aquatic.damage.cost$average.total.cost$total_cost,
                        aquatic.management.cost$average.total.cost$total_cost,
                        aquatic.mixed.cost$average.total.cost$total_cost)
aquatic.plot.array <- data.frame(aquatic.name.array,aquatic.types, aquatic.type.array)

aquatic.type.plot <- ggplot(aquatic.plot.array, aes(x = aquatic.plot.array$aquatic.name.array,
                                                    y = aquatic.plot.array$aquatic.type.array)) +
  geom_col(aes(fill = aquatic.plot.array$aquatic.types, width = 0.95))+
  theme_classic() + theme(legend.position = "none")
aquatic.type.plot

#Sector costs for Aquatic environments (separate plot made due to small influence overall)
aquatic.name.sectors <- c(rep("Aquatic", 9))
aquatic.sectors <-c("Agriculture", 
                    "Agriculture/Authorities-Stakeholders", 
                    "Fishery/Public and social welfare",
                    "Authorities-Stakeholders", 
                    "Health", 
                    "Unspecified", 
                    "Forestry", 
                    "Authorities-Stakeholders/Health", 
                    "Agriculture/Forestry")
aquatic.sector.array <- c(aquatic.agriculture.cost$average.total.cost$total_cost,
                          aquatic.agAuthorStake.cost$average.total.cost$total_cost,
                          aquatic.fishPub.cost$average.total.cost$total_cost,
                          aquatic.authorStake.cost$average.total.cost$total_cost,
                          aquatic.health.cost$average.total.cost$total_cost,
                          aquatic.unspecified.cost$average.total.cost$total_cost,
                          aquatic.forestry.cost$average.total.cost$total_cost,
                          aquatic.authorStakeHealth.cost$average.total.cost$total_cost,
                          aquatic.agFor.cost$average.total.cost$total_cost)
aquatic.sector.df <- data.frame(aquatic.name.sectors, aquatic.sectors, aquatic.sector.array)

aquatic.sector.plot <- ggplot(aquatic.sector.df, aes(x = aquatic.sector.df$aquatic.name.sectors,
                                                     y = aquatic.sector.df$aquatic.sector.array)) +
  geom_col(aes(fill = aquatic.sector.df$aquatic.sectors, width = 0.95))+
  theme_classic() + theme(legend.position = "none")
aquatic.sector.plot