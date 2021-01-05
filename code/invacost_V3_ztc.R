#Code used to generate tables and in-text numbers for 
  #Bodey, Carter, Haubrock, Cuthbert, Welsh, Diagne and Courchamp 2020
  #Biological invasions in New Zealand - first steps towards a comprehensive economic cost synthesis
  #Created 20/12/2020

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
environment.array <- c(as.character(invacost.nz.env.aquatic$Environment_IAS[1]), 
                       as.character(invacost.nz.env.terrestrial$Environment_IAS[1]),
                       as.character(invacost.nz.env.diverse$Environment_IAS[1]))
estimate.number <- c(nz.raw.cost.aquatic$parameters$number.of.estimates,
                     nz.raw.cost.terrestrial$parameters$number.of.estimates,
                     nz.raw.cost.diverse$parameters$number.of.estimates)
n.proportion <- c((estimate.number[1]/sum(estimate.number))*100,
                        (estimate.number[2]/sum(estimate.number))*100,
                        (estimate.number[3]/sum(estimate.number))*100)
environment.cost <- c(as.numeric(nz.raw.cost.aquatic$average.total.cost$total_cost),
                      as.numeric(nz.raw.cost.terrestrial$average.total.cost$total_cost),
                      as.numeric(nz.raw.cost.diverse$average.total.cost$total_cost))
environment.cost = environment.cost/1000
nz.env.cost <- c(environment.cost[1]*1.4,
             environment.cost[2]*1.4,
             environment.cost[3]*1.4)
env.cost.proportion <- c((environment.cost[1]/sum(environment.cost))*100,
                     (environment.cost[2]/sum(environment.cost))*100,
                     (environment.cost[3]/sum(environment.cost))*100)

#-------------------------------------------------------------------------------
#Costing over Type

invacost.nz.management <- invacost.nz.reliableObs[which(invacost.nz.reliableObs$
                                                          Type_of_cost_merged == "Management_costs"), ]
management.cost <-calculateRawAvgCosts(invacost.nz.management,
                                           minimum.year = 1977,
                                           maximum.year = 2017)

invacost.nz.damage <- invacost.nz.reliableObs[which(invacost.nz.reliableObs$
                                                          Type_of_cost_merged == "Damage_costs"), ]
damage.cost <-calculateRawAvgCosts(invacost.nz.damage,
                                       minimum.year = 1977,
                                       maximum.year = 2017)

invacost.nz.mixed <- invacost.nz.reliableObs[which(invacost.nz.reliableObs$
                                                          Type_of_cost_merged == "Mixed_costs"), ]
mixed.cost <-calculateRawAvgCosts(invacost.nz.mixed,
                                       minimum.year = 1977,
                                       maximum.year = 2017)

type.names <- c("Management_costs", "Damage_costs", "Mixed_costs")
n.type <- c(management.cost$parameters$number.of.estimates,
            damage.cost$parameters$number.of.estimates,
            mixed.cost$parameters$number.of.estimates)
n.type.proportion <- c((n.type[1]/sum(n.type))*100,
                     (n.type[2]/sum(n.type))*100,
                     (n.type[3]/sum(n.type))*100)
type.cost <- c(management.cost$average.total.cost$total_cost,
               damage.cost$average.total.cost$total_cost,
               mixed.cost$average.total.cost$total_cost)
type.cost = type.cost/1000

nz.type.cost = type.cost *1.4

type.cost.proporiton <- c((type.cost[1]/sum(type.cost))*100,
                          (type.cost[2]/sum(type.cost))*100,
                          (type.cost[3]/sum(type.cost))*100)

#-------------------------------------------------------------------------------
#Costing over sector
invacost.nz.authorities <- invacost.nz.reliableObs[which(invacost.nz.reliableObs$
                                                           Impacted_sector == "Authorities-Stakeholders"), ]
authorities.cost <-calculateRawAvgCosts(invacost.nz.authorities,
                                       minimum.year = 1977,
                                       maximum.year = 2017)

invacost.nz.authorHealth <- invacost.nz.reliableObs[which(invacost.nz.reliableObs$
                                                           Impacted_sector == "Authorities-Stakeholders/Health"), ]
authorHealth.cost <-calculateRawAvgCosts(invacost.nz.authorHealth,
                                        minimum.year = 1977,
                                        maximum.year = 2017)

invacost.nz.agriculture <- invacost.nz.reliableObs[which(invacost.nz.reliableObs$
                                                            Impacted_sector == "Agriculture"), ]
agriculture.cost <-calculateRawAvgCosts(invacost.nz.agriculture,
                                         minimum.year = 1977,
                                         maximum.year = 2017)

invacost.nz.unspecified <- invacost.nz.reliableObs[which(invacost.nz.reliableObs$
                                                           Impacted_sector == "Unspecified"), ]
unspecified.cost <-calculateRawAvgCosts(invacost.nz.unspecified,
                                        minimum.year = 1977,
                                        maximum.year = 2017)

invacost.nz.agFor <- invacost.nz.reliableObs[which(invacost.nz.reliableObs$
                                                           Impacted_sector == "Agriculture/Forestry"), ]
agFor.cost <-calculateRawAvgCosts(invacost.nz.agFor,
                                        minimum.year = 1977,
                                        maximum.year = 2017)

invacost.nz.forestry <- invacost.nz.reliableObs[which(invacost.nz.reliableObs$
                                                     Impacted_sector == "Forestry"), ]
forestry.cost <-calculateRawAvgCosts(invacost.nz.forestry,
                                  minimum.year = 1977,
                                  maximum.year = 2017)

invacost.nz.health <- invacost.nz.reliableObs[which(invacost.nz.reliableObs$
                                                     Impacted_sector == "Health"), ]
health.cost <-calculateRawAvgCosts(invacost.nz.health,
                                  minimum.year = 1977,
                                  maximum.year = 2017)

invacost.nz.fish <- invacost.nz.reliableObs[which(invacost.nz.reliableObs$
                                                      Impacted_sector == "fishery/Public and social welfare"), ]
fish.cost <-calculateRawAvgCosts(invacost.nz.fish,
                                   minimum.year = 1977,
                                   maximum.year = 2017)

invacost.nz.agAuthor <- invacost.nz.reliableObs[which(invacost.nz.reliableObs$
                                                    Impacted_sector == "Agriculture/Authorities-Stakeholders"), ]
agAuthor.cost <-calculateRawAvgCosts(invacost.nz.agAuthor,
                                 minimum.year = 1977,
                                 maximum.year = 2017)



sector.names <- c("Authorities-Stakeholders",
                  "Authorities-Stakeholders/Health",
                  "Agriculture",
                  "Unspecified",
                  "Agriculture/Forestry",
                  "Forestry",
                  "Health",
                  "Fishery/Public and social welfare",
                  "Agriculture/Authorities-stakeholders")
n.sector <- c(authorities.cost$parameters$number.of.estimates,
              authorHealth.cost$parameters$number.of.estimates,
              agriculture.cost$parameters$number.of.estimates,
              unspecified.cost$parameters$number.of.estimates,
              agFor.cost$parameters$number.of.estimates,
              forestry.cost$parameters$number.of.estimates,
              health.cost$parameters$number.of.estimates,
              fish.cost$parameters$number.of.estimates,
              agAuthor.cost$parameters$number.of.estimates)

n.sector.proportion <- c((n.sector[1]/sum(n.sector))*100,
                         (n.sector[2]/sum(n.sector))*100,
                         (n.sector[3]/sum(n.sector))*100,
                         (n.sector[4]/sum(n.sector))*100,
                         (n.sector[5]/sum(n.sector))*100,
                         (n.sector[6]/sum(n.sector))*100,
                         (n.sector[7]/sum(n.sector))*100,
                         (n.sector[8]/sum(n.sector))*100,
                         (n.sector[9]/sum(n.sector))*100)

sector.cost <- c(authorities.cost$average.total.cost$total_cost,
                 authorHealth.cost$average.total.cost$total_cost,
                 agriculture.cost$average.total.cost$total_cost,
                 unspecified.cost$average.total.cost$total_cost,
                 agFor.cost$average.total.cost$total_cost,
                 forestry.cost$average.total.cost$total_cost,
                 health.cost$average.total.cost$total_cost,
                 fish.cost$average.total.cost$total_cost,
                 agAuthor.cost$average.total.cost$total_cost)
sector.cost = sector.cost/1000

nz.sector.cost <- sector.cost * 1.4

sector.cost.proportion <- (sector.cost/(sum(sector.cost)))*100

#-------------------------------------------------------------------------------
#cost over management type

invacost.nz.postInvasion <- invacost.nz.reliableObs[which(invacost.nz.reliableObs$
                                                           Management_type == "Post-invasion_management"), ]
postInvasion.cost <-calculateRawAvgCosts(invacost.nz.postInvasion,
                                        minimum.year = 1977,
                                        maximum.year = 2017)

invacost.nz.knowledge<- invacost.nz.reliableObs[which(invacost.nz.reliableObs$
                                                        Management_type == "Knowledge/funding"), ]
knowledge.cost <-calculateRawAvgCosts(invacost.nz.knowledge,
                                      minimum.year = 1977,
                                      maximum.year = 2017)

invacost.nz.preInvasion <- invacost.nz.reliableObs[which(invacost.nz.reliableObs$
                                                           Management_type == "Pre-invasion_management"), ]
preInvasion.cost <-calculateRawAvgCosts(invacost.nz.preInvasion,
                                     minimum.year = 1977,
                                     maximum.year = 2017)
invacost.nz.mixedManage <- invacost.nz.reliableObs[which(invacost.nz.reliableObs$
                                                           Management_type == "Mixed"), ]
mixedManage.cost <-calculateRawAvgCosts(invacost.nz.mixedManage,
                                        minimum.year = 1977,
                                        maximum.year = 2017)

invacost.nz.naManage <- invacost.nz.reliableObs[which(invacost.nz.reliableObs$
                                                           Management_type == "NA"), ]
naManage.cost <-calculateRawAvgCosts(invacost.nz.naManage,
                                     minimum.year = 1977,
                                     maximum.year = 2017)
  
management.names <- unique(c(invacost.nz.reliableObs$Management_type))
n.management <- c(postInvasion.cost$parameters$number.of.estimates,
                  knowledge.cost$parameters$number.of.estimates,
                  preInvasion.cost$parameters$number.of.estimates,
                  mixedManage.cost$parameters$number.of.estimates,
                  naManage.cost$parameters$number.of.estimates)
management.proportion <- (n.management/sum(n.management))*100

management.cost <- c(postInvasion.cost$average.total.cost$total_cost,
                     knowledge.cost$average.total.cost$total_cost,
                     preInvasion.cost$average.total.cost$total_cost,
                     mixedManage.cost$average.total.cost$total_cost,
                     naManage.cost$average.total.cost$total_cost)
management.cost = management.cost/1000
nz.management.cost <- management.cost * 1.4
management.cost.proportion <- (management.cost/(sum(management.cost))) *100

management.cost.df <- data.frame(management.names,
                                 n.management,
                                 management.proportion,
                                 management.cost,
                                 nz.management.cost,
                                 management.cost.proportion)

#-------------------------------------------------------------------------------
#Compile and summarise findings


db.summary.table <- data.frame(c("Annualised cost entries",
                                 "Reported total",
                                 "NZ reported total",
                                 "n observed costs",
                                 "n reliable costs",
                                 "Total cost",
                                 "NZ total cost"),
                               c(expanded_records,
                                 sum_total,
                                 nz_sum,
                                 observed_numb,
                                 reliable_numb,
                                 total_cost,
                                 nz_total_cost))
colnames(db.summary.table) = c("Description", "Value")

env.cost.df <- data.frame(environment.array, 
                          estimate.number, 
                          n.proportion, 
                          environment.cost,
                          nz.env.cost,
                          env.cost.proportion)

type.cost.df <- data.frame(type.names,
                           n.type,
                           n.type.proportion,
                           type.cost,
                           nz.type.cost,
                           type.cost.proporiton)

sector.cost.df <- data.frame(sector.names,
                             n.sector,
                             n.sector.proportion,
                             sector.cost,
                             nz.sector.cost,
                             sector.cost.proportion)


summary <- list(knitr::kable(db.summary.table,
                             caption = "Summary details for NZ invacost (Results paragraph #1)."),
                knitr::kable(env.cost.df,
                             caption = "Summary details for environmental costs."),
                knitr::kable(type.cost.df,
                             caption = "Summary details for type of costs."),
                knitr::kable(sector.cost.df,
                             caption = "Summary details for sector costs.")
                )



