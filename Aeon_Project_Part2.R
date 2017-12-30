
#Aeon Project Part 2 

library(tidyr)
library(dplyr)
library(arules)
library(arulesViz)
library(data.table)

aeon <- read.csv("C:\\Users\\ox6036qb\\Documents\\Fall 2017\\DSCI 210\\aeon_transactions.csv")

setnames(aeon, old = c('intx_record_type_Connection_to_Community_Resource','intx_subtype_Non.payment_of_rent','event_type_Property_Meeting','intx_resource_Community_Involvement','event_subtype_Educational','event_type_Food_Shelf','intx_record_type_Housing_Stability','lead_job_Bulk_Mail','intx_record_type_Material_Goods','intx_type_Material_Goods','Leadership_Tier_2','lead_job_Event_Assistance','intx_type_Eviction_Prevention','intx_resource_Coats','intx_resource_Household_Goods','intx_resource_Employment','intx_resource_Back_to_School_Supplies'), 
         new = c('Connection to Community','Nonpayment of Rent','Property Meeting','Community Involvement','Educational Event','Food Shelf','Housing Stability Issue','Mail Job','Material Goods Interaction Record','Material Goods Interaction','Leadership Tier 2','Event Assistance','Eviction Prevention','Coat Resource','Household Goods Interaction','Employment Resource','School Supplies Resource'))


head(aeon[,1:6])

#Rules that imply Postitive.MO
aeon_pos <- aeon[,-104]

aeon_rules1 <-
  aeon_pos %>%
  mutate_if(is.integer, as.factor) %>%
  as("transactions") %>%
  apriori(parameter = list(supp = 0.05,
                           conf = 0.20,
                           maxlen = 3)) 

aeon_rules_with_pos <- 
  subset(aeon_rules1, 
         subset = rhs %in% "Positive.MO=1")

high_lift_rules_pos <-
  aeon_rules_with_pos %>%
  sort(by = "lift") %>%
  head(10)

inspect(high_lift_rules_pos)

#Rules that imply Negative.MO
aeon_neg <- aeon[,-105]

aeon_rules2 <-
  aeon_neg %>%
  mutate_if(is.integer, as.factor) %>%
  as("transactions") %>%
  apriori(parameter = list(supp = 0.05,
                           conf = 0.20,
                           maxlen = 3)) 
aeon_rules_with_neg <- 
  subset(aeon_rules2, 
         subset = rhs %in% "Negative.MO=1")

high_lift_rules_neg <-
  aeon_rules_with_neg %>%
  sort(by = "lift") %>%
  head(10)

inspect(high_lift_rules_neg)


#Positive.MO graphs
plot(high_lift_rules_pos, method="grouped")

plot(high_lift_rules_pos, method="graph")

#Negative.MO graphs
plot(high_lift_rules_neg, method="grouped")

plot(high_lift_rules_neg, method="graph")



setnames(aeon, old = c('intx_record_type_Connection_to_Community_Resource','intx_subtype_Non.payment_of_rent','event_type_Property_Meeting','intx_resource_Community_Involvement','event_subtype_Educational','event_type_Food_Shelf','intx_record_type_Housing_Stability','lead_job_Bulk_Mail','intx_record_type_Material_Goods','intx_type_Material_Goods','Leadership_Tier_2','lead_job_Event_Assistance','intx_type_Eviction_Prevention','intx_resource_Coats','intx_resource_Household_Goods','intx_resource_Employmen','intx_resource_Back_to_School_Supplies'), 
         new = c('Connection to Community','Nonpayment of Rent','Property Meeting','Community Involvement','Educational Event','Food Shelf','Housing Stability Issue','Mail Job','Material Goods Interaction Record','Material Goods Interaction','Leadership Tier 2','Event Assistance','Eviction Prevention','Coat Resource','Household Goods Interaction','Employment Resource','School Supplies Resource'))


