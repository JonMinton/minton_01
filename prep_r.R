# link:
# https://aqmen.shinyapps.io/minton_01/
  

# # preparation
# 
# require(plyr)
# require(reshape2)
# 
# # One variable, one year
# 
# persons <- read.csv(
#   "G:/dropbox/Dropbox/Data/SNS/rearranged_data/by_year/persons.csv"
#   )
# 
# # GR_hspeop : all persons
# # GR_hswork : working age population
# 
# # GP_sapemal : male population
# # GP_sapemalchild : male children
# # gp-sapemalwork : male working age 
# # GP-sapemalpens : male pensionable age
# # GP_sapefem : female population
# # GP_sapefemchild : female children
# # gp-sapefemwork : female working age 
# # GP-sapefempens : female pensionable age
# 
# # HO_allpeople : total number of people
# # HO_councilpeop : total number of people, rented from council
# 
# 
# # HO-dwellings : total number of dwellings 
# # HO-Semidetached : number of semi-detached dwellings
# 
# 
# 
# working_and_child_tax_credits <- read.csv(
#   "G:/dropbox/Dropbox/Data/SNS/rearranged_data/by_year/working_and_child_tax_credits.csv"
#   )
# 
# tenure_households <- read.csv(
#   "G:/dropbox/Dropbox/Data/SNS/rearranged_data/by_year/tenure_households.csv"
#   )
# 
# 
# # First two examples:
# 
# # 1) Working age population out of all population, last available year
# example_pop <- persons
# example_pop <- subset(
#   example_pop,
#   subset=year==1996,
#   select=c("datazone", "GR.hspeop", "GR.hswork")
#   )
# 
# example_pop <- rename(
#   example_pop,
#   replace=c("GR.hspeop"="total_count", "GR.hswork"="workingage_count")
# )
# 
# write.csv(example_pop, file="data/working_age_people_1996.csv")
# # 2) council households out of all households
# example_house <- tenure_households
# # already just for one year ( 2001)
# example_house <- subset(
#   example_house,
#   select=c("datazone", "HO.allhouseholds", "HO.council")
#   )
# 
# example_house <- rename(example_house,
#                         replace=c("HO.allhouseholds"="total_count", "HO.council"="councilhouse_count")
#                         )
# 
# write.csv(example_house, file="data/council_houses_2011.csv")
# 
# 
# 

