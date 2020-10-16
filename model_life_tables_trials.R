library(strcode)
library("rlang")

#   ____________________________________________________________________________
#   Title: model life table                                                              ####


# here I try to outline in a complete way the whole process. What can we do here ? 
#   
#   
# I basically want to understand if it is possible to use the model life tables to produce life tables estimaties for the educational attainment. 
#  
# 
# For the moment we do not have full life tables from EuroStat so I need to just rely on the life-expectancy 
# (having good results there might be a good way to have good results in general)


#Let's start from the packages: 

library(dplyr)
library(tidyverse)
library(stringr)
library(eurostat)
library(writexl)
library(excelR)

#then there is the first specific package which we need for the first family of model life tables. 

library(LifeTables)

# this is the package which was developped from the paper "Contemporary Model Life Tables for Developed Countries: An Application of Model-based Clustering"
# 
# https://www.csss.uw.edu/research/working-papers/contemporary-model-life-tables-developed-countries-application-model-based

# 
# ok using the package LifeTables we can try to see what we get as life table inputting the life expectancy which we are give
# and then we can compare this with what we have as life expctancies given by the data.
# 


#The functions which we are going to use are the following: 


# first get the appropriate family #
hmd.DA(x=.09, sex="male", child.mort=1, adult.mort=.28)$classification
# then get the appropriate level (alpha) #
alpha.e0(pattern=1, e0.target=59, sex="male")
# put in the family and alpha to model to produce complete schedule #
mortmod(pattern=1, alpha=.034, sex="male")

# ok so the idea would be to see if, inserting the childhood mortality form the normal life table and the life expectancy
# form the ones with the educational attainemtn we could actually get the life table we are looking for !


# Let's prepare the data from eurostat writing a function on the matter !



#   ____________________________________________________________________________
#   the data                                                                ####



# general research command for eurostat stuff
View(search_eurostat(c("educational attainment"), type = "dataset", fixed = TRUE))


#from here we get the life exp taht we will need as testing variable (they are on 1-year intervals)
life_exp_edu <- get_eurostat( "demo_mlexpecedu",
                              time_format = "date",
                              filters = "none",
                              type = "code",
                              select_time = NULL,
                              cache = TRUE,
                              update_cache = FALSE,
                              cache_dir = NULL,
                              compress_file = TRUE,
                              stringsAsFactors = default.stringsAsFactors(), 
                              keepFlags = FALSE)

# from here we have the full history of the life tables. From here we can get the stuff like the child mortality (the deaths counts are starting from the age of 15 so we can not really 
# get them out of the those). We do need to take out stuff which we can use to start the model

life_table <- get_eurostat( "demo_mlifetable",
                            time_format = "date",
                            filters = "none",
                            type = "code",
                            select_time = NULL,
                            cache = TRUE,
                            update_cache = FALSE,
                            cache_dir = NULL,
                            compress_file = TRUE,
                            stringsAsFactors = default.stringsAsFactors(), 
                            keepFlags = FALSE)

#from here we get the number of people in the different countries with the different educational attainment.

counts_edu_3 <- get_eurostat( "lfsq_pgaed",
                              time_format = "date",
                              filters = "none",
                              type = "code",
                              select_time = NULL,
                              cache = TRUE,
                              update_cache = FALSE,
                              cache_dir = NULL,
                              compress_file = TRUE,
                              stringsAsFactors = default.stringsAsFactors(), 
                              keepFlags = FALSE)

#from here we can counte the number of deaths in the different categories. 

deaths_edu <- get_eurostat( "demo_maeduc",
                            time_format = "date",
                            filters = "none",
                            type = "code",
                            select_time = NULL,
                            cache = TRUE,
                            update_cache = FALSE,
                            cache_dir = NULL,
                            compress_file = TRUE,
                            stringsAsFactors = default.stringsAsFactors(), 
                            keepFlags = FALSE)


#   ____________________________________________________________________________
#   The plan                                                                ####

  # --very basic example to start: female childhood mortality Italy 2010--

  # *1 extract the child mortality from the life tables where there is  no educational attainment.
  # *2 extract the adult mortality form the death counts and the pop counts data tables 
  # *3 extract the life expectancy form the life table divided by edu. attainment 
  # *4 insert them into the model and see what comes out
  # *5 compare what we obtain with what we can extract from the life tables with the educational attainment.



##  ............................................................................
##  Step 1                                                                  ####

head(life_table)

life_table %>% distinct(indic_de)

str(life_table)

summary(life_table)


life_table %>% filter(time == "2010-01-01", geo == "IT", age == "Y_LT1", indic_de  == "PROBDEATH", sex == "F") %>% pull(values)


##  ............................................................................
##  Step 2                                                                  ####
# *2 extract the adult mortality form the death counts and the pop counts data tables 


counts_edu <- counts_edu_3


deaths_edu %>% filter( geo =="IT" , time == "2010-01-01", sex=="F") 

counts_edu %>% filter( geo =="IT" , time == "2010-01-01", sex=="F") 



deaths_edu %>% distinct(age)  %>%  pull(age)

# [1] "TOTAL"  "UNK"    "Y1"     "Y10"    "Y11"    "Y12"    "Y13"    "Y14"    "Y15"    "Y16"    "Y17"    "Y18"    "Y19"    "Y2"     "Y20"    "Y21"    "Y22"    "Y23"    "Y24"    "Y25"    "Y26"    "Y27"   
# [23] "Y28"    "Y29"    "Y3"     "Y30"    "Y31"    "Y32"    "Y33"    "Y34"    "Y35"    "Y36"    "Y37"    "Y38"    "Y39"    "Y4"     "Y40"    "Y41"    "Y42"    "Y43"    "Y44"    "Y45"    "Y46"    "Y47"   
# [45] "Y48"    "Y49"    "Y5"     "Y50"    "Y51"    "Y52"    "Y53"    "Y54"    "Y55"    "Y56"    "Y57"    "Y58"    "Y59"    "Y6"     "Y60"    "Y61"    "Y62"    "Y63"    "Y64"    "Y65"    "Y66"    "Y67"   
# [67] "Y68"    "Y69"    "Y7"     "Y70"    "Y71"    "Y72"    "Y73"    "Y74"    "Y75"    "Y76"    "Y77"    "Y78"    "Y79"    "Y8"     "Y80"    "Y81"    "Y82"    "Y83"    "Y84"    "Y85"    "Y86"    "Y87"   
# [89] "Y88"    "Y89"    "Y9"     "Y90"    "Y91"    "Y92"    "Y93"    "Y94"    "Y95"    "Y96"    "Y97"    "Y98"    "Y99"    "Y_LT1"  "Y_OPEN"

counts_edu %>% distinct(age)  %>%  pull(age)
 
# "Y15-19" "Y15-24" "Y15-39" "Y15-59" "Y15-64" "Y15-74" "Y20-24" "Y20-64" "Y25-29" "Y25-39" "Y25-49" "Y25-54" "Y25-59"
# "Y25-64" "Y25-74" "Y30-34" "Y35-39" "Y40-44" "Y40-59" "Y40-64" "Y45-49" "Y50-54"
# "Y50-59" "Y50-64" "Y50-74" "Y55-59" "Y55-64" "Y60-64" "Y65-69"

counts_edu %>% filter(geo =="IT" , time == "2010-01-01", sex=="F") 

# "Y15-39"  "Y40-44"

# ok for the moment we will try to make it out of the stuff we actually have like the counts for these weired ages.

# 
# (pay attention, in the count thing they are in thousands.)


counts <- counts_edu %>% filter(geo =="IT" , time == "2010-01-01", sex=="F", age %in% c("Y15-39","Y40-44")) %>%  group_by(isced11,geo,time) %>% summarise(adult.count.15.to.44 = sum(values*1000))

age_labels <- paste("Y",seq(15,44),sep="")

deaths <- deaths_edu %>% filter(geo =="IT" , time == "2010-01-01", sex=="F", age %in% age_labels) %>%  group_by(isced11,geo,time) %>% summarise(adult.deaths.15.to.44 = sum(values))


deaths_and_counts <- deaths %>% left_join(counts, by=c("isced11"="isced11")) %>% select(isced11, geo.x, adult.deaths.15.to.44, adult.count.15.to.44) %>% mutate(death_prob =adult.deaths.15.to.44/adult.count.15.to.44 )


#now we have also the death probability for the adults in the different age gorups.

##  ............................................................................
##  Step 3                                                                 ####
# extract the life expectancy form the life table divided by edu. attainment 



life_exp_edu %>% filter(time == "2010-01-01", geo == "IT", age == "Y_LT1",  sex == "F") 


##  ............................................................................
##  Step 4                                                                ####
#   insert them into the model and see what comes out

# ok the data we have for this example are: 
#   
#   1q0 (for the moment taken fromt he whole life_table and without difference by edu attainment) : 0.00271
#   adult_prob_dying by edu_attainment :
    
  #   isced11 geo.x adult.deaths.15.to.44 adult.count.15.to.44 death_prob
  # <chr>   <chr>                 <dbl>                <dbl>      <dbl>
  # 1 ED0-2   IT                     2662              4396500   0.000605
  # 2 ED3_4   IT                     1277              5204000   0.000245
  # 3 ED5-8   IT                      245              1908300   0.000128
  # 4 NAP     IT                        0                   NA  NA       
  # 5 TOTAL   IT                     4184             11508800   0.000364
  # 6 UNK     IT                        0                   NA  NA       
    
  #   life exp. by edu attainemnt.
  # 
  # unit  sex   age   isced11 geo   time       values
  # <chr> <chr> <chr> <chr>   <chr> <date>      <dbl>
  # 1 YR    F     Y_LT1 ED0-2   IT    2010-01-01   83.8
  # 2 YR    F     Y_LT1 ED3_4   IT    2010-01-01   86.3
  # 3 YR    F     Y_LT1 ED5-8   IT    2010-01-01   86.7
  # 4 YR    F     Y_LT1 TOTAL   IT    2010-01-01   84.8

 


# child.mort	
# An integer (1-4) to indicate which child mortality indicator is being supplied (1 - 1m0; 2 - 5m0; 3 - 1q0; 4 - 5q0)


### WE START WITH ED0-2

# first get the appropriate family #
family <- hmd.DA(x=0.00271, sex="female", child.mort=3, adult.mort=0.000605)$classification


# then get the appropriate level (alpha) #

alpha.e0 <- alpha.e0(pattern=family, e0.target=83.8, sex="male")


# put in the family and alpha to model to produce complete schedule #
plot(mortmod(pattern=family, alpha=alpha.e0, sex="male"))


mx.examp <- exp(mortmod(pattern=5, alpha=-0.01628192, sex="female"))
plotMLT(mx.out=mx.examp, sex="female", lt.col="All")








