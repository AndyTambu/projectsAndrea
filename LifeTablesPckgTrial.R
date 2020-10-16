
#install.packages("LifeTables")

library(LifeTables)

# install.packages("demography")
# 
# library(demography)

# 
# ok using the package LifeTables we can try to see what we get as lief table inputting the life expectancy which we are give
# and then we can compare this with what we have as life expctancies given by the data.
# 


#The functions which we are going to use are the following: 


# first get the appropriate family #
hmd.DA(x=.09, sex="male", child.mort=1, adult.mort=.28)$classification
# then get the appropriate level (alpha) #
alpha.e0(pattern=1, e0.target=59, sex="male")
# put in the family and alpha to model to produce complete schedule #
mortmod(pattern=1, alpha=.034, sex="male")



# alpha.e0     --->  Alpha to match life expectancy
# Description
# Finds the alpha value to reproduce a desired life expectancy given a life table family/pattern








# install_github(repo = 'sinafala/svdComp5q0')

library(svdComp5q0)

? `svdComp5q0-package`



####################################################################à

# first get the appropriate family #
hmd.DA(x=.09, sex="male", child.mort=1, adult.mort=.28)$classification
# then get the appropriate level (alpha) #
alpha.e0(pattern=1, e0.target=59, sex="male")
# put in the family and alpha to model to produce complete schedule #
mortmod(pattern=1, alpha=.034, sex="male")

alpha_e0()

? hmd.DA()


#and from here we get the totals.

 

hmd.DA(x=0.00219, sex="female", child.mort=1)$classification

alpha.e0(pattern=5, e0.target=82, sex="female")

exp(mortmod(pattern=5, alpha=-0.01628192, sex="female"))
? mortmod()

mod.lt(0.00219, child.mort=1, e0.target=82, adult.mort=NULL, sex="female", alpha=-0.01628192)


mx.examp <- exp(mortmod(pattern=5, alpha=-0.01628192, sex="female"))
plotMLT(mx.out=mx.examp, sex="female", lt.col="All")

#ok what can I try now ? I can compare 


pop_and_deaths_CK_2017_F %>% filter(isced11 == "ED0-2") %>% filter(is.na(age.group) == FALSE)

pop_and_deaths_CK_2017_F %>% ggplot(aes(age.group, Mortality.rate.100000, group = isced11, color=isced11) ) + geom_line()


mod.lt(0.00219, child.mort=1, e0.target=79.4, adult.mort=NULL, sex="female", alpha=-0.01628192)



life_table_CZ_17_F %>% filter(age == "Y_LT1", indic_de == "DEATHRATE")

life_exp_edu_CZ_17_F %>% filter(age == "Y_LT1")

# age   isced11 values
# <fct> <fct>    <dbl>
#   1 Y_LT1 ED0-2     79.4
# 2 Y_LT1 ED3_4     82.4
# 3 Y_LT1 ED5-8     79.9
# 4 Y_LT1 TOTAL     82 







