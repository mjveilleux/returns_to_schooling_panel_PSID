


#################################################
#########   Housekeeping  #######################
#################################################


packages = c("tidyverse", "lubridate","foreign",
             "zoo", "sjlabelled", "haven")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

#This script cleans the data for the HT 81 estimation for 1981-1992 century returns to education

#years 1981-1992

#variables needed
# experience :
# completed education: 
# sex: ER32000    "SEX OF INDIVIDUAL": 1 = male, 2=female
# race: cannot find, can guess what the effects are given past studies
# marital status: marital:  married - 1,2, not married = 0
# unemployed t-1 : employment status =3, if =1 then working:: filter for only emp status equal to 3 and 1, then change names
# age

#NOTES
# income is total income- does not account for mixed income streams
#experience is calculated as age-comp_edu-5, same as HT81
# income and hrs/wks are taken from the next years so to represent current year. Account for this

setwd("~/Dropbox/Emetrics_Project/psid_code_data")
#read in data then make an id variable for each indiviudal using family number *100000 + person number
IND2019ER <- read_dta("IND2019ER.dta") %>% mutate(id = ER30001*10000+ER30002)



#we are going to make a dataframe for each year then row bind each of them
#in retrospect, I should have made a function then inpu the variables (not DRY)
#SCROLL DOWN TILL YOU SEE THE COMMENT SECTION

df81 <- IND2019ER  %>%
                select(
                        id,
                        yr_born = ER30404, #year individual was born
                        mar = ER30347, #marital status
                        comp_edu = ER30356, #completed education
                        emp_1 = ER30323, #employed in the previous year?
                        hrs_ann = ER30388, #how many hours did you work this year? from 1982 survey
                        income=ER30386, #what was your total income? from 1982 survery
                        sex = ER32000) %>% #male or female?
                filter(
                        comp_edu != 98, #dont know
                        comp_edu != 99, #not applicable
                        comp_edu != 0, #must have some eduation
                        hrs_ann >0, #must have worked some hours this year
                        income>0, #must have made some money this yaer
                        emp_1!=0)  %>% #must have appeared in last survey 
                mutate(
                         year = 1981, #make a column of the year 
                         age = year-yr_born, #better value for age, since survey's happen throughout year those in 81 could still be same age in 82
                         exp = age-comp_edu-5, # this is how hausman and taylor identify experience
                         wage = income/hrs_ann, # income / hrs worked in yearly
                         lnwage= log(wage), #log the wage
                         sex = as.factor(sex), #factor is easier to manipulate as categorical variables in stata
                         sex = str_replace_all(sex, '1', 'M'), # M = male
                         sex = str_replace_all(sex, '2', 'F'), # F = female
                         mar = as.factor(mar),
                         mar = str_replace_all(mar, '0', 'S'), # Single
                         mar = str_replace_all(mar, '1', 'M'), # Married
                         mar = str_replace_all(mar, '2', 'M'),
                         mar = str_replace_all(mar, '3', 'M'),
                         mar = str_replace_all(mar, '4', 'M'),
                         emp_1 = as.factor(emp_1),
                         emp_1 = str_replace_all(emp_1, '1', 'W'), #Working 
                         emp_1 = str_replace_all(emp_1, '2', 'U'), # unemployed
                         emp_1 = str_replace_all(emp_1, '3', 'U'))

# I repeat this for the rest of the years
  
  


df82 <- IND2019ER  %>%
  select(
    id,
    yr_born = ER30404,
    mar = ER30377,
    comp_edu = ER30384,
    emp_1 = ER30353,
    hrs_ann = ER30417,
    income=ER30415  ,
    sex = ER32000) %>% 
  filter(
    comp_edu != 98,
    comp_edu != 99,
    comp_edu != 0,
    hrs_ann >0,
    income>0,
    emp_1!=0) %>% 
  mutate(
    year = 1982,
    age = year-yr_born,
    exp = age-comp_edu-5,
    wage = income/hrs_ann,
    lnwage= log(wage),
    sex = as.factor(sex),
    sex = str_replace_all(sex, '1', 'M'),
    sex = str_replace_all(sex, '2', 'F'),
    mar = as.factor(mar),
    mar = str_replace_all(mar, '0', 'S'),
    mar = str_replace_all(mar, '1', 'M'),
    mar = str_replace_all(mar, '2', 'M'),
    mar = str_replace_all(mar, '3', 'M'),
    mar = str_replace_all(mar, '4', 'M'),
    emp_1 = as.factor(emp_1),
    emp_1 = str_replace_all(emp_1, '1', 'W'),
    emp_1 = str_replace_all(emp_1, '2', 'U'),
    emp_1 = str_replace_all(emp_1, '3', 'U'))





df83 <- IND2019ER  %>%
  select(
    id,
    yr_born = ER30404,
    mar = ER30405,
    comp_edu =ER30413 ,
    emp_1 = ER30382,
    hrs_ann = ER30447,
    income= ER30445,
    sex = ER32000) %>% 
  filter(
    comp_edu != 98,
    comp_edu != 99,
    comp_edu != 0,
   
    hrs_ann >0,
    income>0,
    emp_1!=0) %>% 
  mutate(
    year = 1983,
    age = year-yr_born,
    exp = age-comp_edu-5,
    wage = income/hrs_ann,
    lnwage= log(wage),
    sex = as.factor(sex),
    sex = str_replace_all(sex, '1', 'M'),
    sex = str_replace_all(sex, '2', 'F'),
    mar = as.factor(mar),
    mar = str_replace_all(mar, '0', 'S'),
    mar = str_replace_all(mar, '1', 'M'),
    mar = str_replace_all(mar, '2', 'M'),
    mar = str_replace_all(mar, '3', 'M'),
    mar = str_replace_all(mar, '4', 'M'),
    emp_1 = as.factor(emp_1),
    emp_1 = str_replace_all(emp_1, '1', 'W'),
    emp_1 = str_replace_all(emp_1, '2', 'U'),
    emp_1 = str_replace_all(emp_1, '3', 'U'))




df84 <- IND2019ER  %>%
  select(
    id,
    yr_born = ER30404,
    mar = ER30435,
    comp_edu = ER30443,
    emp_1 = ER30411,
    hrs_ann =ER30482 ,
    income= ER30480,
    sex = ER32000) %>% 
  filter(
    comp_edu != 98,
    comp_edu != 99,
    comp_edu != 0,
   
    hrs_ann >0,
    income>0,
    emp_1!=0) %>% 
  mutate(
    year = 1984,
    age = year-yr_born,
    exp = age-comp_edu-5,
    wage = income/hrs_ann,
    lnwage= log(wage),
    sex = as.factor(sex),
    sex = str_replace_all(sex, '1', 'M'),
    sex = str_replace_all(sex, '2', 'F'),
    mar = as.factor(mar),
    mar = str_replace_all(mar, '0', 'S'),
    mar = str_replace_all(mar, '1', 'M'),
    mar = str_replace_all(mar, '2', 'M'),
    mar = str_replace_all(mar, '3', 'M'),
    mar = str_replace_all(mar, '4', 'M'),
    emp_1 = as.factor(emp_1),
    emp_1 = str_replace_all(emp_1, '1', 'W'),
    emp_1 = str_replace_all(emp_1, '2', 'U'),
    emp_1 = str_replace_all(emp_1, '3', 'U'))






df85 <- IND2019ER  %>%
  select(
    id,
    yr_born = ER30404,
    mar = ER30469,
    comp_edu = ER30478,
    emp_1 = ER30441,
    hrs_ann = ER30517,
    income= ER30515,
    sex = ER32000) %>% 
  filter(
    comp_edu != 98,
    comp_edu != 99,
    comp_edu != 0,
   
    hrs_ann >0,
    income>0,
    emp_1!=0) %>% 
  mutate(
    year = 1985,
    age=year-yr_born, 
    exp = age-comp_edu-5,
    wage = income/hrs_ann,
    lnwage= log(wage),
    sex = as.factor(sex),
    sex = str_replace_all(sex, '1', 'M'),
    sex = str_replace_all(sex, '2', 'F'),
    mar = as.factor(mar),
    mar = str_replace_all(mar, '0', 'S'),
    mar = str_replace_all(mar, '1', 'M'),
    mar = str_replace_all(mar, '2', 'M'),
    mar = str_replace_all(mar, '3', 'M'),
    mar = str_replace_all(mar, '4', 'M'),
    emp_1 = as.factor(emp_1),
    emp_1 = str_replace_all(emp_1, '1', 'W'),
    emp_1 = str_replace_all(emp_1, '2', 'U'),
    emp_1 = str_replace_all(emp_1, '3', 'U'))








df86 <- IND2019ER  %>%
  select(
    id,
    yr_born = ER30404,
    mar = ER30504,
    comp_edu = ER30513,
    emp_1 = ER30474,
    hrs_ann = ER30553,
    income= ER30551,
    sex = ER32000) %>% 
  filter(
    comp_edu != 98,
    comp_edu != 99,
    comp_edu != 0,
   
    hrs_ann >0,
    income>0,
    emp_1!=0) %>% 
  mutate(
    year = 1986,
    age= year-yr_born,
    exp = age-comp_edu-5,
    wage = income/hrs_ann,
    lnwage= log(wage),
    sex = as.factor(sex),
    sex = str_replace_all(sex, '1', 'M'),
    sex = str_replace_all(sex, '2', 'F'),
    mar = as.factor(mar),
    mar = str_replace_all(mar, '0', 'S'),
    mar = str_replace_all(mar, '1', 'M'),
    mar = str_replace_all(mar, '2', 'M'),
    mar = str_replace_all(mar, '3', 'M'),
    mar = str_replace_all(mar, '4', 'M'),
    emp_1 = as.factor(emp_1),
    emp_1 = str_replace_all(emp_1, '1', 'W'),
    emp_1 = str_replace_all(emp_1, '2', 'U'),
    emp_1 = str_replace_all(emp_1, '3', 'U'))





df87 <- IND2019ER  %>%
  select(
    id,
    yr_born = ER30404,
    mar = ER30541,
    comp_edu = ER30549,
    emp_1 = ER30509,
    hrs_ann = ER30588,
    income= ER30586,
    sex = ER32000) %>% 
  filter(
    comp_edu != 98,
    comp_edu != 99,
    comp_edu != 0,
   
    hrs_ann >0,
    income>0,
    emp_1!=0) %>% 
  mutate(
    year = 1987,
    age = year-yr_born,
    exp = age-comp_edu-5,
    wage = income/hrs_ann,
    lnwage= log(wage),
    sex = as.factor(sex),
    sex = str_replace_all(sex, '1', 'M'),
    sex = str_replace_all(sex, '2', 'F'),
    mar = as.factor(mar),
    mar = str_replace_all(mar, '0', 'S'),
    mar = str_replace_all(mar, '1', 'M'),
    mar = str_replace_all(mar, '2', 'M'),
    mar = str_replace_all(mar, '3', 'M'),
    mar = str_replace_all(mar, '4', 'M'),
    emp_1 = as.factor(emp_1),
    emp_1 = str_replace_all(emp_1, '1', 'W'),
    emp_1 = str_replace_all(emp_1, '2', 'U'),
    emp_1 = str_replace_all(emp_1, '3', 'U'))




df88 <- IND2019ER  %>%
  select(
    id,
    yr_born = ER30404,
    mar = ER30576,
    comp_edu = ER30584,
    emp_1 = ER30545,
    hrs_ann = ER30624,
    income= ER30622,
    sex = ER32000) %>% 
  filter(
    comp_edu != 98,
    comp_edu != 99,
    comp_edu != 0,
   
    hrs_ann >0,
    income>0,
    emp_1!=0) %>% 
  mutate(
    year = 1988,
    age = year-yr_born,  
    exp = age-comp_edu-5,
    wage = income/hrs_ann,
    lnwage= log(wage),
    sex = as.factor(sex),
    sex = str_replace_all(sex, '1', 'M'),
    sex = str_replace_all(sex, '2', 'F'),
    mar = as.factor(mar),
    mar = str_replace_all(mar, '0', 'S'),
    mar = str_replace_all(mar, '1', 'M'),
    mar = str_replace_all(mar, '2', 'M'),
    mar = str_replace_all(mar, '3', 'M'),
    mar = str_replace_all(mar, '4', 'M'),
    emp_1 = as.factor(emp_1),
    emp_1 = str_replace_all(emp_1, '1', 'W'),
    emp_1 = str_replace_all(emp_1, '2', 'U'),
    emp_1 = str_replace_all(emp_1, '3', 'U'))



df89 <- IND2019ER  %>%
  select(
    id,
    yr_born = ER30404,
    mar = ER30612,
    comp_edu = ER30620,
    emp_1 = ER30580,
    hrs_ann =ER30661 ,
    income= ER30659,
    sex = ER32000) %>% 
  filter(
    comp_edu != 98,
    comp_edu != 99,
    comp_edu != 0,
   
    hrs_ann >0,
    income>0,
    emp_1!=0) %>% 
  mutate(
    year = 1989,
    age = year-yr_born,  
    exp = age-comp_edu-5,
    wage = income/hrs_ann,
    lnwage= log(wage),
    sex = as.factor(sex),
    sex = str_replace_all(sex, '1', 'M'),
    sex = str_replace_all(sex, '2', 'F'),
    mar = as.factor(mar),
    mar = str_replace_all(mar, '0', 'S'),
    mar = str_replace_all(mar, '1', 'M'),
    mar = str_replace_all(mar, '2', 'M'),
    mar = str_replace_all(mar, '3', 'M'),
    mar = str_replace_all(mar, '4', 'M'),
    emp_1 = as.factor(emp_1),
    emp_1 = str_replace_all(emp_1, '1', 'W'),
    emp_1 = str_replace_all(emp_1, '2', 'U'),
    emp_1 = str_replace_all(emp_1, '3', 'U'))






df90 <- IND2019ER  %>%
  select(
    id,
    yr_born = ER30404,
    mar = ER30648,
    comp_edu = ER30657,
    emp_1 = ER30616,
    hrs_ann = ER30709,
    income= ER30705,
    sex = ER32000) %>% 
  filter(
    comp_edu != 98,
    comp_edu != 99,
    comp_edu != 0,
   
    hrs_ann >0,
    income>0,
    emp_1!=0) %>% 
  mutate(
    year = 1990,
    age = year-yr_born,  
    exp = age-comp_edu-5,
    wage = income/hrs_ann,
    lnwage= log(wage),
    sex = as.factor(sex),
    sex = str_replace_all(sex, '1', 'M'),
    sex = str_replace_all(sex, '2', 'F'),
    mar = as.factor(mar),
    mar = str_replace_all(mar, '0', 'S'),
    mar = str_replace_all(mar, '1', 'M'),
    mar = str_replace_all(mar, '2', 'M'),
    mar = str_replace_all(mar, '3', 'M'),
    mar = str_replace_all(mar, '4', 'M'),
    emp_1 = as.factor(emp_1),
    emp_1 = str_replace_all(emp_1, '1', 'W'),
    emp_1 = str_replace_all(emp_1, '2', 'U'),
    emp_1 = str_replace_all(emp_1, '3', 'U'))

##################################################  ##################################################
#Now that we are done with making the dataframes, lets combine them and clean them up some more
##################################################  ##################################################

#combine all the data frames by row bind
df<- df81%>%   
  rbind(df82) %>% 
  rbind(df83) %>% 
  rbind(df84) %>% 
  rbind(df85) %>% 
  rbind(df86) %>% 
  rbind(df87) %>% 
  rbind(df88) %>% 
  rbind(df89) %>%
  rbind(df90) %>% #don't want any negative values
  mutate(emp_1 = str_replace_all(emp_1, '2','U'),#anything other than working full-time is considered unemployed
         emp_1 = str_replace_all(emp_1, '3','U'),
         emp_1 = str_replace_all(emp_1, '4','U'),
         emp_1 = str_replace_all(emp_1, '5','U'),
         emp_1 = str_replace_all(emp_1, '6','U'),
         emp_1 = str_replace_all(emp_1, '7','U'),
         emp_1 = str_replace_all(emp_1, '8','U'),
         emp_1 = str_replace_all(emp_1, '9','U')) 


#to force the assumption that education is time-invariant-- all education as it was  reportedin each year
c<-  df %>% filter(year == 1981) %>% select(id, comp_edu81=comp_edu)
d<-  df %>% filter(year == 1982) %>% select(id, comp_edu82=comp_edu)
e<-  df %>% filter(year == 1983) %>% select(id, comp_edu83=comp_edu)
f<-  df %>% filter(year == 1984) %>% select(id, comp_edu84=comp_edu)
g<-  df %>% filter(year == 1985) %>% select(id, comp_edu85=comp_edu)
h<-  df %>% filter(year == 1986) %>% select(id, comp_edu86=comp_edu)
i<-  df %>% filter(year == 1987) %>% select(id, comp_edu87=comp_edu)
j<-  df %>% filter(year == 1988) %>% select(id, comp_edu88=comp_edu)
k<-  df %>% filter(year == 1989) %>% select(id, comp_edu89=comp_edu)
l<-  df %>% filter(year == 1990) %>% select(id, comp_edu90=comp_edu)

#combine 
df.p<-df %>% 
  left_join(c) %>% 
  left_join(d)%>% 
  left_join(e)%>% 
  left_join(f)%>% 
  left_join(g)%>% 
  left_join(h)%>% 
  left_join(i)%>% 
  left_join(j)%>% 
  left_join(k)%>% 
  left_join(l) %>% 
  mutate(emp_1 = as.factor(emp_1), # make factor, again.
         sex= as.factor(sex),
         mar = as.factor(mar)) %>% filter(age >= 25 & age <=55, #same age range as HT 81
                                          lnwage <5 & lnwage >0)  %>% #get rid of outliers in income estimates
group_by(id) %>%
  filter(all(1981:1990 %in% year)) #take only those individuals who appear in all years 




##################################################  ################################################## 
#Great! now that we have the OG dataset, lets add in supplement data from the Family Public Data Index 
##################################################  ################################################## 
#found here: https://simba.isr.umich.edu/DC/i.aspx
##################################################



#add additional PSID information: Race, Occupation, Union Coverage, and Region
# we are only taking variables from 1985 survey
df_complete <- read_dta("~/Dropbox/Emetrics_Project/add_psid/add_psid_data.dta") %>% 
  select(fam_num = V11118, per_num =V11119, race85 = V11938, occ85= V11912, union_cov85 = V11649, region85 =V12379) %>% #select variables
  na.omit()  %>% #get rid of any empty rows
  mutate(id = fam_num*10000+per_num) %>% #make id that links with th e OG clean dataframe
  select(-fam_num,-per_num) %>% #clean up by taking out variables used to make id
  right_join(df.p, by = 'id') %>% #join to the OG clean dataframe
  na.omit() %>% #take out any values that did not get linked up
  distinct(id,year, .keep_all = T) %>% # get rid any duplicates
  group_by(id) %>%
  filter(all(1981:1990 %in% year))%>% mutate( # take only those individuals who appeared in all years 
  occ85 = as.character(occ85), #clean up occupation var
  occ85 = str_replace_all(occ85, '0','Inap'),
  occ85 = str_replace_all(occ85, '1','Professional'),
  occ85 = str_replace_all(occ85, '2','Manager'),
  occ85 = str_replace_all(occ85, '3','Entrepenuer'),
  occ85 = str_replace_all(occ85, '4','Clerical'),
  occ85 = str_replace_all(occ85, '5','Craftsman'),
  occ85 = str_replace_all(occ85, '6','Operative'),
  occ85 = str_replace_all(occ85, '7','Laborer'),
  occ85 = str_replace_all(occ85, '8','Farmer'),
  occ85 = str_replace_all(occ85, '9','Misc'),
  
  race85 = as.character(race85),#clean up race var
  race85 = str_replace_all(race85, '1','White'),
  race85 = str_replace_all(race85, '2','Black'),
  race85 = str_replace_all(race85, '3','Native'),
  race85 = str_replace_all(race85, '4','Asian'),
  race85 = str_replace_all(race85, '7','Other'),
  race85 = str_replace_all(race85, '9','Inap'),
  
  union_cov85 = as.character(union_cov85), #clean up union coverage var
  union_cov85 = str_replace_all(union_cov85, '0','Covered'),
  union_cov85 = str_replace_all(union_cov85, '1','Not_Covered'),
  union_cov85 = str_replace_all(union_cov85, '5','DK'),
  union_cov85 = str_replace_all(union_cov85, 'Inap','Inap'),
  
  
  region85 = as.character(region85),#clean up region var
  region85 = str_replace_all(region85, '1','Northeast'),
  region85 = str_replace_all(region85, '2','North_Central'),
  region85 = str_replace_all(region85, '3','South'),
  region85 = str_replace_all(region85, '4','West'),
  region85 = str_replace_all(region85, '5','AK_HI'),
  region85 = str_replace_all(region85, '6','Foreign'),
  occ85 = as.factor(occ85),# makes these variables as factors for appropriate dummy variables
  race85 = as.factor(race85),
  union_cov85 = as.factor(union_cov85),
  region85 = as.factor(region85),
  id = as.factor(id)
) %>% filter(occ85 != 'Inap',race85 != 'Inap',union_cov85 != 'Inap',region85 != 'Inap', income >1000 & income <100000) %>% #get rid of any that have an inappropiate label
  group_by(id) %>%                                                                                                #also, rid of anyone who does not know their wages are covered by union
  filter(all(1981:1990 %in% year))


#get rid of any that have an inappropiate label


#write the cleaned data as a .dta to read into stata
write.dta(df_complete, 'clean_81_90.dta')

#Move to stata now and run do.file











