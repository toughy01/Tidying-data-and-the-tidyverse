###############################################################################
#                   TASK 1 - HUMAN DEVELOPMENT INDEX                          #
###############################################################################

#LET'S LOAD IN THE REQUIRED PACKAGES...
library(tidyverse)





#LET'S READ IN THE CSV FILE "HUMAN_DEVELOPMENT_INDEX" AS A DATAFRAME
#REMEMBER - WE NEED TO USE THE "READ.CSV" FUNCTION WITH AN EXCEL CSV FILE!
hdi <- read.csv("raw_data/Human_development_index.csv", header = T)





                         ###DATA TIDYING###

#1). LET'S FIRSTLY USE THE "janitor::clean_names()" FUNCTION TO REMOVE ANY
#UPPERCASE LETTERS ETC;
hdi <- hdi %>%
  janitor::clean_names()







#2).AS WE CAN SEE THE DATA IS A MESS - WE HAVE MULTIPLE OBSERVATIONS IN EVERY
#ROW...LET'S USE THE "PIVOT_LONGER" FUNCTION TO SORT THIS OUT...
hdi_tidy <- hdi %>%
  pivot_longer(names_to = "year",
               values_to = "index",
               cols = -c(hdi_rank_2018, country))

#SO NOW WE HAVE SINGLE OBSERVATIONS IN EVERY ROW AND DISTINCT VARIABLES
#IN THE FOUR COLUMNS (HDI RANK 2018, COUNTRY, YEAR AND INDEX)





#3). NOW LET'S REMOVE THE X PORTION OF THE STRINGS IN THE "YEAR COLUMN"...
hdi_tidy <- hdi_tidy %>%
  mutate(year =  str_replace(year, "x", "") %>% as.numeric())
#SO WHAT WE'VE DONE ABOVE IS WE HAVE REPLACED THE PORTION OF THE STRINGS
#WITHIN THE "YEAR" COLUMN THAT HAVE "X" SO X1990 ETC; WITH NOTHING AND THUS
#THE X WILL BE REMOVED FROM THAT COLUMN...





#4). SCROLLING DOWN THE "INDEX" COLUMN WE CAN SEE THAT SOME VALUES ARE MISSING
# DENOTED BY "NA", LET'S REMOVE THESE MISSING VALUES FROM THE DATAFRAME!
#THE "FILTER" FUNCTION AND "IS.NA" FUNCTION WILL BE USEFULE HERE! 
hdi_tidy_no_na <- hdi_tidy %>%
  filter(!is.na(index))

#SO WHAT WE'VE DONE ABOVE IN THE CODE IS WE'VE CREATED A NEW OBJECT
#WHEREBY WE ARE FILTERING OUT OBJECTS THAT DO NOT MATCH THE PARAMETERS
#GIVEN, WHICH ARE !IS.NA. SO ANY VALUE THAT IS "NA" WILL BE REMOVED...




#5). SO LOOKING AT THE DATA WE HAVE INDEX VALUES FOR A VARIETY OF DIFFERENT
# COUNTRIES WITH VARYING SAMPLE SIZES (MORE ANNUAL INDEX RECORDINGS
#IN SOME COUNTRIES THAN OTHERS). SO LET'S USE THE "GROUP_BY" AND
#"SUMMARISE" FUNCTION TO ESTABLISH THE MEAN INDEX VALUES FOR EACH COUNTRY
hdi.summary <- hdi_tidy_no_na %>%
  group_by(country) %>%
  summarise(mean_index = mean(index),
            n = length(index),
            sd = sd(index),
            se = sd / sqrt(n))




#6). NOW LET'S FILTER ON THE 10 LOWEST SCORING COUNTRIES FOR HDI...
hdi_summary_10_lowest <- hdi.summary %>%
  filter(rank(mean_index) < 11)
#SO WHAT WE'VE DONE HERE USING THE "FILTER" FUNCTION AND THE "RANK" FUNCTION
#IS WE'VE SPECIFIED THAT WE ONLY WANT TO LEAVE IN THE MEAN RANKS OF THE
#LOWEST 10 COUNTRIES (<11) = LEAVE IN ALL RANKS BELOW RANK 11 SO THE 
#LOWEST 10 HDI SCORING COUNTRIES ON AVERAGE WITH THE PROVIDED DATA...
#WE'VE NAMED THIS OBJECT DATAFRAME "hdi_summary_10_lowest"





#7). NOW LET'S DO THE SAME WITH THE TOP 10 HIGHEST SCORING COUNTRIES
#ON THE HDI SCALE...
hdi_summary_10_highest <- hdi.summary %>%
  filter(rank(mean_index) > 194)
#SO WE HAVE FILTERED IN ONLY THE RANKS OF THE MEAN THAT ARE ABOVE THE 
#MEAN THAT RANKS 11TH HIGHEST...









#8). LET'S DO A PLOT OF THE MEAN HDI SCORES THE TOP 10 HIGHEST SCORING
#    AND THE TOP 10 LOWEST SCORING COUNTRIES...


#TOP 10 LOWEST SCORING HDI COUNTRIES...
ggplot()+
  geom_point(data = hdi_summary_10_lowest, aes(x = country, y = mean_index))+
  geom_errorbar(data = hdi_summary_10_lowest, aes(x = country, ymin = mean_index - se,
                                                  ymax = mean_index + se))+
  scale_y_continuous(limits = c(0, 0.5),
                     expand = c(0, 0),
                     name = "HDI") +
  scale_x_discrete(expand = c(0, 0),
                   name = "")+
  theme_classic()+
  coord_flip()








#TOP 10 HIGHEST HDI SCORING COUNTRIES...
ggplot()+
  geom_point(data = hdi_summary_10_highest, aes(x = country, y = mean_index))+
  geom_errorbar(data = hdi_summary_10_highest, aes(x = country, 
                                                   ymin = mean_index - se,
                                                   ymax = mean_index + se))+
  scale_y_continuous(limits = c(0.8, 1),
                     expand = c(0, 0),
                     name = "HDI")+
  scale_x_discrete(expand = c(0, 0),
                   name = "")+
  theme_classic()+
  coord_flip()
                    





#LET'S TRY AND BUILD A PIPELINE THAT TAKES ALL OF THE ABOVE DATA TIDYING,
#SUMMARY STATS AND PLOT IN ONE PIPELINE...

