#load packages
library(readr)
library(dplyr)

#set working directory to be location of all data files
setwd("C:/Users/mlanza/Desktop/CHOP-master/CHOP-master")

#pull in all fullfilenames in working directory
fullfilenames <- list.files(pattern="*.csv")

#filenames minus extension
datanames <- tools::file_path_sans_ext(fullfilenames)

#creates lists of all data by reading all files in folder
mydata <- lapply(fullfilenames, read_csv)

#loop to assign variables to all datasets and gives names to each dataset in list for subsetting and prints out data
for (i in 1:length(mydata)){
  assign(datanames[i],mydata[[i]])
  names(mydata)[i] <- datanames[i]
}

#Get info on the visit table
str(visit)

#Part 1
#Find patients where visit is a hospital encounter, after August 1, 2014, Age is greater than or equal 1 and less than or equal to 18
hospitalencounters <- visit %>%
                        filter(DICT_ENC_TYPE_KEY == 83 & HOSP_ADMIT_DT > '2014-08-01 00:00:00' & AGE >= 1.0 & AGE <=18.0)

#View diagnosis info
str(diagnosis)

#Set up list of ICD9 Codes for reference to anaphylaxis or allergic reaction
ICD9codes <- list('995.0','995.3','995.6','995.60','995.61','995.62','995.63','995.64','995.65','995.66','995.67','995.68','995.69','995.7','999.4','999.41','999.42','999.49')

#Find all diagnosis that are anaphylaxis from the ICD9codes
anaphylaxisdiagnosis <- diagnosis %>%
                            filter(ICD9_CD %in% ICD9codes)

#Find all ED Primary and Secondary diagnosis visits and all anaphylaxis diagnosis visits
anaphylaxisvisitdiagnosis <- visit_diagnosis %>%
                                      filter(DICT_DX_STS_KEY == 313 | DICT_DX_STS_KEY == 314) %>%
                                      merge(anaphylaxisdiagnosis, by = 'DX_KEY')

#removes all Urgent Care departments from Hospitalencounters and stores DEPT_KEYs
nonurgentdepts = department %>% 
                      filter(!grepl('urgent', department$SPECIALTY, ignore.case = TRUE))

#merge dataframes to find which hospital visits are for anaphylaxis and filter out non urgent departments
cohort <- merge(hospitalencounters, anaphylaxisvisitdiagnosis, by = c('VISIT_KEY', 'PAT_KEY'), suffixes= c('_hospital', '_visit')) %>%
                          filter(DEPT_KEY %in% nonurgentdepts$DEPT_KEY)

#Part 2, create additional features

#create anaphylaxis patient
anaphylaxispatient <- cohort %>%
                        filter(grepl('anaphylaxis', cohort$DX_NM, ignore.case = TRUE)) %>%
                        subset(select = c('VISIT_KEY','PAT_KEY'))

#create ANAPH_DX_IND indicator based on anaphylaxis patient
cohort$ANAPH_DX_IND <- as.numeric(cohort$VISIT_KEY %in% anaphylaxispatient$VISIT_KEY , cohort$PAT_KEY %in% anaphylaxispatient$PAT_KEY )


#merge medication order into cohort
#Find medication orders where epinephrine was ordered
epinephrineorder = medication_order %>%
                            filter(grepl('epinephrine', medication_order$MED_ORD_NM, ignore.case = TRUE)) %>%
                            subset(select = c('VISIT_KEY','PAT_KEY'))

#create EPI_ORDER_IND: finds where Visit_KEY and PAT_KEY from epinephrineorder matches in cohort and applies a 1 to all rows of that visit
cohort$EPI_ORDER_IND <- as.numeric(cohort$VISIT_KEY %in% epinephrineorder$VISIT_KEY , cohort$PAT_KEY %in% epinephrineorder$PAT_KEY )

#creates outpatientvisit dataframe of patients in cohort
outpatientvisit <- visit %>%
                      filter(DICT_ENC_TYPE_KEY == 108, PAT_KEY %in% cohort$PAT_KEY)

#merges cohort visits with outpatient visits and looks at outpatient visits that occured after a hospital visit
hospoutvisit <- merge(subset(cohort, select=c('VISIT_KEY', 'PAT_KEY', 'HOSP_DISCHRG_DT')),subset(outpatientvisit, select=c('VISIT_KEY', 'PAT_KEY', 'APPT_CHECKIN_DT')), by = 'PAT_KEY', suffixes= c('_HOSP', '_OUTP')) %>%
                  filter(HOSP_DISCHRG_DT < APPT_CHECKIN_DT)

#computes the amount of days between the hospital visit and outpatient visit
hospoutvisit$VISIT_DIFF_DAYS <- as.Date(hospoutvisit$APPT_CHECKIN_DT)-as.Date(hospoutvisit$HOSP_DISCHRG_DT)

#creates immediate hosoutvisit to filter out all visits that are not less than or equal to 7 days
immhospoutvisit <- hospoutvisit %>%
                    filter(hospoutvisit$VISIT_DIFF_DAYS <= 7)

#create FOLLOW_UP_IND: finds where Visit_KEY and PAT_KEY from hospoutvisit matches in cohort and applies a 1 to all rows of that visit
cohort$FOLLOW_UP_IND <- as.numeric(cohort$VISIT_KEY %in% immhospoutvisit$VISIT_KEY_HOSP , cohort$PAT_KEY %in% immhospoutvisit$PAT_KEY )

#Find the min date of the outpatient visits that occured within 7 days of a hospital visit
mindate <- immhospoutvisit %>% 
                    group_by(PAT_KEY, VISIT_KEY_HOSP) %>%
                    summarise(FOLLOW_UP_DATE = min(APPT_CHECKIN_DT))

#Merges the cohort with the mindate, fills in na where not found
cohort <- merge(cohort, mindate, by.y = c('PAT_KEY', 'VISIT_KEY_HOSP'), by.x = c('PAT_KEY', 'VISIT_KEY'), all.x = TRUE)

#Number of days between hospital visit and first outpatient visit
mindays <- hospoutvisit %>% 
                  group_by(PAT_KEY, VISIT_KEY_HOSP) %>%
                  summarise(DAYS_TO_FOLLOW_UP = min(VISIT_DIFF_DAYS))

#Merges the cohort with the mindays, fills in na where not found
cohort <- merge(cohort, mindays, by.y = c('PAT_KEY', 'VISIT_KEY_HOSP'), by.x = c('PAT_KEY', 'VISIT_KEY'), all.x = TRUE)

#finalize cohort structure and remove duplicates
cohort <- unique(cohort[c('PAT_KEY', 'VISIT_KEY', 'HOSP_ADMIT_DT', 'AGE', 'ANAPH_DX_IND', 'EPI_ORDER_IND', 'FOLLOW_UP_IND', 'FOLLOW_UP_DATE', 'DAYS_TO_FOLLOW_UP')])

#write to csv file
write.csv(cohort, 'Healthcare Data Analyst Cohort R.csv')
