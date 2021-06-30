

library(readr) 
library(dplyr) 
library(tidyverse) 



#load dataset
data <- read_csv(file="F:\\project\\surgical_case_durations.csv", delim= ";", col_names = TRUE, col_types = NULL)

)

#rename columns
d1 <- data %>%
  rename(surgery_type = 'Operatietype', surgeon = 'Chirurg', anesthesiologist =  'Anesthesioloog',
         surgical_approach = 'Benadering', operation_room = 'OK', urgency = 'Casustype',
         time_of_day = 'Dagdeel', patient_age = 'Leeftijd', patient_gender = 'Geslacht',
         presence_of_atrial_fibrillation = 'AF',
         presence_of_chronic_lung_disease = 'Chronische longziekte',
         presence_of_extracardial_arteriopathy = 'Extracardiale vaatpathie',
         previous_heart_surgery = 'Eerdere hartchirurgie',
         presence_of_active_endocarditis = 'Actieve endocarditis',
         critical_pre_or_state = 'Kritische preoperatieve status',
         mycordial_infarction_before_surgery = 'Myocard infact <90 dagen',
         aortic_surgery = 'Aorta chirurgie',
         presence_of_pulmonary_hypertension = 'Pulmonale hypertensie',
         left_ventricle = 'Linker ventrikel functie', euroscore1 = 'Euroscore1',
         euroscore2 = 'Euroscore2', renal_function = 'Nierfunctie',
         presence_of_poor_mobility = 'Slechte mobiliteit', bmi = 'BMI', dm = 'DM',
         presence_of_hypercholesterolemia = 'Hypercholesterolemie',
         presence_of_hypertension = 'Hypertensie',
         presence_of_peripherial_vascula_disease = 'Perifeer vaatlijden', ccs = 'CCS', nyha = 'NYHA',
         amount_of_bypasses = 'Aantal anastomosen', cardiopulmonary_bypass_use = 'HLM',
         outcome1_planned_surgery_duration = 'Geplande operatieduur',
         outcome2_surgery_duration = 'Operatieduur', outcome3_hospital_days = 'Ziekenhuis ligduur',
         outcome4_intensive_care_days = 'IC ligduur')


d1[d1 == 'NULL']<-NA
d1[d1 == 'Onbekend']<-NA


d2 <- subset(d1, select = -c(patient_age, patient_gender,
                                   presence_of_chronic_lung_disease,
                                   presence_of_extracardial_arteriopathy, previous_heart_surgery,
                                   presence_of_active_endocarditis, critical_pre_or_state, 
                                   mycordial_infarction_before_surgery, aortic_surgery,
                                   presence_of_pulmonary_hypertension, left_ventricle, euroscore1,
                                   renal_function, presence_of_poor_mobility, bmi, dm, ccs, nyha ) )





d3 <- d2[complete.cases(data2[ , 9]),]





d4 <- subset(d3, select = -c(outcome1_planned_surgery_duration, outcome3_hospital_days,
                                   outcome4_intensive_care_days) )




#Convert "N" to 0 and "J" to 1 for better understanding(binary results)
d4[d4 == 'N']<-0
d4[d4 == 'J']<-1




d4$surgeon <- as.character(gsub(",00","",data4$surgeon))
d4$anesthesiologist <- as.character(gsub(",00","",data4$anesthesiologist))
d4$euroscore2 <- as.numeric(gsub(",",".",data4$euroscore2))



#automatically convert the classes
d5 <- type_convert(d4)



d5$surgeon <- as.character(d5$surgeon)
d5$anesthesiologist <- as.character(d5$anesthesiologist)
d5$amount_of_bypasses <- as.numeric(d5$amount_of_bypasses)
d5$outcome2_surgery_duration <- as.numeric(d5$outcome2_surgery_duration)
str(data5)





d_st_groups <- filter (d5, surgery_type == "Ascendensvervanging" | surgery_type =="AVR" |
                            surgery_type == "AVR + MVP" | surgery_type == "Bentall" | surgery_type == "CABG" |
                            surgery_type == "CABG + AVR" |
                            surgery_type == "CABG + MVP" | surgery_type == "Epicardiale LV-lead" |
                            surgery_type == "Lobectomie of segmentresectie" | surgery_type == "Mediastinoscopie" |
                            surgery_type == "MVP" | surgery_type == "MVP + TVP" | surgery_type == "Nuss" |
                            surgery_type == "Refixatie sternum" | surgery_type == "Staaldraden verwijderen" |
                            surgery_type == "VATS Boxlaesie" | surgery_type == "Wondtoilet")


d_st_other <- filter (d5, surgery_type != "Ascendensvervanging" & surgery_type !="AVR" &
                           surgery_type !="AVR + MVP" & surgery_type != "Bentall" & surgery_type != "CABG" &
                           surgery_type != "CABG + AVR" &
                           surgery_type != "CABG + MVP" & surgery_type != "Epicardiale LV-lead" &
                           surgery_type != "Lobectomie of segmentresectie" & surgery_type != "Mediastinoscopie" &
                           surgery_type != "MVP" & surgery_type != "MVP + TVP" & surgery_type != "Nuss" &
                           surgery_type != "Refixatie sternum" & surgery_type != "Staaldraden verwijderen" &
                           surgery_type != "VATS Boxlaesie" & surgery_type != "Wondtoilet")


d_st_other$surgery_type <- "Other types"

d6 <- rbind(d_st_groups, d_st_other)

d6 %>% group_by(surgery_type) %>% tally()


d6 %>% group_by(surgical_approach) %>% tally()

d_sa <- filter (d6, surgical_approach == "Volledige sternotomie" |
                     surgical_approach == "Antero lateraal links" |
                     surgical_approach == "Antero lateraal rechts" |
                     surgical_approach == "Postero lateraal links" |
                     surgical_approach == "Postero lateraal rechts" |
                     surgical_approach == "Partiele sternotomie")

