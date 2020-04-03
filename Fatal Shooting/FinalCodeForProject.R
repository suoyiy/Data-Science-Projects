library(tidyverse)
setwd("/Users/suoyiyang/Documents/101CProject")
train <- read.csv("train.csv")
test <- read.csv("test.csv")


train$SubjectGender <- gsub("U", NA, train$SubjectGender)
train$SubjectGender <- gsub("N/A", NA, train$SubjectGender)

# impute NA as M.... may not be good
for (i in 1:nrow(train)) {
  if (is.na(train$SubjectGender[i])) {
    train$SubjectGender[i] <- "M"
  }
}

train$SubjectRace <- gsub("U", NA, train$SubjectRace)
train$SubjectRace <- gsub("O", NA, train$SubjectRace)
train$SubjectArmed <- gsub("U", NA, train$SubjectArmed)
train$Fatal <- gsub("U", NA, train$Fatal)

train$OfficerRace <- gsub("U", NA, train$OfficerRace)
train$OfficerRace <- gsub("^[wW].+", "W", train$OfficerRace)
train$OfficerRace <- gsub("^[bB].+", "B", train$OfficerRace)
train$OfficerRace <- gsub("^[aA].+", "A", train$OfficerRace)
train$OfficerRace <- gsub("^[lL].+", "L", train$OfficerRace)
train$OfficerRace <- gsub("^[hH].+", "L", train$OfficerRace)
train$OfficerRace <- gsub("^[oO].+", NA, train$OfficerRace)
train$OfficerRace <- gsub("NA/W", "W", train$OfficerRace)


# impute NA as W.... may not be good
for (i in 1:nrow(train)) {
  if (is.na(train$OfficerRace[i])) {
    train$OfficerRace[i] <- "W"
  }
}


train$OfficerGender <- gsub("U", NA, train$OfficerGender)
train$OfficerGender <- gsub("^[mM].+", "M", train$OfficerGender)
train$OfficerGender <- gsub("^[fF].+", "F", train$OfficerGender)

# impute NA as M.... may not be good
for (i in 1:nrow(train)) {
  if (is.na(train$OfficerGender[i])) {
    train$OfficerGender[i] <- "M"
  }
}

# Clean Number of Shots

train$NumberOfShots <- gsub("U", NA, train$NumberOfShots)
train$NumberOfShots <- gsub("Unknown", NA, train$NumberOfShots)
train$NumberOfShots <- gsub("not clear", NA, train$NumberOfShots)
train$NumberOfShots <- gsub("no information", NA, train$NumberOfShots)
train$NumberOfShots <- gsub(">/=1", 1.0, train$NumberOfShots)
train$NumberOfShots <- gsub(">/=20", 20.0, train$NumberOfShots)
train$NumberOfShots <- gsub(">/=2", 2.0, train$NumberOfShots)
train$NumberOfShots <- gsub(">/=3", 3.0, train$NumberOfShots)
train$NumberOfShots <- gsub(">/=4", 4.0, train$NumberOfShots)
train$NumberOfShots <- gsub(">/=5", 5.0, train$NumberOfShots)
train$NumberOfShots <- gsub(">1", 1.0, train$NumberOfShots)
train$NumberOfShots <- gsub(">2", 2.0, train$NumberOfShots)
train$NumberOfShots <- gsub(">4", 4.0, train$NumberOfShots)
train$NumberOfShots <- gsub("45 total", 45.0, train$NumberOfShots)
train$NumberOfShots <- gsub(";", NA, train$NumberOfShots)
train$NumberOfShots <- gsub("Multiple", NA, train$NumberOfShots)

train$NumberOfShots <- as.numeric(train$NumberOfShots)

train$NumberOfShots[which(train$NumberOfShots > 100)] <- 5 # to account for the erroneous input

#impute NA as mean
for (i in 1:nrow(train)) {
  if (is.na(train$NumberOfShots[i])) {
    train$NumberOfShots[i] <- mean(na.omit(train$NumberOfShots))
  }
}


## Clean Nature of Stop

train$NatureOfStop <- gsub("^[sS]hooting.+", "Shots Fired", train$NatureOfStop)
train$NatureOfStop <- gsub("^[sS]hooting", "Shots Fired", train$NatureOfStop)
train$NatureOfStop <- gsub("^Accidental Discharge w/ injuries", "Shots Fired", train$NatureOfStop)
train$NatureOfStop <- gsub("^accidental shooting", "Shots Fired", train$NatureOfStop)
train$NatureOfStop <- gsub("^Domestic violence; shots fired", "Shots Fired", train$NatureOfStop)
train$NatureOfStop <- gsub("^Person shot", "Shots Fired", train$NatureOfStop)
train$NatureOfStop <- gsub("^Crowd-control; shooting", "Shots Fired", train$NatureOfStop)
train$NatureOfStop <- gsub("^Prostitution; police shooting", "Shots Fired", train$NatureOfStop)
train$NatureOfStop <- gsub("^Active shooter", "Shots Fired", train$NatureOfStop)
train$NatureOfStop <- gsub("^[sS]hots\\s[fF]ired.+", "Shots Fired", train$NatureOfStop)
train$NatureOfStop <- gsub("^Disturbance; crowd-control; gunshots", "Shots Fired", train$NatureOfStop)
train$NatureOfStop <- gsub("^[sS]hots\\s[fF]ired", "Shots Fired", train$NatureOfStop)
train$NatureOfStop <- gsub("^Assault on officer; shootings; gang", "Shots Fired", train$NatureOfStop)
train$NatureOfStop <- gsub("^Sound of Shots", "Shots Fired", train$NatureOfStop)
train$NatureOfStop <- gsub("^Fight; shots fired", "Shots Fired", train$NatureOfStop)
train$NatureOfStop <- gsub("^Sound of Shots", "Shots Fired", train$NatureOfStop)
train$NatureOfStop <- gsub("^[wW]eapon.+", "Weapon Possesion", train$NatureOfStop)
train$NatureOfStop <- gsub("^[aA]rmed.+", "Weapon Possesion", train$NatureOfStop)
train$NatureOfStop <- gsub("^Drunkeness; weapons posession", "Weapon Possesion", train$NatureOfStop)
train$NatureOfStop <- gsub("^[aA]rmed", "Weapon Possesion", train$NatureOfStop)
train$NatureOfStop <- gsub("^Assault; weapons posession; firearms", "Weapon Possesion", train$NatureOfStop)
train$NatureOfStop <- gsub("^Gun posession", "Weapon Possesion", train$NatureOfStop)
train$NatureOfStop <- gsub("^Gun possession", "Weapon Possesion", train$NatureOfStop)
train$NatureOfStop <- gsub("^Man with a gun", "Weapon Possesion", train$NatureOfStop)
train$NatureOfStop <- gsub("Person with a gun", "Weapon Possesion", train$NatureOfStop)
train$NatureOfStop <- gsub("^Party Armed", "Weapon Possesion", train$NatureOfStop)
train$NatureOfStop <- gsub("^violent, armed dispute", "Weapon Possesion", train$NatureOfStop)
train$NatureOfStop <- gsub("^weilding machete", "Weapon Possesion", train$NatureOfStop)
train$NatureOfStop <- gsub("^Party Armed-suicidal", "Weapon Possesion", train$NatureOfStop)
train$NatureOfStop <- gsub("^Disturbance; firearm", "Weapon Possesion", train$NatureOfStop)
train$NatureOfStop <- gsub("^Disturbance; firearms posession", "Weapon Possesion", train$NatureOfStop)
train$NatureOfStop <- gsub("^Weapon Possesion-suicidal", "Weapon Possesion", train$NatureOfStop)
train$NatureOfStop <- gsub("^Susp Party/Armed", "Weapon Possesion", train$NatureOfStop)
train$NatureOfStop <- gsub("^Domestic dispute; weapons posession; firearm", "Weapon Possesion", train$NatureOfStop)
train$NatureOfStop <- gsub("^Domestic disturbance; firearm; suicide", "Weapon Possesion", train$NatureOfStop)
train$NatureOfStop <- gsub("^Sus Party Armed", "Weapon Possesion", train$NatureOfStop)
train$NatureOfStop <- gsub("^Firearm", "Weapon Possesion", train$NatureOfStop)
train$NatureOfStop <- gsub("^Drunkeness; weapons posession ", "Weapon Possesion", train$NatureOfStop)
train$NatureOfStop <- gsub("^Bulletin indicating victim had firearm", "Weapon Possesion", train$NatureOfStop)
train$NatureOfStop <- gsub("^[mM]urder.+", "Homicide", train$NatureOfStop)
train$NatureOfStop <- gsub("^[mM]urder", "Homicide", train$NatureOfStop)
train$NatureOfStop <- gsub("^[sS]tabbing", "Homicide", train$NatureOfStop)
train$NatureOfStop <- gsub("^[hH]omicide.+", "Homicide", train$NatureOfStop)
train$NatureOfStop <- gsub("^woman being stabbed", "Homicide", train$NatureOfStop)
train$NatureOfStop <- gsub("^Homocide.+", "Homicide", train$NatureOfStop)
train$NatureOfStop <- gsub("Capital murder", "Homicide", train$NatureOfStop)
train$NatureOfStop <- gsub("Unknown", NA, train$NatureOfStop)
train$NatureOfStop <- gsub("^Agg Assault/Op 100", "Aggrevated Assault", train$NatureOfStop)
train$NatureOfStop <- gsub("^Agg. Assault", "Aggrevated Assault", train$NatureOfStop)
train$NatureOfStop <- gsub("^Agg. Assualt", "Aggrevated Assault", train$NatureOfStop)
train$NatureOfStop <- gsub("^Aggraveted assault", "Aggrevated Assault", train$NatureOfStop)
train$NatureOfStop <- gsub("^Agg DV Assault", "Aggrevated Assault", train$NatureOfStop)
train$NatureOfStop <- gsub("^aggravated battery", "Aggrevated Assault", train$NatureOfStop)
train$NatureOfStop <- gsub("^[fF]igh.+", "Assault", train$NatureOfStop)
train$NatureOfStop <- gsub("^Assaulting officer", "Aggrevated Assault", train$NatureOfStop)
train$NatureOfStop <- gsub("^Nag-Assault", "Assault", train$NatureOfStop)
train$NatureOfStop <- gsub("^[dD]omestic [vV]iolence$", "Assault", train$NatureOfStop)
train$NatureOfStop <- gsub("^Battery", "Assault", train$NatureOfStop)
train$NatureOfStop <- gsub("^Ambush", "Assault", train$NatureOfStop)
train$NatureOfStop <- gsub("^Violence Reduction", "Assault", train$NatureOfStop)
train$NatureOfStop <- gsub("^Violent altercation", "Assault", train$NatureOfStop)
train$NatureOfStop <- gsub("^Assault; car accident", "Assault", train$NatureOfStop)
train$NatureOfStop <- gsub("^Volent dispute", "Assault", train$NatureOfStop)
train$NatureOfStop <- gsub("^Volent dispute ", "Assault", train$NatureOfStop)
train$NatureOfStop <- gsub("^Domestic aggrevated assault", "Aggrevated Assault", train$NatureOfStop)
train$NatureOfStop <- gsub("Domestic assault and battery; mental illness", "Assault", train$NatureOfStop)
train$NatureOfStop <- gsub("^suspicious.+|^Suspicious.+", "Suspicious Entity", train$NatureOfStop)
train$NatureOfStop <- gsub("^Susp.\\s.+", "Suspicious Entity", train$NatureOfStop)
train$NatureOfStop <- gsub("\"suspicious person looking into vehicles in the area\"", "Suspicious Entity", train$NatureOfStop)
train$NatureOfStop <- gsub("Surveillance", "Suspicious Entity", train$NatureOfStop)
train$NatureOfStop <- gsub("^Wanted\\s.+", "Suspect", train$NatureOfStop)
train$NatureOfStop <- gsub("^Stakeout", "Suspect", train$NatureOfStop)
train$NatureOfStop <- gsub("^Disturbance wanted-PCS", "Suspect", train$NatureOfStop)
train$NatureOfStop <- gsub("^Felony Car check", "Suspect", train$NatureOfStop)
train$NatureOfStop <- gsub("^Pursuit", "Suspect", train$NatureOfStop)
train$NatureOfStop <- gsub("^Felony fugitive", "Suspect", train$NatureOfStop)
train$NatureOfStop <- gsub("^Suspe.+", "Suspect", train$NatureOfStop)
train$NatureOfStop <- gsub("^BO.+", "Suspect", train$NatureOfStop)
train$NatureOfStop <- gsub("^gang activity", "Suspect", train$NatureOfStop)
train$NatureOfStop <- gsub("^Gang activity; nightlife", "Suspect", train$NatureOfStop)
train$NatureOfStop <- gsub("^Gang; suspicious person", "Suspect", train$NatureOfStop)
train$NatureOfStop <- gsub("^Search for suspect in an aggravated robbery", "Suspect", train$NatureOfStop)
train$NatureOfStop <- gsub("^Fugitive; Immigration and customs enforcement", "Suspect", train$NatureOfStop)
train$NatureOfStop <- gsub("domestic argument", "Disturbance", train$NatureOfStop)
train$NatureOfStop <- gsub("^[dD]omestic [dD]ispute$", "Disturbance", train$NatureOfStop)
train$NatureOfStop <- gsub("^[dD]omestic [dD]isturbance$", "Disturbance", train$NatureOfStop)
train$NatureOfStop <- gsub("domestic distrurbance", "Disturbance", train$NatureOfStop)
train$NatureOfStop <- gsub("Altercation", "Disturbance", train$NatureOfStop)
train$NatureOfStop <- gsub("complaint", "Disturbance", train$NatureOfStop)
train$NatureOfStop <- gsub("Loud music disturbance", "Disturbance", train$NatureOfStop)
train$NatureOfStop <- gsub("Major disturbance", "Disturbance", train$NatureOfStop)
train$NatureOfStop <- gsub("^.+nted-PCS$", "Disturbance", train$NatureOfStop)
train$NatureOfStop <- gsub("domestic distrubance", "Disturbance", train$NatureOfStop)
train$NatureOfStop <- gsub("Noise Disturbance", "Disturbance", train$NatureOfStop)
train$NatureOfStop <- gsub("Disturbance; emergency", "Disturbance", train$NatureOfStop)
train$NatureOfStop <- gsub("Animal disturbance", "Disturbance", train$NatureOfStop)
train$NatureOfStop <- gsub("Party", "Disturbance", train$NatureOfStop)
train$NatureOfStop <- gsub("^[dD]rug.+", "Narcotics", train$NatureOfStop)
train$NatureOfStop <- gsub("^Narcotics", "Narcotics", train$NatureOfStop)
train$NatureOfStop <- gsub("^Narcotics.+", "Narcotics", train$NatureOfStop)
train$NatureOfStop <- gsub("^Stolen [cC]ar", "Larceny", train$NatureOfStop)
train$NatureOfStop <- gsub("^Larceny.+", "Larceny", train$NatureOfStop)
train$NatureOfStop <- gsub("^[sS]tolen [vV]ehicle", "Larceny", train$NatureOfStop)
train$NatureOfStop <- gsub("vehicle burglary" , "Larceny", train$NatureOfStop)
train$NatureOfStop <- gsub("Stolen police car" , "Larceny", train$NatureOfStop)
train$NatureOfStop <- gsub("Shoplifter" , "Larceny", train$NatureOfStop)
train$NatureOfStop <- gsub(".+shoplifting$" , "Larceny", train$NatureOfStop)
train$NatureOfStop <- gsub("^Shoplifting", "Larceny", train$NatureOfStop)
train$NatureOfStop <- gsub("^Stolen Property", "Larceny", train$NatureOfStop)
train$NatureOfStop <- gsub("^Theft", "Larceny", train$NatureOfStop)
train$NatureOfStop <- gsub("^Stolen Auto", "Larceny", train$NatureOfStop)
train$NatureOfStop <- gsub("^Believed stolen vehicle", "Larceny", train$NatureOfStop)
train$NatureOfStop <- gsub("^Carjacking; weapons posession", "Larceny", train$NatureOfStop)
train$NatureOfStop <- gsub("^[bB]urglary.+", "Burglary", train$NatureOfStop)
train$NatureOfStop <- gsub("^[bB]urglary", "Burglary", train$NatureOfStop)
train$NatureOfStop <- gsub("^Breaking and entering", "Burglary", train$NatureOfStop)
train$NatureOfStop <- gsub("^[hH]ome [iI]nvasion", "Burglary", train$NatureOfStop)
train$NatureOfStop <- gsub("^[rRob].+", "Robbery", train$NatureOfStop)
train$NatureOfStop <- gsub("^Aggravated assault; robbery", "Robbery", train$NatureOfStop)
train$NatureOfStop <- gsub("^[bB]ank\\s.+", "Robbery", train$NatureOfStop)
train$NatureOfStop <- gsub("^Investigation; robbery", "Robbery", train$NatureOfStop)
train$NatureOfStop <- gsub("^possible robbery", "Robbery", train$NatureOfStop)
train$NatureOfStop <- gsub("^Aggravated robbery", "Robbery", train$NatureOfStop)
train$NatureOfStop <- gsub(".+obbery Apprehension$", "Robbery", train$NatureOfStop)
train$NatureOfStop <- gsub("^.+[wW]arrant.+", "Warrant", train$NatureOfStop)
train$NatureOfStop <- gsub("^[wW]arrant.+", "Warrant", train$NatureOfStop)
train$NatureOfStop <- gsub("^.+[wW]arrant", "Warrant", train$NatureOfStop)
train$NatureOfStop <- gsub("^[wW]arrant", "Warrant", train$NatureOfStop)
train$NatureOfStop <- gsub("^[wW]arrants", "Warrant", train$NatureOfStop)
train$NatureOfStop <- gsub("^[mM]enta.+", "Mental Health", train$NatureOfStop)
train$NatureOfStop <- gsub("^[sS]uici.+", "Mental Health", train$NatureOfStop)
train$NatureOfStop <- gsub("^Cutting", "Mental Health", train$NatureOfStop)
train$NatureOfStop <- gsub("^Disturbance; mental illness", "Mental Health", train$NatureOfStop)
train$NatureOfStop <- gsub("^Welfare\\s.+", "Other", train$NatureOfStop)
train$NatureOfStop <- gsub("^emotionally disturbed person call", "Mental Health", train$NatureOfStop)
train$NatureOfStop <- gsub("^[cC]ar\\s.+", "Car Related", train$NatureOfStop)
train$NatureOfStop <- gsub("hit-and-run", "Crime", train$NatureOfStop)
train$NatureOfStop <- gsub("hit and run chase", "Crime", train$NatureOfStop)
train$NatureOfStop <- gsub("Fleeing vehicle", "Suspect", train$NatureOfStop)
train$NatureOfStop <- gsub("^[tT]raffic.+", "Car Related", train$NatureOfStop)
train$NatureOfStop <- gsub("^Hit and run", "Crime", train$NatureOfStop)
train$NatureOfStop <- gsub("^[vV]ehicle\\s.+", "Car Related", train$NatureOfStop)
train$NatureOfStop <- gsub("^D.U.I. Check Point", "Car Related", train$NatureOfStop)
train$NatureOfStop <- gsub("^Arson; traffic stop", "Car Related", train$NatureOfStop)
train$NatureOfStop <- gsub("^Police chase", "Suspect", train$NatureOfStop)
train$NatureOfStop <- gsub("^dangerous driver", "Car Related", train$NatureOfStop)
train$NatureOfStop <- gsub("^Street racing", "Car Related", train$NatureOfStop)
train$NatureOfStop <- gsub("^Traffic", "Car Related", train$NatureOfStop)
train$NatureOfStop <- gsub("^pursuit of a vehicle", "Suspect", train$NatureOfStop)
train$NatureOfStop <- gsub("^[oO]ff.+", "Off Duty", train$NatureOfStop)
train$NatureOfStop <- gsub("^[cC]all [fF]or [sS]ervice", "Call for Service", train$NatureOfStop)
train$NatureOfStop <- gsub("^[eE]mergencey [cC]all [oO]r [rR]equest [fF]or [aA]ssitance", "Call for Service", train$NatureOfStop)          
train$NatureOfStop <- gsub("^[eE]mergency [cC]all [oO]r [rR]equest [fF]or [aA]ssistance", "Call for Service", train$NatureOfStop)       
train$NatureOfStop <- gsub("^911 hang-up", "Call for Service", train$NatureOfStop)
train$NatureOfStop <- gsub("^Radio Call", "Call for Service", train$NatureOfStop)
train$NatureOfStop <- gsub("^Citizen Contact", "Call for Service", train$NatureOfStop)
train$NatureOfStop <- gsub("^CFS/Op-100", "Call for Service", train$NatureOfStop)
train$NatureOfStop <- gsub("^Op 100", "Call for Service", train$NatureOfStop)
train$NatureOfStop <- gsub("^arrest", "Crime", train$NatureOfStop)
train$NatureOfStop <- gsub("^Arrest", "Crime", train$NatureOfStop)
train$NatureOfStop <- gsub("^Foot Chase", "Crime", train$NatureOfStop)
train$NatureOfStop <- gsub("^Alarm activation", "Other", train$NatureOfStop)
train$NatureOfStop <- gsub("^Expired plates", "Other", train$NatureOfStop)
train$NatureOfStop <- gsub("^Assist the Officer", "Other", train$NatureOfStop)
train$NatureOfStop <- gsub("^Eviction", "Other", train$NatureOfStop)
train$NatureOfStop <- gsub("^Transient", "Other", train$NatureOfStop)
train$NatureOfStop <- gsub("^pedestrian stop", "Other", train$NatureOfStop)
train$NatureOfStop <- gsub("^Proactive Policing", "Other", train$NatureOfStop)
train$NatureOfStop <- gsub("^Public tranportation fees", "Other", train$NatureOfStop)
train$NatureOfStop <- gsub("^Pedestrian Check", "Other", train$NatureOfStop)
train$NatureOfStop <- gsub("^Investigation", "Other", train$NatureOfStop)
train$NatureOfStop <- gsub("^Sexual predator", "Other", train$NatureOfStop)
train$NatureOfStop <- gsub("^Tampering with vehicle", "Other", train$NatureOfStop)
train$NatureOfStop <- gsub("^tire slashing", "Other", train$NatureOfStop)
train$NatureOfStop <- gsub("^Trespassing", "Other", train$NatureOfStop)
train$NatureOfStop <- gsub("^Vandalism", "Other", train$NatureOfStop)
train$NatureOfStop <- gsub("^Flagged down", "Other", train$NatureOfStop)
train$NatureOfStop <- gsub("^Fraud", "Other", train$NatureOfStop)
train$NatureOfStop <- gsub("^Trespassing", "Other", train$NatureOfStop)
train$NatureOfStop <- gsub("^Property crime", "Other", train$NatureOfStop)
train$NatureOfStop <- gsub("Property Damage", "Other", train$NatureOfStop)
train$NatureOfStop <- gsub(".+een drinking", "Other", train$NatureOfStop)
train$NatureOfStop <- gsub("new year's eve patrol", "Other", train$NatureOfStop)
train$NatureOfStop <- gsub("Patrol", "Other", train$NatureOfStop)
train$NatureOfStop <- gsub("Aggravated kidnapping", "Hostage or Major Emergency", train$NatureOfStop)
train$NatureOfStop <- gsub("Carjacking;Sexual Assault;Kidnapping", "Hostage or Major Emergency", train$NatureOfStop)
train$NatureOfStop <- gsub("Hostage situation", "Hostage or Major Emergency", train$NatureOfStop)
train$NatureOfStop <- gsub("Hostage; firearm", "Hostage or Major Emergency", train$NatureOfStop)
train$NatureOfStop <- gsub("Hostage, barricade, or other emergency situation", "Hostage or Major Emergency", train$NatureOfStop)
train$NatureOfStop <- gsub("Domestic violence; hostage", "Hostage or Major Emergency", train$NatureOfStop)
train$NatureOfStop <- gsub("Kidnapping", "Hostage or Major Emergency", train$NatureOfStop)
train$NatureOfStop <- gsub(".+jury to child", "Hostage or Major Emergency", train$NatureOfStop)
train$NatureOfStop <- gsub("SWAT", "Hostage or Major Emergency", train$NatureOfStop)
train$NatureOfStop <- gsub(".+session", "Weapon Possesion", train$NatureOfStop)
train$NatureOfStop <- gsub("Aggrevated Assault; car accident", "Aggrevated Assault", train$NatureOfStop)

# Clean Subject Age

train$SubjectAge<-gsub("0-19", "10", train$SubjectAge)
train$SubjectAge<-gsub("20-29", "25", train$SubjectAge)
train$SubjectAge<-gsub("30-39", "35", train$SubjectAge)
train$SubjectAge<-gsub("40-49", "45", train$SubjectAge)
train$SubjectAge<-gsub("50-59", "55", train$SubjectAge)
train$SubjectAge<-gsub("Juvenile", "0-19", train$SubjectAge)
train$SubjectAge <- gsub("U", NA, train$SubjectAge)
train$SubjectAge <- gsub("UNKNOWN", NA, train$SubjectAge)
train$SubjectAge<-gsub("N/A",NA,train$SubjectAge)
train$SubjectAge<-as.numeric(train$SubjectAge)
#train$SubjectAge<-cut(train$SubjectAge,breaks=c(0,20,30,40,50,60,73),labels=c("0-19","20-29","30-39","40-49","50-59","60-70"))

#impute NA as mean
for (i in 1:nrow(train)) {
  if (is.na(train$SubjectAge[i])) {
    train$SubjectAge[i] <- round(mean(na.omit(train$SubjectAge)))
  }
}


#impute NA as mean
for (i in 1:nrow(train)) {
  if (is.na(train$NumberOfOfficers[i])) {
    train$NumberOfOfficers[i] <- round(mean(na.omit(train$NumberOfOfficers)))
  }
}

# impute NA as B
for (i in 1:nrow(train)) {
  if (is.na(train$SubjectRace[i])) {
    train$SubjectRace[i] <- "B"
  }
}



#train.sample <-  train[,-c(2,3,9,11,14,15,16,17)]






############################################ TEST

test$SubjectGender <- gsub("U", NA, test$SubjectGender)

# impute NA as M.... may not be good
for (i in 1:nrow(test)) {
  if (is.na(test$SubjectGender[i])) {
    test$SubjectGender[i] <- "M"
  }
}

test$SubjectRace <- gsub("U", NA, test$SubjectRace)
test$SubjectRace <- gsub("O", NA, test$SubjectRace)
test$SubjectRace <- gsub("A", NA, test$SubjectRace)
test$SubjectArmed <- gsub("U", NA, test$SubjectArmed)

test$OfficerRace <- gsub("U", NA, test$OfficerRace)
test$OfficerRace <- gsub("^[wW].+", "W", test$OfficerRace)
test$OfficerRace <- gsub("^[bB].+", "B", test$OfficerRace)
test$OfficerRace <- gsub("^[aA].+", "A", test$OfficerRace)
test$OfficerRace <- gsub("^[lL].+", "L", test$OfficerRace)
test$OfficerRace <- gsub("^[hH].+", "L", test$OfficerRace)
test$OfficerRace <- gsub("^[oO].+", NA, test$OfficerRace)
test$OfficerRace <- gsub("m/m", "L", test$OfficerRace)


# impute NA
for (i in 1:nrow(test)) {
  if (is.na(test$OfficerRace[i])) {
    test$OfficerRace[i] <- "W"
  }
}

test$OfficerGender <- gsub("U", NA, test$OfficerGender)
test$OfficerGender <- gsub("^[mM].+", "M", test$OfficerGender)
test$OfficerGender <- gsub("^[fF].+", "F", test$OfficerGender)

# impute NA to M
for (i in 1:nrow(test)) {
  if (is.na(test$OfficerGender[i])) {
    test$OfficerGender[i] <- "M"
  }
}


test$NumberOfShots <- gsub("U", NA, test$NumberOfShots)
test$NumberOfShots <- gsub("Unknown", NA, test$NumberOfShots)
test$NumberOfShots <- gsub("not clear", NA, test$NumberOfShots)
test$NumberOfShots <- gsub("no information", NA, test$NumberOfShots)
test$NumberOfShots <- gsub(">/=1", 1.0, test$NumberOfShots)
test$NumberOfShots <- gsub(">/=20", 20.0, test$NumberOfShots)
test$NumberOfShots <- gsub(">/=2", 2.0, test$NumberOfShots)
test$NumberOfShots <- gsub(">/=3", 3.0, test$NumberOfShots)
test$NumberOfShots <- gsub(">/=4", 4.0, test$NumberOfShots)
test$NumberOfShots <- gsub(">/=5", 5.0, test$NumberOfShots)
test$NumberOfShots <- gsub(">1", 1.0, test$NumberOfShots)
test$NumberOfShots <- gsub(">2", 2.0, test$NumberOfShots)
test$NumberOfShots <- gsub(">4", 4.0, test$NumberOfShots)
test$NumberOfShots <- gsub("45 total", 45.0, test$NumberOfShots)
test$NumberOfShots <- gsub(";", NA, test$NumberOfShots)
test$NumberOfShots <- gsub("Multiple", NA, test$NumberOfShots)

test$NumberOfShots <- as.numeric(test$NumberOfShots)

train$NumberOfShots[which(train$NumberOfShots > 100)] <- 5

#impute NA as mean
for (i in 1:nrow(test)) {
  if (is.na(test$NumberOfShots[i])) {
    test$NumberOfShots[i] <- mean(na.omit(test$NumberOfShots))
  }
}



test$NatureOfStop <- gsub("^Agg DV Assault", "Aggrevated Assault", test$NatureOfStop)
test$NatureOfStop <- gsub("^Agg-Assault", "Aggrevated Assault", test$NatureOfStop)
test$NatureOfStop <- gsub("^Agg. Assault", "Aggrevated Assault", test$NatureOfStop)
test$NatureOfStop <- gsub("^Attacked officer", "Aggrevated Assault", test$NatureOfStop)
test$NatureOfStop <- gsub("^Agg. Assault", "Aggrevated Assault", test$NatureOfStop)
test$NatureOfStop <- gsub("^Domestic disturbance; assaultling police officer", "Aggrevated Assault", test$NatureOfStop)
test$NatureOfStop <- gsub("^Domestic [vV]iolen.+", "Assault", test$NatureOfStop)
test$NatureOfStop <- gsub("^Ambush", "Assault", test$NatureOfStop)
test$NatureOfStop <- gsub("^Fight / disorderly conduct", "Assault", test$NatureOfStop)
test$NatureOfStop <- gsub("^Medical help; assault", "Assault", test$NatureOfStop)
test$NatureOfStop <- gsub("^Reckless driving; assault on officer", "Assault", test$NatureOfStop)
test$NatureOfStop <- gsub("^Violence; public safety threat", "Assault", test$NatureOfStop)
test$NatureOfStop <- gsub("^[bB]urg.+", "Burglary", test$NatureOfStop)
test$NatureOfStop <- gsub("witness.+", "Crime", test$NatureOfStop)
test$NatureOfStop <- gsub("Threatening officer", "Crime", test$NatureOfStop)
test$NatureOfStop <- gsub("Murder", "Homicide", test$NatureOfStop)
test$NatureOfStop <- gsub("Arrest warrant;attempted murder", "Homicide", test$NatureOfStop)
test$NatureOfStop <- gsub("Domestic dispute, homicide", "Homicide", test$NatureOfStop)
test$NatureOfStop <- gsub("Intruder; homicide threat; home invasion", "Homicide", test$NatureOfStop)
test$NatureOfStop <- gsub("Stabbing", "Homicide", test$NatureOfStop)
test$NatureOfStop <- gsub("^Hosta.+", "Hostage or Major Emergency", test$NatureOfStop)
test$NatureOfStop <- gsub("^Bomb Investigation", "Hostage or Major Emergency", test$NatureOfStop)
test$NatureOfStop <- gsub("^Kidnapping", "Hostage or Major Emergency", test$NatureOfStop)
test$NatureOfStop <- gsub("^[sS]tolen.+", "Larceny", test$NatureOfStop)
test$NatureOfStop <- gsub("^Traffic; stolen car", "Larceny", test$NatureOfStop)
test$NatureOfStop <- gsub("^vehicle theft", "Larceny", test$NatureOfStop)
test$NatureOfStop <- gsub("^theft", "Larceny", test$NatureOfStop)
test$NatureOfStop <- gsub("^Suspected carjacking", "Larceny", test$NatureOfStop)
test$NatureOfStop <- gsub("^[dD]ru.+", "Narcotics", test$NatureOfStop)
test$NatureOfStop <- gsub("^[nN]arc.+", "Narcotics", test$NatureOfStop)
test$NatureOfStop <- gsub("^Complaint; drugs", "Narcotics", test$NatureOfStop)
test$NatureOfStop <- gsub("^Search warrant- Drugs", "Narcotics", test$NatureOfStop)
test$NatureOfStop <- gsub("^Off-duty", "Off Duty", test$NatureOfStop)
test$NatureOfStop <- gsub("^Off duty", "Off Duty", test$NatureOfStop)
test$NatureOfStop <- gsub("^Eviction", "Other", test$NatureOfStop)
test$NatureOfStop <- gsub("^Alarm call", "Other", test$NatureOfStop)
test$NatureOfStop <- gsub("^Check welfare; fire", "Other", test$NatureOfStop)
test$NatureOfStop <- gsub("^Counterfeiting", "Other", test$NatureOfStop)
test$NatureOfStop <- gsub("^Asst. Officer", "Other", test$NatureOfStop)
test$NatureOfStop <- gsub("^Asst. Officer", "Other", test$NatureOfStop)
test$NatureOfStop <- gsub("^Investigat.+", "Other", test$NatureOfStop)
test$NatureOfStop <- gsub("^other$", "Other", test$NatureOfStop)
test$NatureOfStop <- gsub("^Park rules", "Other", test$NatureOfStop)
test$NatureOfStop <- gsub("^Patr.+", "Other", test$NatureOfStop)
test$NatureOfStop <- gsub("^person being threatened", "Other", test$NatureOfStop)
test$NatureOfStop <- gsub("^Property destruction", "Other", test$NatureOfStop)
test$NatureOfStop <- gsub("^Restricted area; trespassing at police station", "Other", test$NatureOfStop)
test$NatureOfStop <- gsub("^prostitution solicitation", "Other", test$NatureOfStop)
test$NatureOfStop <- gsub("^Prowlers", "Other", test$NatureOfStop)
test$NatureOfStop <- gsub("^Publi.+", "Other", test$NatureOfStop)
test$NatureOfStop <- gsub("^runaway", "Other", test$NatureOfStop)
test$NatureOfStop <- gsub("^Self-Initiated", "Other", test$NatureOfStop)
test$NatureOfStop <- gsub("^Sex crime", "Other", test$NatureOfStop)
test$NatureOfStop <- gsub("^Sexual harassment", "Other", test$NatureOfStop)
test$NatureOfStop <- gsub("^Trespassing", "Other", test$NatureOfStop)
test$NatureOfStop <- gsub("^Underaged party", "Other", test$NatureOfStop)
test$NatureOfStop <- gsub("^Welfare check", "Other", test$NatureOfStop)
test$NatureOfStop <- gsub("^[aA]ggravated robbery", "Robbery", test$NatureOfStop)
test$NatureOfStop <- gsub("^[rR]obb.+", "Robbery", test$NatureOfStop)
test$NatureOfStop <- gsub("^[aA]rmed [rR]obbery", "Robbery", test$NatureOfStop)
test$NatureOfStop <- gsub("^Home inv.+", "Robbery", test$NatureOfStop)
test$NatureOfStop <- gsub("^Party Wanted-Robbery", "Robbery", test$NatureOfStop)
test$NatureOfStop <- gsub("^Shot.+", "Shots Fired", test$NatureOfStop)
test$NatureOfStop <- gsub("^[sS]hoo.+", "Shots Fired", test$NatureOfStop)
test$NatureOfStop <- gsub("^Weapons posession; shooting", "Shots Fired", test$NatureOfStop)
test$NatureOfStop <- gsub("^accidental fire", "Shots Fired", test$NatureOfStop)
test$NatureOfStop <- gsub("^Active shooter", "Shots Fired", test$NatureOfStop)
test$NatureOfStop <- gsub("^Assault on officer; shooting; mental health", "Shots Fired", test$NatureOfStop)
test$NatureOfStop <- gsub("^Officer shot", "Shots Fired", test$NatureOfStop)
test$NatureOfStop <- gsub("^BO.+", "Suspect", test$NatureOfStop)
test$NatureOfStop <- gsub("^gang sweeps", "Suspect", test$NatureOfStop)
test$NatureOfStop <- gsub("^Police chase", "Suspect", test$NatureOfStop)
test$NatureOfStop <- gsub("^Vehicle Pursuit", "Suspect", test$NatureOfStop)
test$NatureOfStop <- gsub("^vehicle chase", "Suspect", test$NatureOfStop)
test$NatureOfStop <- gsub("^Fleeing vehicle", "Suspect", test$NatureOfStop)
test$NatureOfStop <- gsub("^[sS]uspi.+", "Suspicious Entity", test$NatureOfStop)
test$NatureOfStop <- gsub("^Pursuit", "Suspicious Entity", test$NatureOfStop)
test$NatureOfStop <- gsub("^Ran from officers", "Suspicious Entity", test$NatureOfStop)
test$NatureOfStop <- gsub("^Execution of a warrant", "Warrant", test$NatureOfStop)
test$NatureOfStop <- gsub("^Traffic stop; warrant", "Warrant", test$NatureOfStop)
test$NatureOfStop <- gsub("^Search.+", "Warrant", test$NatureOfStop)
test$NatureOfStop <- gsub("^Search.+", "Warrant", test$NatureOfStop)
test$NatureOfStop <- gsub("^Warrants$", "Warrant", test$NatureOfStop)
test$NatureOfStop <- gsub("^warrant$", "Warrant", test$NatureOfStop)
test$NatureOfStop <- gsub("^Warrant/crime bulletin; marijuana", "Warrant", test$NatureOfStop)
test$NatureOfStop <- gsub("^[wW]eap.+", "Weapon Possesion", test$NatureOfStop)
test$NatureOfStop <- gsub("^[fF]ire.+", "Weapon Possesion", test$NatureOfStop)
test$NatureOfStop <- gsub("^Mental Health; weapons posession", "Weapon Possesion", test$NatureOfStop)
test$NatureOfStop <- gsub("^[aA]rm.+", "Weapon Possesion", test$NatureOfStop)
test$NatureOfStop <- gsub("^Disturbance; threatened shooting", "Weapon Possesion", test$NatureOfStop)
test$NatureOfStop <- gsub("^Domestic disturbance; firearm", "Weapon Possesion", test$NatureOfStop)
test$NatureOfStop <- gsub("^Gun posession", "Weapon Possesion", test$NatureOfStop)
test$NatureOfStop <- gsub("^Man with a gun", "Weapon Possesion", test$NatureOfStop)
test$NatureOfStop <- gsub("^Gang activity; weapon posession; suspicious subject", "Weapon Possesion", test$NatureOfStop)
test$NatureOfStop <- gsub("^Nightlife; gun posession", "Weapon Possesion", test$NatureOfStop)
test$NatureOfStop <- gsub("^Sus Party Armed", "Weapon Possesion", test$NatureOfStop)
test$NatureOfStop <- gsub("^showed firearm to bus riders", "Weapon Possesion", test$NatureOfStop)
test$NatureOfStop <- gsub("^[sS]uici.+", "Mental Health", test$NatureOfStop)
test$NatureOfStop <- gsub("^[mM]ent.+", "Mental Health", test$NatureOfStop)
test$NatureOfStop <- gsub("^Domestic dispute; mental health; suicide", "Mental Health", test$NatureOfStop)
test$NatureOfStop <- gsub("^Major disturbance; mental illness", "Mental Health", test$NatureOfStop)
test$NatureOfStop <- gsub("^threatening behavior; mental illness", "Mental Health", test$NatureOfStop)
test$NatureOfStop <- gsub("^[cC]all [fF]or [sS]ervice", "Call for Service", test$NatureOfStop)
test$NatureOfStop <- gsub("^Citizen Contact", "Call for Service", test$NatureOfStop)
test$NatureOfStop <- gsub("^Emergency Call or Request for Assistance", "Call for Service", test$NatureOfStop)
test$NatureOfStop <- gsub("^Radio Call", "Call for Service", test$NatureOfStop)
test$NatureOfStop <- gsub("^[tT]raff.+", "Car Related", test$NatureOfStop)
test$NatureOfStop <- gsub("^[cC]ar\\s.+", "Car Related", test$NatureOfStop)
test$NatureOfStop <- gsub("^Street racing", "Car Related", test$NatureOfStop)
test$NatureOfStop <- gsub("^vehicle drove toward cop car", "Car Related", test$NatureOfStop)
test$NatureOfStop <- gsub("^Vehicle [sS]top", "Car Related", test$NatureOfStop)
test$NatureOfStop <- gsub("^Ftraffic stop", "Car Related", test$NatureOfStop)
test$NatureOfStop <- gsub("^[dD]omes.+", "Disturbance", test$NatureOfStop)
test$NatureOfStop <- gsub("^argument", "Disturbance", test$NatureOfStop)
test$NatureOfStop <- gsub("^Family Disturbance", "Disturbance", test$NatureOfStop)
test$NatureOfStop <- gsub("^Major disturbance$", "Disturbance", test$NatureOfStop)
test$NatureOfStop <- gsub("^Noise complaint", "Disturbance", test$NatureOfStop)
test$NatureOfStop <- gsub("[uU]nkn.+", NA, test$NatureOfStop) 
test$NatureOfStop <- gsub("N/A", NA, test$NatureOfStop) 

test$NatureOfStop <- gsub("Off Duty", NA, test$NatureOfStop)

test$SubjectAge<-gsub("Juvenile", "0-19", test$SubjectAge)
test$SubjectAge<-gsub("0-19", "10", test$SubjectAge)
test$SubjectAge<-gsub("20-29", "25", test$SubjectAge)
test$SubjectAge<-gsub("30-39", "35", test$SubjectAge)
test$SubjectAge<-gsub("40-49", "45", test$SubjectAge)
test$SubjectAge<-gsub("50-59", "55", test$SubjectAge)
test$SubjectAge <- gsub("U", NA, test$SubjectAge)
test$SubjectAge <- gsub("UNKNOWN", NA, test$SubjectAge)
test$SubjectAge<-gsub("N/A",NA,test$SubjectAge)
test$SubjectAge<-as.numeric(test$SubjectAge)

#impute NA as mean
for (i in 1:nrow(test)) {
  if (is.na(test$SubjectAge[i])) {
    test$SubjectAge[i] <- round(mean(na.omit(test$SubjectAge)))
  }
}

#impute NA as mean
for (i in 1:nrow(test)) {
  if (is.na(test$NumberOfOfficers[i])) {
    test$NumberOfOfficers[i] <- round(mean(na.omit(test$NumberOfOfficers)))
  }
}

# impute NA as B
for (i in 1:nrow(test)) {
  if (is.na(test$SubjectRace[i])) {
    test$SubjectRace[i] <- "B"
  }
}


## FACTOR

train$Fatal <- as.factor(train$Fatal)
train$SubjectArmed <- as.factor(train$SubjectArmed) 
train$SubjectRace <- as.factor(train$SubjectRace) 
train$SubjectGender <- as.factor(train$SubjectGender) 
train$NatureOfStop <- as.factor(train$NatureOfStop) 
train$OfficerRace <- as.factor(train$OfficerRace) 
train$OfficerGender <- as.factor(train$OfficerGender) 


test$SubjectArmed <- as.factor(test$SubjectArmed) 
test$SubjectRace <- as.factor(test$SubjectRace) 
test$SubjectGender <- as.factor(test$SubjectGender) 
test$NatureOfStop <- as.factor(test$NatureOfStop) 
test$OfficerRace <- as.factor(test$OfficerRace) 
test$OfficerGender <- as.factor(test$OfficerGender) 


########################## DIRECT CHANGE TO TEST

test.notes <- na.omit(test[,c(1,16)])
test.notes$Notes <- as.character(test.notes$Notes)

Fatal <- rep(NA,1400)

submission <- as.data.frame(cbind(test$id,Fatal))
colnames(submission) <- c("id","Fatal")

indx <- grepl('no hit', tolower(test.notes$Notes))
test.notes[indx, ]$Notes <- "Non-Ftl"

indx <- grepl('non-fatal', tolower(test.notes$Notes))
test.notes[indx, ]$Notes <- "Non-Ftl"

indx <- grepl('fatal', tolower(test.notes$Notes))
test.notes[indx, ]$Notes <- "Ftl"

indx <- grepl('death', tolower(test.notes$Notes))
test.notes[indx, ]$Notes <- "Ftl"

indx <- grepl('died', tolower(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('WILLIAM', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('TERRY', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('JEFF', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('DAKOTA', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('DAVID', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('TYWON', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('DOMINIQ', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('FELIX', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('RICKY', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('ISMAEL', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('XAVIER', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('KARAKA', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('REGINALD', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('ARTURO', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('QUENTIN', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('ANTONIO', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('ANTIONE', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('BRIAN', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('TIFF', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('BROWN', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('ALONZO', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('LYNELL', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('DARRELL', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('JAMES', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('JASON',toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('CHRIS', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('ALFREDO', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('GEORGE', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('HARRIS', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('LITTLE', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('HALL', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('CAYO', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

indx <- grepl('NATHAN', toupper(test.notes$Notes))
test.notes[indx, ]$Notes  <- "Ftl"

#########################Enter Yes/No to Fatal column of Submission according to ID############### 

NonFatId <- test.notes[test.notes$Notes == "Non-Ftl",]$id
FatId <- test.notes[test.notes$Notes == "Ftl",]$id

NonFatIdx <- submission$id %in% NonFatId
FatIdx <- submission$id %in%  FatId

submission[NonFatIdx,]$Fatal <- "No"
submission[FatIdx,]$Fatal <- "Yes"

NaIndex <- is.na(submission$Fatal)
test2 <- test[NaIndex,]

test.fn <- na.omit(test2[,c(1,14)])
test.fn$FullNarrative <- as.character(test.fn$FullNarrative)

indx <- grepl('died', test.fn$FullNarrative)
test.fn[indx, ]$FullNarrative <- "Ftl"

indx <- grepl('killed', test.fn$FullNarrative)
test.fn[indx, ]$FullNarrative <- "Ftl"

indx <- grepl('[pP]ronounced', test.fn$FullNarrative)
test.fn[indx, ]$FullNarrative <- "Ftl"

indx <- grepl('[dD]ead', test.fn$FullNarrative)
test.fn[indx, ]$FullNarrative <- "Ftl"

indx <- grepl('non-fatal', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative  <- "Non-Ftl"

indx <- grepl('fatal', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative  <- "Ftl"

indx <- grepl('demise', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Ftl"

indx <- grepl('decease', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Ftl"

indx <- grepl('stable', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative  <- "Non-Ftl"

indx <- grepl('missed', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Non-Ftl"

indx <- grepl('minor', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Non-Ftl"

indx <- grepl('mortal', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative  <- "Ftl"

indx <- grepl('succumb', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Ftl"

indx <- grepl('wound', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Non-Ftl"

indx <- grepl('in the head', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Ftl"

indx <- grepl('miss', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Non-Ftl"

indx <- grepl('custody', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Non-Ftl"

indx <- grepl('no injur', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Non-Ftl"

indx <- grepl('chest', test.fn$FullNarrative)
test.fn[indx, ]$FullNarrative <- "Ftl"

indx <- grepl('apprehended', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Non-Ftl"

indx <- grepl('kill', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Ftl"

indx <- grepl('passed', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Ftl"

indx <- grepl('treat', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Non-Ftl"

indx <- grepl('not struck', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Non-Ftl"

indx <- grepl('ending the threat', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Ftl"

indx <- grepl('injured', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Non-Ftl"

indx <- grepl('expire', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Ftl"

indx <- grepl('OIS_2010', test.fn$FullNarrative)
test.fn[indx, ]$FullNarrative <- "Ftl"

indx <- grepl('OIS_2011', test.fn$FullNarrative)
test.fn[indx, ]$FullNarrative <- "Non-Ftl"

indx <- grepl('2012_[12]', test.fn$FullNarrative)
test.fn[indx, ]$FullNarrative <- "Non-Ftl"

indx <- grepl('2012_[35]', test.fn$FullNarrative)
test.fn[indx, ]$FullNarrative <- "Ftl"

indx <- grepl('OIS_2015_1', test.fn$FullNarrative)
test.fn[indx, ]$FullNarrative <- "Ftl"

indx <- grepl('OIS_2015_2', test.fn$FullNarrative)
test.fn[indx, ]$FullNarrative <- "Non-Ftl"

indx <- grepl('no hits', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Non-Ftl"

indx <- grepl('butt', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative  <- "Non-Ftl" 

indx <- grepl('leg', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Non-Ftl" 

indx <- grepl('death', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Ftl"

indx <- grepl('escape', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Non-Ftl"

indx <- grepl('non-critical', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Non-Ftl"

indx <- grepl('arrested', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Non-Ftl"

indx <- grepl('non-life', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative  <- "Non-Ftl"

indx <- grepl("striking suspect's foot", tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Non-Ftl"

indx <- grepl("striking suspect's head", tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Ftl"

indx <- grepl("striking suspect's hand", tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Non-Ftl"

indx <- grepl('not believe', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Non-Ftl"

indx <- grepl('unresponsive', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Ftl"

indx <- grepl('striking his arm', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Non-Ftl"

indx <- grepl('released from hospital', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Non-Ftl"

indx <- grepl('vehicle then sped away from the location', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Non-Ftl"

indx <- grepl('did not hit him', tolower(test.fn$FullNarrative))
test.fn[indx, ]$FullNarrative <- "Non-Ftl"

#########################Enter Yes/No to Fatal column of Submission according to ID############### 

NonFatId2 <- test.fn[test.fn$FullNarrative == "Non-Ftl",]$id
FatId2 <- test.fn[test.fn$FullNarrative == "Ftl",]$id

NonFatIdx2 <- submission$id %in% NonFatId2
FatIdx2 <- submission$id %in%  FatId2

submission[NonFatIdx2,]$Fatal <- "No"
submission[FatIdx2,]$Fatal <- "Yes"
################## MODEL and PRED


library(tree)
set.seed(1234)

model.tree <-tree(Fatal ~ SubjectArmed + SubjectRace + SubjectGender + SubjectRace +  NumberOfShots + OfficerRace + OfficerGender, data = train)
summary(model.tree)

NAIndex <- is.na(submission$Fatal)
test.new <- test[NAIndex,]
test.new$SubjectArmed <- as.factor(test.new$SubjectArmed) 
test.new$SubjectRace <- as.factor(test.new$SubjectRace) 
test.new$SubjectGender <- as.factor(test.new$SubjectGender) 
test.new$NatureOfStop <- as.factor(test.new$NatureOfStop) 
test.new$OfficerRace <- as.factor(test.new$OfficerRace) 
test.new$OfficerGender <- as.factor(test.new$OfficerGender) 

Fatal2 <- predict(model.tree, newdata = test.new, type="class")


miniSub <- as.data.frame(cbind(test.new$id,as.character(Fatal2)))
colnames(miniSub) <- c("id", "Fatal")


NonFatId <- miniSub[miniSub$Fatal == "N",]$id
FatId <- miniSub[miniSub$Fatal == "F",]$id

NonFatIdx <- submission$id %in% NonFatId
FatIdx <- submission$id %in%  FatId

submission[NonFatIdx,]$Fatal <- "No"
submission[FatIdx,]$Fatal <- "Yes"

write.csv(submission, "submission5.csv")
