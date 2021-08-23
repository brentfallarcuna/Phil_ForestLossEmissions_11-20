#SaveAs Correlation between Annual loss per province (and corresponding biomass) and time
#Author: LVDZ (ldzapiter@up.edu.ph) and Brent Fallarcuna (brentfallarcuna@essc.org.ph, brentiebark@gmail.com)

setwd("/automnt/ILCA/Hansen_Exploratory/R_codes_files/significant_trend_test/")
#This is used to import data from the folder containing .csv file into R
time <- read.csv(file="/automnt/ILCA/Hansen_Exploratory/R_codes_files/significant_trend_test/Years_2011-2020.csv",header = T, sep=",")
forest_loss <- read.csv(file="/automnt/ILCA/Hansen_Exploratory/R_codes_files/significant_trend_test/annual_loss_11-20.csv",header = T, sep=",")
biomass <- read.csv(file="/automnt/ILCA/Hansen_Exploratory/R_codes_files/significant_trend_test/annual_biomass_11-20.csv",header = T, sep=",")

#Declaration of matrices
flosscor <- matrix(0, 81, 4)
fCbmasscor <- matrix(0, 81, 4)
colnames(flosscor) <- c("Provinces", "Correlation (pearson)", "p-value", "Is it significant?")
colnames(fCbmasscor) <- c("Provinces", "Correlation (pearson)", "p-value", "Is it significant?")

#Initializing the 1st column into name of provinces
flosscor[,1] <- c("Roundwood",	"Agusan del Norte",	"Agusan del Sur",	"Aklan", "Albay",	"Antique",	"Apayao",	"Aurora",	"Basilan",	"Bataan",	"Batanes",	"Batangas",	"Benguet",	"Biliran","Bohol",	"Bukidnon",	"Bulacan",	"Cagayan",	"Camarines Norte",	"Camarines Sur",	"Camiguin",	"Capiz",	"Catanduanes",	"Cavite",	"Cebu",	"Compostela Valley",	"Davao Oriental",	"Davao del Norte",	"Davao del Sur",	"Dinagat Islands",	"Eastern Samar",	"Guimaras",	"Ifugao",	"Ilocos Norte",	"Ilocos Sur",	"Iloilo",	"Isabela",	"Kalinga",	"La Union",	"Laguna",	"Lanao del Norte",	"Lanao del Sur",	"Leyte",	"Maguindanao",	"Marinduque",	"Masbate",	"Misamis Occidental",	"Misamis Oriental",	"Mountain Province",	"National Capital Region",	"Negros Occidental",	"Negros Oriental",	"North Cotabato",	"Northern Samar",	"Nueva Ecija",	"Nueva Vizcaya",	"Occidental Mindoro",	"Oriental Mindoro",	"Palawan",	"Pampanga",	"Pangasinan",	"Quezon",	"Quirino",	"Rizal",	"Romblon",	"Samar",	"Sarangani",	"Siquijor",	"Sorsogon",	"South Cotabato",	"Southern Leyte",	"Sultan Kudarat",	"Sulu",	"Surigao del Norte",	"Surigao del Sur",	"Tarlac",	"Tawi-Tawi",	"Zambales",	"Zamboanga Sibugay",	"Zamboanga del Norte",	"Zamboanga del Sur")
fCbmasscor[,1] <- c("Abra",	"Agusan del Norte",	"Agusan del Sur",	"Aklan", "Albay",	"Antique",	"Apayao",	"Aurora",	"Basilan",	"Bataan",	"Batanes",	"Batangas",	"Benguet",	"Biliran","Bohol",	"Bukidnon",	"Bulacan",	"Cagayan",	"Camarines Norte",	"Camarines Sur",	"Camiguin",	"Capiz",	"Catanduanes",	"Cavite",	"Cebu",	"Compostela Valley",	"Davao Oriental",	"Davao del Norte",	"Davao del Sur",	"Dinagat Islands",	"Eastern Samar",	"Guimaras",	"Ifugao",	"Ilocos Norte",	"Ilocos Sur",	"Iloilo",	"Isabela",	"Kalinga",	"La Union",	"Laguna",	"Lanao del Norte",	"Lanao del Sur",	"Leyte",	"Maguindanao",	"Marinduque",	"Masbate",	"Misamis Occidental",	"Misamis Oriental",	"Mountain Province",	"National Capital Region",	"Negros Occidental",	"Negros Oriental",	"North Cotabato",	"Northern Samar",	"Nueva Ecija",	"Nueva Vizcaya",	"Occidental Mindoro",	"Oriental Mindoro",	"Palawan",	"Pampanga",	"Pangasinan",	"Quezon",	"Quirino",	"Rizal",	"Romblon",	"Samar",	"Sarangani",	"Siquijor",	"Sorsogon",	"South Cotabato",	"Southern Leyte",	"Sultan Kudarat",	"Sulu",	"Surigao del Norte",	"Surigao del Sur",	"Tarlac",	"Tawi-Tawi",	"Zambales",	"Zamboanga Sibugay",	"Zamboanga del Norte",	"Zamboanga del Sur")

#Correlation of the annual log production and forest cover by province
for(n in 1:81)
{flosscor[n,2] <- cor(time[1:8,n],forest_loss[1:8,n], method = "pearson")
 fCbmasscor[n,2] <- cor(time[1:8,n],biomass[1:8,n], method = "pearson")
}

#To test the significance if p-level is < 0.05 (Forest cover)
for(n in 1:81)
{ if((all(is.na(time[1:8,n]) == F))&(all(is.na(forest_loss[1:8,n]) == F))){ #Only print the p-values of specific province with complete data
        flosscor[n,3] <- cor.test(time[1:8,n],forest_loss[1:8,n], method = "pearson", conf.level=0.95)$p.value #Input the p-value in a matrix
            if(abs(cor.test(time[1:8,n],forest_loss[1:8,n], method = "pearson", conf.level=0.95)$p.value) < 0.05){ #Test the significance if p-level < 0.05
              flosscor[n,4] <- c("Yes")
            }else{
              flosscor[n,4] <- c("No")
            }
  }else{
      flosscor[n,3] <- c("NA")
      flosscor[n,4] <- c("No")
    }
}

#To test the significane if p-level is < 0.05 (Forest loss)
for(n in 1:81)
{ if((all(is.na(time[1:8,n]) == F))&(all(is.na(biomass[1:8,n]) == F))){ #Only print the p-values of specific province with complete data
        fCbmasscor[n,3] <- cor.test(time[1:8,n],biomass[1:8,n], method = "pearson", conf.level=0.95)$p.value  #Input the p-value in a matrix
            if(abs(cor.test(time[1:8,n],biomass[1:8,n], method = "pearson", conf.level=0.95)$p.value) < 0.05){ #Test the signifance if p-level < 0.05
              fCbmasscor[n,4] <- c("Yes")
            }else{
              fCbmasscor[n,4] <- c("No")
            }
  }else{
        fCbmasscor[n,3] <- c("NA")
        fCbmasscor[n,4] <- c("No")
  }
}

setwd("/automnt/ILCA/Hansen_Exploratory/R_codes_files/significant_trend_test/pearson/")
#Write into a .csv file
write.csv(flosscor, file = "forest_loss x time.csv", row.names = F) #To hide row numbers/names
write.csv(fCbmasscor, file = "biomass x time.csv", row.names = F)

#Note that the printed "NA"s symbolize that the data are
#incomplete or does not exist for that specific province

