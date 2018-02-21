#Libraries====
if (!require(tidyverse)) library(tidyverse) # load packages if not in yet 
if (!require(readr)) library(readr) # load packages if not in yet 

#Sample Data=====
#Straight from web:
WOTHS <- read_delim("https://raw.githubusercontent.com/PWaryszak/TURF/master/sample_data_WOTH_TURF_2018.csv", ";", escape_double = FALSE, trim_ws = TRUE)
dim(WOTHS)#2105   14

#If above code does not work,"sample_data_WOTH_TURF_2018.csv" can be found online here:https://github.com/PWaryszak/TURF
#So try to download first into your working directory and then go:
WOTHS <- read_delim("sample_data_WOTH_TURF_2018.csv", ";", escape_double = FALSE, trim_ws = TRUE)
dim(WOTHS)#2105   14

#Data Summarizing Pipeline:=====
WOTH.New.One.Pipeline<-WOTHS%>%distinct(WOTHS)%>%
  select(SCIENTIFIC,COUNTRY,LONGITUDE, LATITUDE, LOCALITY_ID, MM,ZonVida2,SRTM_W_250)%>%
  mutate(Season = ifelse(MM==1|MM==2|MM==10|MM==11|MM==12,"winter",
                         ifelse(MM==9, "fall",
                         ifelse(MM==3|MM==4,"spring", "summer"))))%>%
  group_by(COUNTRY,LOCALITY_ID,ZonVida2,Season,SRTM_W_250) %>%
  distinct(WOTH.New)%>%
  group_by(COUNTRY,ZonVida2, Season) %>%
  summarize(total= n(),Elev.mean=mean(SRTM_W_250), Elev.sd=sd(SRTM_W_250), Elev.min=min(SRTM_W_250), Elev.max=max(SRTM_W_250))%>%
  na.omit()

View(WOTH.New.One.Pipeline)
=======
#Libraries====
if (!require(tidyverse)) library(tidyverse) # load packages if not in yet 
if (!require(readr)) library(readr) # load packages if not in yet 

#Sample Data=====
#Straight from web:
WOTHS <- read_delim("https://raw.githubusercontent.com/PWaryszak/TURF/master/sample_data_WOTH_TURF_2018.csv", ";", escape_double = FALSE, trim_ws = TRUE)
dim(WOTHS)#2105   14

#If above code does not work,"sample_data_WOTH_TURF_2018.csv" can be found online here:https://github.com/PWaryszak/TURF
#So try to download first into your working directory and then go:
WOTHS <- read_delim("sample_data_WOTH_TURF_2018.csv", ";", escape_double = FALSE, trim_ws = TRUE)
dim(WOTHS)#2105   14

#Data Summarizing Pipeline:=====
WOTH.New.One.Pipeline<-WOTHS%>%distinct(WOTHS)%>%
  select(SCIENTIFIC,COUNTRY,LONGITUDE, LATITUDE, LOCALITY_ID, MM,ZonVida2,SRTM_W_250)%>%
  mutate(Season = ifelse(MM==1|MM==2|MM==10|MM==11|MM==12,"winter",
                         ifelse(MM==9, "fall",
                         ifelse(MM==3|MM==4,"spring", "summer"))))%>%
  group_by(COUNTRY,LOCALITY_ID,ZonVida2,Season,SRTM_W_250) %>%
  distinct(WOTH.New)%>%
  group_by(COUNTRY,ZonVida2, Season) %>%
  summarize(total= n(),Elev.mean=mean(SRTM_W_250), Elev.sd=sd(SRTM_W_250), Elev.min=min(SRTM_W_250), Elev.max=max(SRTM_W_250))%>%
  na.omit()

View(WOTH.New.One.Pipeline)
>>>>>>> 7d8ac0335192870029b814969a8ba6bd26ead926
