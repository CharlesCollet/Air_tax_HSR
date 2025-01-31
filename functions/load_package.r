list.of.packages <- c("tidyverse","readr","tidyr","tidyverse","dplyr","stringr","xlsx","geosphere","cartograflow","R.utils","lmtest","plm","estimatr","ggplot2","lfe","data.table","fixest","did2s","reporttools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(readr)
library(tidyr)
library(tidyverse)
library(dplyr)
library(stringr)
library(xlsx)
library(geosphere)
library(cartograflow)
library(R.utils)
library(lmtest)
library(plm)
library(estimatr)
library(ggplot2)
library(lfe)
library(tidyverse)
library(data.table)
library(fixest)
library(did2s)
library(reporttools)

