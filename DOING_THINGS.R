first_code = 1
library('swirl')



caminho <- "C:\\Users\\estev\\Downloads\\14_310x_Intro_to_R"



setwd("C:\\Users\\estev\\Downloads")

uzp <- "asset-v1_MITxT+14.310x+3T2023+type@asset+block@14_310x_Intro_to_R.zip"

unzip(caminho, exdir = "C:\\Users\\estev\\Downloads\\")

install_course_zip(uzp,multi=FALSE)


