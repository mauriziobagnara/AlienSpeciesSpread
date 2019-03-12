@ECHO OFF
ECHO R.exe
R.exe -e "library(CASPIAN);runCASPIAN(configFile='ConfigFile.R');q()"
PAUSE

