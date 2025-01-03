:: run_comola.bat
:: Version: 0.2.0
:: Date:    16.05.2024
:: Author: Christoph Schürz
:: email:  christoph.schuerz@ufz.de
::
:: This script runs the entire OPTAIN CoMOLA workflow. 
:: It first runs an initialization R script which does some settings and performs general checks on the project.
:: Then it calls the CoMOLA workflow by running the python script __init__.py

@ECHO OFF
ECHO Initializing CoMOLA run...
:: Delete error log from previous runs if file exists.
IF EXIST "error_log.txt" (
  DEL "error_log.txt"
)
:: Delete init.R from previous runs if file exists.
::IF EXIST "init.Rout" (
::  DEL init.Rout
::)
:: Read the R and python paths from the config.ini to specifically use those for executing R and python scripts
FOR /F "tokens=2 delims==" %%r in ('FINDSTR /B /N "file_path_R = " config.ini') DO SET "r_path=%%r"
FOR /F "tokens=2 delims==" %%p in ('FINDSTR /B /N "file_path_Python = " config.ini') DO SET "python_path=%%p"
:: Check if defined R and python executable files exist.
IF NOT EXIST %r_path% (
  ECHO No R.exe file can be found in %r_path%
  ECHO Please update the 'file_path_R' in 'config.ini'
  ECHO Press any key to quit...
  PAUSE>nul
  EXIT /B 0
)
IF NOT EXIST %python_path% (
  ECHO No python.exe file can be found in %python_path%
  ECHO Please update the 'file_path_Python' in 'config.ini'
  ECHO Press any key to quit...
  PAUSE>nul
  EXIT /B 0
)
:: Run initialization and perform checks on CoMOLA setup.
:: %r_path% CMD BATCH init.R
:: Check if errors occured and stop batch script.
:: IF EXIST "error_log.txt" (
::  ECHO Initialization resulted in errors! See 'error_log.txt' and fix reported issues before re-running.
::  ECHO Press any key to quit...
:: PAUSE>nul
::  EXIT /B 0
::)
:: Delete R output after running init.R
::IF EXIST "init.Rout" (
::  DEL init.Rout
::)
:: Start CoMOLA routine.
ECHO Start of CoMOLA routine
ECHO -----------------------
:: Ask user to define the number of cores for CoMOLA run.
:INPUT
SET /P ncore=Define the number of cores for running CoMOLA: 
SETLOCAL ENABLEDELAYEDEXPANSION
ECHO !ncore!| FINDSTR /r "^[0-9]*$"
IF ERRORLEVEL 1 (
    ECHO Invalid input. Please enter an integer.
    GOTO input
)
:: Run CoMOLA python script
%python_path% __init__.py -t !ncore!