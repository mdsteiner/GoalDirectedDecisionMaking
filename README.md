# Getting what you came for: Decisions under uncertainty with a goal

In this research we investigated the effect of the presence of a goal in different environments on the behavior of participants in an experience based learning task.

## File Structure

### Experiment Code
The experiment code is in the App folder. The code for the no goal condition is in the subfolder *NoGoalConditions*, the code for the goal condition in the subfolder *GoalConditions*. The apps won't work in their current form, as the file with the dropbox acces tokens is not included in the files.

### Study Data
Anonymized experiment data is in the folder *data/Study1Data/useData*. Raw data of the behavioral task is in the files with id_XXX_g_S1.txt. Raw data of the surveys is in the files with id_XXX_s_S1.txt. Trial, game and participant level datasets are in the .rds files in the same folder.

### Exclusion Logs
Logs of the performed exculsions are in the *documents* folder.

### Data Analysis Code
R code is in the *R* folder. Analysis scripts used for *Getting what you came for: Decisions under uncertainty with a goal* start with S1_XX_. The does not necessarily mean, that a script of a higher number won't work if not every other script with a lower number was executed before. The Stan code for the quantification of the difference between two proportions of answers given in one question of the survey, is in the *stan* subfolder.

### Plots
Plots from the manuscript as well as many more are as pdfs in the *plot* folder. Finding the right one might take some time, sorry...