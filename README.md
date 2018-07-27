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
R code is in the *R* folder. The numbers in front of the scripts indicate the order in which they should be executed such that they work (the first one only needs to be executed once to generate the datasets, since this has already been done 01_dataPreparation.R needs not be executed again). 02_mainAnalysis contains the preliminary analysis script (with some bug fixes), however the in the manuscript reported regressions, which combine the results of several regressions as planned initially in one single mixed effects model, are in 03_RegressionsOnPRisky.R, model fitting is done in 04_Optim_Fit.R.

### Plots
Plots from the manuscript are saved as pdfs in the *plot* folder.
