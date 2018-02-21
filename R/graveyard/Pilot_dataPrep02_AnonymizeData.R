rm(list = ls())
gc()
# ------------------------
# anonymize data
# ------------------------

# note that this script will not work as it is now. I just uploaded
# it for documentation purposes. What it does/ did is that it anonymized
# the datasets and created the data files that are now in the data folder.

# get a list of files and directory they're in
list.games <- list.files("C:/Users/Markus/Dropbox/Masterarbeit/DataAndAnalysis/DataPilotRun3/useData/", pattern = "_g.csv")
list.surveys <- list.files("C:/Users/Markus/Dropbox/Masterarbeit/DataAndAnalysis/DataPilotRun3/useData/", pattern = "_s.csv")

list.games <- paste0("C:/Users/Markus/Dropbox/Masterarbeit/DataAndAnalysis/DataPilotRun3/useData/", list.games)
list.surveys <- paste0("C:/Users/Markus/Dropbox/Masterarbeit/DataAndAnalysis/DataPilotRun3/useData/", list.surveys)

# loop through game files
for (ii in 1:length(list.games)){
  
  # read in files
  temp.game <- read.table(list.games[ii], header = T, sep = ",", as.is = T)
  
  # change workerid to a number
  temp.game$workerid <- paste0("id_", ifelse(ii < 10, "00", ifelse(ii < 100, "0", "")), ii)
  
  # save the file as .rds file
  write.table(temp.game, paste0("data/",
                                ifelse(ii < 10, "00", ifelse(ii < 100, "0", "")),
                                ii, "_g.txt"),
              row.names = F, sep = "\t")
  # saveRDS(temp.game, paste0("data/",
  #                           ifelse(ii < 10, "00", ifelse(ii < 100, "0", "")),
  #                           ii, "_g.rds"))
}

# loop through survey files
for (jj in 1:length(list.surveys)){
  
  # read in files
  temp.surveys <- read.table(list.surveys[jj], header = T, sep = ",", as.is = T)
  
  # change workerid to a number
  temp.surveys$workerid <- paste0("id_", ifelse(jj < 10, "00", ifelse(jj < 100, "0", "")), jj)
  
  # get rid of condition columns, with potential personal information
  temp.surveys <- temp.surveys[, !(names(temp.surveys) %in% c("completion.code",
                                                              "comments"))]
  
  # save the file as .rds file
  write.table(temp.surveys, paste0("data/",
                                   ifelse(jj < 10, "00", ifelse(jj < 100, "0", "")),
                                   jj, "_s.txt"),
              row.names = F, sep = "\t")
  # saveRDS(temp.surveys, paste0("data/",
  #                              ifelse(jj < 10, "00", ifelse(jj < 100, "0", "")),
  #                              jj, "_s.rds"))
}
  