# --------------------
# Create Folder with the usable data
# --------------------
# note that this script will not work as it is now. I just uploaded
# it for documentation purposes. What it does/ did is that it put the usable data
# in a one folder and the one we could not use in another. The usable data is now
# in anonymized form (see script dataPrepAnonymizeData.R) in the data folder.


# Data-Folder names 
batches <- c("Batch1", "Batch2", "Batch3")

# loop through these folders
for (ba in batches){
  
  # list the files that contain the id variables
  setwd(paste0("C:/Users/Markus/Dropbox/Masterarbeit/DataAndAnalysis/DataPilotRun3/Ids", ba))
  fil <- list.files()
  
  # read in the id files and save the ids in a matrix
  ids <- matrix(NA, ncol = 1, nrow = length(fil))
  for (ii in 1:length(fil)){
    temp.df <- read.table(fil[ii], header = T, sep = ",")
    ids[ii, 1] <- as.character(temp.df[1, 1])
  }
  
  # get the ids of those who either only accessed one part of the study or when it crashed
  table.ids <- table(ids)
  exclude1 <- names(table.ids[table.ids != 2])
  unique.ids <- unique(ids)
  
  # copy the datafiles to a new directory using command prompt
  for (fi in 1:length(unique.ids)){
    
    # for the ones with exclusion criteria
    if(unique.ids[fi] %in% exclude1){
      
      # list the files
      data.files <- list.files(paste0("C:/Users/Markus/Dropbox/Masterarbeit/DataAndAnalysis/DataPilotRun3/", ba),
                               pattern = unique.ids[fi])
      
      if (length(data.files) > 0){
        for (jj in 1:length(data.files)){
          
          # create the command with the filenames
          filename <- data.files[jj]
          command = paste0("copy ",
                           "C:\\Users\\Markus\\Dropbox\\Masterarbeit\\DataAndAnalysis\\DataPilotRun3\\", ba, "\\",
                           filename,
                           " C:\\Users\\Markus\\Dropbox\\Masterarbeit\\DataAndAnalysis\\DataPilotRun3\\errorData\\",
                           filename)
          
          # send command to the console
          system2("cmd.exe", input = command)
        }
      }
    } else {
      
      # list the files
      data.files <- list.files(paste0("C:/Users/Markus/Dropbox/Masterarbeit/DataAndAnalysis/DataPilotRun3/", ba),
                               pattern = unique.ids[fi])
      
      
      if (length(data.files) > 0){
        for (jj in 1:length(data.files)){
          
          # create the command with the filenames
          filename <- data.files[jj]
          command = paste0("copy ",
                           "C:\\Users\\Markus\\Dropbox\\Masterarbeit\\DataAndAnalysis\\DataPilotRun3\\", ba, "\\",
                           filename,
                           " C:\\Users\\Markus\\Dropbox\\Masterarbeit\\DataAndAnalysis\\DataPilotRun3\\useData\\",
                           filename)
          
          # send command to the console
          system2("cmd.exe", input = command)
        }
      }
    }
  }
}
