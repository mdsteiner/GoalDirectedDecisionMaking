# ------------------
#  BANDIT TASK
# ------------------

# Load libraries
library(shiny)
library(shinyjs)
library(rdrop2)

# User Interface
ui <- fixedPage(

  title = "The Boxes Game",
  uiOutput("MainAction"),
  includeCSS("style.css"),
#  tags$style(type = "text/css", ".recalculating {opacity: 1.0;}"),   # Prevents gray screen during Sys.sleep()
#  tags$style(type = "text/css", "#SelectA {margin-left: 40%;}"),
#  tags$style(type = "text/css", "#SelectB {margin-left: 40%;}"),
#  tags$style(type = "text/css", "#SelectC {margin-left: 40%;}"),
#  tags$style(type = "text/css", "#firstRow {margin-top: 3em;"),
  useShinyjs()


)

# Server
server <- function(input, output, session) {

# Stop the App from running when you close the browser window
#session$onSessionEnded(stopApp)

# --------------------------
# Dropbox Parameters
# --------------------------

EPtoken <- readRDS("EP_droptoken.rds")          # Reads in authentication for EP dropbox
outputDir <- "msteiner/GoalBanditJava/data"          # Determine dropbox output folder
idDir <- "msteiner/GoalBanditJava/ids"
expContrDir <- "msteiner/GoalBanditJava/expControll"


# --------------------------
# Participant parameters
# --------------------------


condition <- 1


# --------------------------
# Game Parameters
# --------------------------

autoInvalidate <- reactiveTimer(1, session)
# --------
# Set up reactive values
# These are objects that will change over the course of the game
# ---------

# CurrentValues stores scalers representing the latest game outcomes
CurrentValues <- reactiveValues(page = 1)




# CALCULATE COMPLETION CODE
completion.code <- paste0("EP-", sample(100:999, size = 1), "-", sample(100:999, size = 1), "-", sample(100:999, size = 1))

# --------
# PageLayouts
# ---------
{
PageLayouts <- reactive({

# 1) WELCOME PAGE
if (CurrentValues$page == 1) {

  return(
    div(class = "page1", checked = NA,
    list(
      h2("Decision Making - Part 2", class = "firstRow"),
      p("Before you start with the questionnaires, please enter your WorkerID below."),
      p("Again, your answers to all future questions will not affect your bonus. However, please make sure to complete the rest of the survey for your work and bonus to be accepted."),
      p("Please enter your mturk WorkerID below and click Continue."),
      textInput(inputId = "workerid",
                label = "Please enter your WorkerID",
                value = "",
                placeholder = "e.g.; AXLDKGHSJM"),
      # This displays the action putton Next.
     disabled(actionButton(inputId = "continue17",
                  label = "Continue", class = "continueButtons"))
    )
    )
  )}

# INSTRUCTIONS INTRO


# 17) Post Game Survey A

if (CurrentValues$page == 17) {
  return(
    div(class = "page17", checked = NA,
    list(
      p(strong("How difficult did you find the game? How difficult was it to earn points?"), br(), "Please give a response between 1 and 5, with 1 being extremely easy, and 5 being extremely difficult.", class = "firstRow"),
      radioButtons("gameDifficulty",
                   label = NULL,
                   choices = list("1 - Extremely easy" = 1,
                                  "2 - Somewhat easy" = 2,
                                  "3 - Neither easy nor difficult" = 3,
                                  "4 - Somewhat difficult" = 4,
                                  "5 - Extremely difficult" = 5),
                   selected = character(0)),
      p(strong("What was your strategy in the game?"), br(), "Please be as descriptive as possible, we are very interested in how people play the game."),
      textAreaInput(inputId = "strategy",
                    label = NULL,
                    placeholder = "I tried to...", resize = "both"),
      p(strong("Did you change your strategy over the games? If so, what did you do differently?")),
      textAreaInput(inputId = "strategyChange",
                    label = NULL,
                    placeholder = NULL, resize = "both"),
      p(strong("Which of the following two strategies best describes how you made selections in the game?")),
      radioButtons("whichStrategy",
                   label = NULL,
                   choices = list("I always tried to select the box that gives the most points on average." = 1,
                                  "I first looked at how many clicks I had left and how many points I had. Then, I selected one box or the other." = 2),
                   selected = character(0),
                   width = "600px"),
      p(strong("Have you played a similar task as this one on the MTurk?")),
      radioButtons("similarTask",
                   label = NULL,
                   choices = list("Yes, I have played similar tasks." = 1,
                                  "No, this was the first time." = 2),
                   selected = character(0),
                   width = "600px"),
      p(strong("If you have any additional comments about the game that you'd like to share with us, please describe them here:"), br(), "For example, did you think it was interesting or boring? Is there something we could do to improve the game?"),
      textAreaInput("comments",
                    label = NULL,
                    resize = "both"),

      disabled(actionButton(inputId = "continue18",
                   label = "Continue", class = "continueButtons"))
    )
  )
  )
}

# 18) Post Game Survey B

if (CurrentValues$page == 18) {
  return(
    div(class = "page18", checked = NA,
    list(
  radioButtons("instructionsClear",
               label = "Were the instructions to the Boxes game clear?",
               choices = list("Yes, the instructions were clear and I think I understood how the game worked from the beginning." = 1,
                              "No, the instructions were not clear and I did not fully understand how the game worked before starting." = 2),
               selected = character(0),
               width = "500px"),
  p(strong("If there was anything in particular that you did not understand about the game, please describe it here:"),
    class = "inputTitle"),
  textAreaInput("notUnderstood",
                label = "",
                resize = "both",
                width = "500px"),
  radioButtons("errorOrBugs",
               label = "Did you experience any computer errors or bugs while playing the Boxes game?",
               choices = list("No, I did not experience any computer errors or bugs in the game." = 1,
                              "Yes, I did experience an error or bug." = 2),
               selected = character(0),
               width = "500px"),
  p(strong("If you noticed a computer error or bug, please describe it here:"),
    class = "inputTitle"),
  textAreaInput("describeError",
                label = "",
                resize = "both",
                width = "500px"),
  disabled(actionButton(inputId = "continue19",
               label = "Continue", class = "continueButtons"))
    )
  )
  )
}


# 19) Post Game Survey B



if (CurrentValues$page == 19) {
  return(
    div(class = "page19", checked = NA,
    list(
      tags$br(), tags$br(), tags$br(),
      radioButtons("gaveUp",
                   label = "Did you give up and stop caring about earning points at any time during the games?",
                   choices = list("Never. I never gave up trying to earn as many points as possible" = 1,
                                  "Sometimes. During one or more games I gave up trying to earn points" = 2,
                                  "Often. During many games I gave up trying to earn points" = 3),
                   selected = character(0),
                   width = "500px"),
      radioButtons("tookNotes",
                   label = "Did you take any notes while playing the games?",
                   choices = list("No, I did not take any notes while playing the games" = 2,
                                  "Yes, I took notes while playing the games" = 1),
                   selected = character(0),
                   width = "500px"),
      radioButtons("usedCalculator",
                   label = "Did you use a calculator or an equivalent calculation device?",
                   choices = list("No, I did not use a calculator or similar device" = 2,
                                  "Yes, I did use a calculator or similar device" = 1),
                   selected = character(0),
                   width = "500px"),
      radioButtons("gotHelp",
                   label = "Did you get any other help?",
                   choices = list("No, I did not get any additional help" = 2,
                                  "Yes, I received some form of help" = 1),
                   selected = character(0),
                   width = "500px"),
      disabled(actionButton(inputId = "continue20",
                   label = "Continue", class = "continueButtons"))
    )
  )
  )
}

# 20) Numeracy Test

if (CurrentValues$page == 20) {
  return(
    div(class = "page20", checked = NA,
    list(
      p(strong("Please do not use any helping device such as a calculator to answer the following question."), class = "firstRow"),
      p("Out of 1,000 people in a small town 500 are members of a choir. Out of these 500 members in the choir 100 are men. Out of the 500 inhabitants that are not in the choir 300 are men. What is the probability that a randomly drawn man is a member of the choir? (please indicate the probability in percent)."),
      textInput("BNT",
                label = "",
                placeholder = "XX % (Just enter the number without the % sign...)",
                width = "500px"),
      disabled(actionButton(inputId = "continue21",
                   label = "Continue", class = "continueButtons"))
    )
  )
  )
}

# 21) CHECK IF NUMERACY ITEM WAS KNOWN

if (CurrentValues$page == 21) {
  return(
    div(class = "page21", checked = NA,
    list(
      radioButtons("knewBNT",
                   label = "Did you already know the question you just answered e.g. from another experiment?",
                   choices = list("Yes, I already knew the question." = 1,
                                  "No, this was the first time I answered this question." = 2),
                   selected = character(0),
                   width = "500px"),
      disabled(actionButton(inputId = "continue22",
                   label = "Continue", class = "continueButtons"))
    )
  )
  )
}

# 22) FINAL DEMOGRAPHICS
if (CurrentValues$page == 22) {

 return(
   div(class = "page22", checked = NA,
       list(
             radioButtons("sex",
                          label = "What is your sex?",
                          choices = list("Male" = 1, "Female" = 2, "Other" = 3),
                          selected = character(0)),
             numericInput("age",
                          label = "What is your age?",
                          value = 0),
             radioButtons("interesting",
                          label = "How interesting did you find the Boxes Game?",
                         choices = c("1 - Not at all" =  1,
                                     "2" = 2,
                                     "3" = 3,
                                     "4" = 4,
                                     "5 - Very Much" = 5
                                     ),
                         selected = character(0)),
             radioButtons("education",
                          label = "What is the highest level of education you have completed?",
                          choices = list("No high school degree" = 1,
                                         "High school degree" = 2,
                                         "2-year post-high school degree (e.g.; Associates)" = 3,
                                         "Bachelor's degree (BA) or equivalent" = 4,
                                         "Master's degree (MA) or equivalent" = 5,
                                         "Doctoral degree (PhD) or equivalent" = 6),
                          selected = character(0),
                          width = "500px"),
             p(strong("Are you a native English speaker?"), br(), "This is an attention check. Please answer this question by entering \"other\". In addition, in the next box at the bottom of this page, please type \"I read the instructions\""),
             radioButtons("attentionCheck",
                          label = NULL,
                          choices = list("Yes" = 1, "No" = 2, "Other" = 3),
                          selected = character(0)),
             p(strong("Can we trust your data for scientific research? For example, if you did not pay attention during the game, cheated by using the JavaScript console or were intoxicated (etc.), please answer \"No\"."), br(), "Please answer honestly, you", em("WILL"), "receive full payment and bonus regardless of your answer!"),
             radioButtons("trustData",
                          label = NULL,
                          choices = list("Yes, you can trust my data for scientific research" = 1,
                                         "No, you may not want to trust my data for scientific research" = 2),
                          selected = character(0),
                          width = "500px"),
             p(strong("How did you find this HIT?")),
             radioButtons("foundHIT",
                          label = NULL,
                          choices = list("Searched on MTurk" = 1,
                                         "Discovered on an external Website or Messenger" = 2,
                                         "Other" = 3),
                          selected = character(0),
                          width = "500px"),
             textInput("textAttentionCheck",
                       label = NULL),
        disabled(actionButton(inputId = "end",
                     label = "End Study", class = "continueButtons"))))
 )
}

# 23) Goodbye
if(CurrentValues$page == 23) {

 return(
   div(class = "page23", checked = NA,
       list(
   h3("Thank you for your participation!", class = "firstRow"),
   p("Here is your study completion code. Please write it down:"),
   h2(completion.code),
   p("The purpose of this research is to understand how people make risky decisions."),
   p("If you have any questions about this study, please contact us at turkpsych@gmail.com and we will be happy to answer them."),
   p("There have been attention checks. These are only used to decide whether the data will be used for the analysis and will not affect your payment!"),
   p("Once you have recorded your code, you may close this window")
 )))
}

})

# Send dynamic UI to ui
output$MainAction <- renderUI( {
  PageLayouts()
})
}

# --------
# Define button actions
# Dynamic user interface of pages
# ---------


# Page Continue button
 observeEvent(input$continue17, {
#   disable("continue2")
   onlyID <- data.frame("workerID" = input$workerid)
   # Write survey data
   IDDatafileName <- paste0(input$workerid, as.integer(Sys.time()), digest::digest(onlyID), "_s.csv")
   IDDatafilePath <- file.path(tempdir(), IDDatafileName)
   write.csv(onlyID, IDDatafilePath, row.names = FALSE, quote = TRUE)
   rdrop2::drop_upload(IDDatafilePath, dest = idDir, dtoken = EPtoken)
   CurrentValues$page <- 17
 })

 observeEvent(input$continue18, {
#   disable("continue18")
   CurrentValues$page <- 18
 })

 observeEvent(input$continue19, {
#   disable("continue19")
   CurrentValues$page <- 19
 })

 observeEvent(input$continue20, {
#   disable("continue20")
   CurrentValues$page <- 20
 })

 observeEvent(input$continue21, {
#   disable("continue21")
   CurrentValues$page <- 21
 })

 observeEvent(input$continue22, {
#   disable("continue22")
   CurrentValues$page <- 22
 })


# Check for end of study
 observeEvent(input$end, {

  # Create progress message
  withProgress(message = "Saving data...",
              value = 0, {

  incProgress(.25)
  # Write Survey data

  if(length(input$comments) == 0) {comments.i <- NA} else {comments.i <- input$comments}
  if(length(input$age) == 0) {age.i <- NA} else {age.i <- input$age}
  if(length(input$sex) == 0) {sex.i <- NA} else {sex.i <- input$sex}
  if(length(input$gameDifficulty) == 0) {gameDifficulty.i <- NA} else {gameDifficulty.i <- input$gameDifficulty}
  if(length(input$strategy) == 0) {strategy.i <- NA} else {strategy.i <- input$strategy}
  if(length(input$strategyChange) == 0) {strategyChange.i <- NA} else {strategyChange.i <- input$strategyChange}
  if(length(input$whichStrategy) == 0) {whichStrategy.i <- NA} else {whichStrategy.i <- input$whichStrategy}
  if(length(input$similarTask) == 0) {similarTask.i <- NA} else {similarTask.i <- input$similarTask}
  if(length(input$instructionsClear) == 0) {instructionsClear.i <- NA} else {instructionsClear.i <- input$instructionsClear}
  if(length(input$notUnderstood) == 0) {notUnderstood.i <- NA} else {notUnderstood.i <- input$notUnderstood}
  if(length(input$errorOrBugs) == 0) {errorOrBugs.i <- NA} else {errorOrBugs.i <- input$errorOrBugs}
  if(length(input$describeError) == 0) {describeError.i <- NA} else {describeError.i <- input$describeError}
  if(length(input$gaveUp) == 0) {gaveUp.i <- NA} else {gaveUp.i <- input$gaveUp}
  if(length(input$tookNotes) == 0) {tookNotes.i <- NA} else {tookNotes.i <- input$tookNotes}
  if(length(input$usedCalculator) == 0) {usedCalculator.i <- NA} else {usedCalculator.i <- input$usedCalculator}
  if(length(input$gotHelp) == 0) {gotHelp.i <- NA} else {gotHelp.i <- input$gotHelp}
  if(length(input$BNT) == 0) {BNT.i <- NA} else {BNT.i <- input$BNT}
  if(length(input$knewBNT) == 0) {knewBNT.i <- NA} else {knewBNT.i <- input$knewBNT}
  if(length(input$interesting) == 0) {interesting.i <- NA} else {interesting.i <- input$interesting}
  if(length(input$education) == 0) {education.i <- NA} else {education.i <- input$education}
  if(length(input$attentionCheck) == 0) {attentionCheck.i <- NA} else {attentionCheck.i <- input$attentionCheck}
  if(length(input$trustData) == 0) {trustData.i <- NA} else {trustData.i <- input$trustData}
  if(length(input$foundHIT) == 0) {foundHIT.i <- NA} else {foundHIT.i <- input$foundHIT}
  if(length(input$textAttentionCheck) == 0) {textAttentionCheck.i <- NA} else {textAttentionCheck.i <- input$textAttentionCheck}
  if(condition %in% 1:2){caredReachGoal.i <- NA}else{if(length(input$caredReachGoal) == 0){caredReachGoal.i <- NA} else {caredReachGoal.i <- input$caredReachGoal}}
  # Write Survey data
  SurveyData.i <- data.frame("workerid" = input$workerid,
                             "age" = age.i,
                             "sex" = sex.i,
                             "comments" = comments.i,
                             "completion.code" = completion.code,
                             "game.difficulty" = gameDifficulty.i,
                             "strategy" = strategy.i,
                             "strategy.change" = strategyChange.i,
                             "which.strategy" = whichStrategy.i,
                             "attention.clicks" = NA,
                             "instructions.clear" = instructionsClear.i,
                             "similar.task" = similarTask.i,
                             "not.understood" = notUnderstood.i,
                             "error.or.bugs" = errorOrBugs.i,
                             "describeError" = describeError.i,
                             "gave.up" = gaveUp.i,
                             "tookNotes" = tookNotes.i,
                             "usedCalculator" = usedCalculator.i,
                             "got.help" = gotHelp.i,
                             "BNT" = BNT.i,
                             "knewBNT" = knewBNT.i,
                             "interesting" = interesting.i,
                             "education" = education.i,
                             "attention.check" = attentionCheck.i,
                             "trust.data" = trustData.i,
                             "foundHIT" = foundHIT.i,
                             "text.attention.check" = textAttentionCheck.i,
                             "condition" = condition,
                             "caredReachGoal" = caredReachGoal.i)
  incProgress(.5)

  # Write survey data
  SurveyDatafileName <- paste0(input$workerid, as.integer(Sys.time()), digest::digest(SurveyData.i), "_s.csv")
  SurveyDatafilePath <- file.path(tempdir(), SurveyDatafileName)
  write.csv(SurveyData.i, SurveyDatafilePath, row.names = FALSE, quote = TRUE)
  rdrop2::drop_upload(SurveyDatafilePath, dest = outputDir, dtoken = EPtoken)

  incProgress(.75)

  CurrentValues$page <- 23
  Sys.sleep(.5)
  incProgress(1)

                })

 })

 # Check for end of game

observe({

    # Check if input was given and enable and disable the continue button
  if(CurrentValues$page == 1){

    if(!is.null(input$workerid)){

      if(nchar(as.character(input$workerid)) > 4){

        enable("continue17")

      }
    }
  }

  if(CurrentValues$page == 17){

    if(!is.null(input$gameDifficulty) &
       !is.null(input$strategy) &
       !is.null(input$strategyChange) &
       !is.null(input$whichStrategy)){

      if(input$gameDifficulty %in% c(1:5) &
         nchar(as.character(input$strategy)) > 1 &
         nchar(as.character(input$strategyChange)) > 1 &
         input$whichStrategy %in% c(1, 2)){

        enable("continue18")

      }
    }
  }

  if(CurrentValues$page == 18){

    if(!is.null(input$instructionsClear) &
       !is.null(input$errorOrBugs)){

      if(input$instructionsClear %in% c(1, 2) &
         input$errorOrBugs %in% c(1, 2)){

        enable("continue19")

      }
    }
  }

  if(CurrentValues$page == 19){

    if(!is.null(input$gaveUp) &
       !is.null(input$tookNotes) &
       !is.null(input$usedCalculator) &
       !is.null(input$gotHelp)){

      if(input$gaveUp %in% c(1:3) &
         input$tookNotes %in% c(1, 2) &
         input$usedCalculator %in% c(1, 2) &
         input$gotHelp %in% c(1, 2)){

        enable("continue20")

      }
    }
  }

  if(CurrentValues$page == 20){

    if(!is.null(input$BNT)){

      if(nchar(as.character(input$BNT)) >= 1){

        enable("continue21")

      }
    }
  }

  if(CurrentValues$page == 21){
    if(!is.null(input$knewBNT)){
      if(input$knewBNT %in% c(1, 2)){

        enable("continue22")

      }
    }
  }

  if(CurrentValues$page == 22){
    if(!is.null(input$sex) &
       !is.null(input$interesting) &
       !is.null(input$education) &
       !is.null(input$attentionCheck) &
       !is.null(input$trustData)){

      if(input$sex %in% 1:3 &
         input$interesting %in% 1:5 &
         input$education %in% 1:6 &
         input$attentionCheck %in% 1:3 &
         input$trustData %in% 1:2){

        enable("end")

      }
    }
  }
})

}

# Create app!
shinyApp(ui = ui, server = server)
