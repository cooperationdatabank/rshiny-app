# Mandatory fields for Long InputForm (add papers)
fieldsMandatory <- c("addAuthors", "addTitle", "addYearConducted", "addLanguage",
                     "addStudyNumberOfChoices", "addChoiceLow", "addChoiceHigh",
                     "addStudyDilemmaType",
                     'userName','userEmail')

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-
  ".mandatory_star { color: red; }
   #error { color: red; }"

# All fields (except for treatments) for Long InputForm (add papers)
fieldsAll <- c(
               # metadata
               "addAuthors", "addTitle", "addYearConducted", "addLanguage", "addPublicationStatus", "addAbstract",
               # Sample Characteristics
               "addYearOfDataCollection", "addYearSource", "addCountries", "addCountrySource", "addSampleSize",
               "addMaleProportion", "addMeanAge", "addAgeHigh", "addAgeLow", "addStudyStudentSample",
               "addStudyAcademicDiscipline", "addRecruitmentMethod",
               # Study Characteristics
               "addStudyExperimentalSetting", "addStudyDilemmaType","addStudyContinuousPGG","addStudySymmetric",
               "addStudyOneShot", "addStudyOneShotRepeated", "addStudyMatchingProtocol",
               "addStudyKnownEndgame", "addStudyShowUpFee","addStudyGameIncentive",
               "addStudyGroupSize","addStudyKindex","addStudyMPCR","addStudyPGDThreshold",
               "addReplenishmentRate","addDiscussion","addParticipantDecision",
               "addChoiceLow", "addChoiceHigh", "addStudyNumberOfChoices",
               "addDeception", "addStudyRealPartner", "addStudyAcquaintance","addSanction",
               # Overall cooperation
               "addStudyTrialOfCooperation", "addOverallProportionCooperation","addOverallMeanContributions",
               "addOverallMeanWithdrawal","addOverallStandardDeviation",
               "addOverallPercentageEndowmentContributed","addNumberOfObservations",

               "userEmail", 'userName'
)

metaAnalysisFields <- c(
  "analysisTitle", "analysisAuthor", "analysisPublicationYear", "effectSizeEstimate",
  "effectSizeVariance", "effectSizeSampleSize"
)

metaRegressionFields <- c(
  "regressionTitle", "regressionAuthor", "regressionYear",
  "regressionEffectSize",
  "regressionVariance",
  "regressionSampleSize"
)

# Save the submitted papers in the following location at the server
responsesDir <- file.path("submitted_papers")
epochTime <- function() {
  as.integer(Sys.time())
}

loadData <- function() {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  data <- dplyr::rbind_all(data)
  data
}



