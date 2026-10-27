# =============================================================================
# Early Identification of Students at Risk of Not Returning
# Retention prediction model: full workflow in one script
#
# Author: Emmanuel Assefuah Ocran
#
# What this script does
#   1. Loads a student level retention data set
#   2. Cleans it, fills in missing test scores and GPA, and builds an
#      engagement index
#   3. Screens candidate predictors (correlation, VIF, stepwise selection)
#   4. Compares several models on one train/test split
#   5. Re-tests each model many times on random samples (resampling study)
#   6. Fits the final Random Forest model, picks a cutoff from the ROC curve,
#      and scores every current student
#
# Data privacy
#   No student data is included with this script. File locations are
#   placeholders set in the CONFIG section. The student level output in
#   Part 7 contains individual risk scores; store it only where your
#   institution permits FERPA protected data.
#
# To try the script without real data, set use_demo_data = TRUE in CONFIG.
# A small made-up data set with the same columns will be generated.
# =============================================================================


# -----------------------------------------------------------------------------
# PART 0. Packages and settings
# -----------------------------------------------------------------------------

library(readxl)        # read the Excel data extract
library(writexl)       # write results to Excel
library(dplyr)         # data cleaning
library(VIM)           # kNN imputation of missing values
library(caret)         # train/test split and confusion matrix
library(randomForest)  # Random Forest
library(gbm)           # Gradient Boosting
library(glmnet)        # Ridge and Lasso regression
library(pROC)          # ROC curves and cutoff selection
library(MASS)          # stepwise selection (stepAIC)
library(car)           # variance inflation factor (VIF)
library(rpart)         # decision tree
library(corrplot)      # correlation plot

# Note: MASS also has a select() function. Use dplyr's version explicitly.
select <- dplyr::select

CONFIG <- list(
  # Input file: one row per student per term (placeholder name)
  data_file = "retention_data.xlsx",

  # Folder where results are saved
  output_dir = "output",

  # Set to TRUE to run on a made-up data set instead of real data
  use_demo_data = FALSE,

  # Random seed so results can be reproduced
  seed = 123,

  # Terms coded at or after this value do not yet have a known
  # return outcome. They are scored but not used for training.
  # Term codes are YYYYTT (for example 202610).
  first_unobserved_term = 202610,

  # Share of known records used for training (the rest is the test set)
  train_share = 0.7
)

dir.create(CONFIG$output_dir, showWarnings = FALSE)

# Outcome: 1 = student returned the following year, 0 = did not return
OUTCOME <- "YEAR_RETRN_IND"

# Final predictor set (selected in Part 4)
PREDICTORS_FINAL <- c("INVOL_IND", "REPORT_CREDIT_HRS", "PELL_IND",
                      "DROP_HOURS", "YEARAID", "YEARCOA_ALL")

# Broader predictor set used in earlier model comparisons
PREDICTORS_EXTENDED <- c("HS_GPA", "ENTRY_CREDITS", "INVOL_IND",
                         "REPORT_CREDIT_HRS", "REG_TIMEDIFF_DAYS", "TERMAID",
                         "YEARCOA", "PERCENT_UNMEET_NEED_YEAR")

# Full candidate list used at the start of variable screening
PREDICTORS_CANDIDATE <- c("ACT", "HS_GPA", "AGE", "ENTRY_CREDITS",
                          "DEV_CRED_HOURS2", "INVOL_IND", "REPORT_CREDIT_HRS",
                          "PELL_IND", "REG_TIMEDIFF_DAYS", "DROP_HOURS",
                          "DROP_COUNT", "YEARAID", "YEARCOA_ALL",
                          "PERCENT_UNMEET_NEED_YEAR_ALL")

# Yes/No columns stored as "TRUE"/"FALSE" text in the extract
YES_NO_COLUMNS <- c("FIRST_GEN_STUDENT", "CITIZENSHIP_IND", "ONLINE_STUDENT_IND",
                    "FULLTIME_IND", "HOUSING_IND", "ACCESSIBILITY_SERVICES_IND",
                    "TUTORIAL_SERVICES_IND", "SPORT_IND", "FRAT_IND")


# -----------------------------------------------------------------------------
# PART 1. Load the data
# -----------------------------------------------------------------------------

# Builds a made-up data set with the same columns as the real extract.
# Used only when CONFIG$use_demo_data is TRUE. Values are random.
make_demo_data <- function(n = 4000, seed = 1) {
  set.seed(seed)
  yn <- function(p) ifelse(runif(n) < p, "TRUE", "FALSE")
  d <- data.frame(
    STUDENT_KEY                  = seq_len(n),
    TERM_CODE                    = sample(c(202310, 202410, 202510, 202610), n, TRUE),
    ACT                          = round(rnorm(n, 21, 4)),
    HS_GPA                       = round(pmin(4, pmax(1.5, rnorm(n, 3.2, 0.5))), 2),
    AGE                          = sample(17:30, n, TRUE, prob = c(2, 30, 30, rep(1, 11))),
    ENTRY_CREDITS                = rpois(n, 6),
    DEV_CRED_HOURS2              = rpois(n, 1),
    REPORT_CREDIT_HRS            = sample(6:18, n, TRUE),
    PELL_IND                     = rbinom(n, 1, 0.4),
    REG_TIMEDIFF_DAYS            = rpois(n, 60),
    DROP_HOURS                   = rpois(n, 1),
    DROP_COUNT                   = rpois(n, 0.5),
    TERMAID                      = round(runif(n, 0, 9000)),
    YEARAID                      = round(runif(n, 0, 18000)),
    YEARCOA                      = round(runif(n, 18000, 30000)),
    YEARCOA_ALL                  = round(runif(n, 18000, 30000)),
    PERCENT_UNMEET_NEED_YEAR     = round(runif(n, 0, 100)),
    PERCENT_UNMEET_NEED_YEAR_ALL = round(runif(n, 0, 100)),
    TERM_GENDER                  = sample(c("F", "M"), n, TRUE),
    FIRST_GEN_STUDENT            = yn(0.35),
    CITIZENSHIP_IND              = yn(0.95),
    ONLINE_STUDENT_IND           = yn(0.15),
    FULLTIME_IND                 = yn(0.85),
    HOUSING_IND                  = yn(0.55),
    ACCESSIBILITY_SERVICES_IND   = yn(0.05),
    TUTORIAL_SERVICES_IND        = yn(0.20),
    SPORT_IND                    = yn(0.08),
    FRAT_IND                     = yn(0.15),
    CAMPUS_PROGRAM_IND           = rbinom(n, 1, 0.30),
    SPRING_IND                   = rbinom(n, 1, 0.3),
    WHITE_IND                    = rbinom(n, 1, 0.7),
    IN_STATE_IND                 = rbinom(n, 1, 0.8),
    COUNTY_CODE                  = sample(1:100, n, TRUE)
  )
  # Outcome loosely tied to a few predictors so models have a signal
  score <- -1 + 0.12 * d$REPORT_CREDIT_HRS - 0.35 * d$DROP_HOURS +
    0.00005 * d$YEARAID + 0.6 * (d$HOUSING_IND == "TRUE") - 0.3 * d$PELL_IND
  d$YEAR_RETRN_IND <- rbinom(n, 1, plogis(score))
  # Add some missing values to test scores and GPA
  d$ACT[sample(n, n * 0.15)]    <- NA
  d$HS_GPA[sample(n, n * 0.05)] <- NA
  d
}

if (CONFIG$use_demo_data) {
  raw_data <- make_demo_data()
} else {
  raw_data <- as.data.frame(read_excel(CONFIG$data_file))
}

# Keep the student key out of every model. It is used only to link
# scores back to students in the secure output file.
ID_COLUMNS <- c("STUDENT_KEY")


# -----------------------------------------------------------------------------
# PART 2. Check missing values
# -----------------------------------------------------------------------------

missing_summary <- data.frame(
  Column            = names(raw_data),
  MissingValues     = sapply(raw_data, function(x) sum(is.na(x))),
  MissingPercentage = sapply(raw_data, function(x) mean(is.na(x)) * 100)
)
print(missing_summary)


# -----------------------------------------------------------------------------
# PART 3. Prepare the data
# -----------------------------------------------------------------------------

# prepare_data() runs every cleaning step in order.
#   impute_vars      columns to fill in with kNN imputation
#   drop_incomplete  TRUE removes any row still missing a value
prepare_data <- function(raw, impute_vars = c("ACT", "HS_GPA"),
                         drop_incomplete = FALSE) {

  # Step 3.1 Fill in missing values using the 5 most similar students (kNN)
  d <- kNN(raw, variable = impute_vars, k = 5)

  # kNN adds helper columns ending in "_imp"; remove them
  d <- d %>% select(-ends_with("_imp"))

  # Step 3.2 Optionally remove rows that still have missing values
  if (drop_incomplete) d <- na.omit(d)

  # Step 3.3 Convert text Yes/No fields to 1/0
  d$TERM_GENDER <- ifelse(d$TERM_GENDER == "F", 1, 0)
  for (v in YES_NO_COLUMNS) {
    d[[v]] <- ifelse(d[[v]] == "TRUE", 1, 0)
  }

  # Step 3.4 Treat any value other than 0 or 1 as 0 for the three
  # engagement flags
  d <- d %>%
    mutate(
      HOUSING_IND        = ifelse(HOUSING_IND %in% c(0, 1), HOUSING_IND, 0),
      FRAT_IND           = ifelse(FRAT_IND %in% c(0, 1), FRAT_IND, 0),
      CAMPUS_PROGRAM_IND = ifelse(CAMPUS_PROGRAM_IND %in% c(0, 1),
                                  CAMPUS_PROGRAM_IND, 0)
    )

  # Step 3.5 Engagement index (0 to 3): how many of these a student has
  #   lives on campus, belongs to a fraternity or sorority,
  #   joined a campus engagement program
  d <- d %>%
    mutate(INVOL_IND = case_when(
      HOUSING_IND == 1 & FRAT_IND == 1 & CAMPUS_PROGRAM_IND == 1 ~ 3,
      (HOUSING_IND == 1 & FRAT_IND == 1) |
        (HOUSING_IND == 1 & CAMPUS_PROGRAM_IND == 1) |
        (FRAT_IND == 1 & CAMPUS_PROGRAM_IND == 1) ~ 2,
      HOUSING_IND == 1 | FRAT_IND == 1 | CAMPUS_PROGRAM_IND == 1 ~ 1,
      TRUE ~ 0
    ))

  # Step 3.6 Flag whether the return outcome is already known for the term
  d <- d %>%
    mutate(
      TERM_YEAR       = as.integer(substr(TERM_CODE, 1, 4)),
      RETURN_OBSERVED = ifelse(TERM_CODE < CONFIG$first_unobserved_term, 1, 0)
    )

  # Step 3.7 Make sure the outcome is stored as 0/1 numbers
  d[[OUTCOME]] <- as.numeric(as.character(d[[OUTCOME]]))

  d
}

# Data used for the final model and most of the analysis
model_data <- prepare_data(raw_data, impute_vars = c("ACT", "HS_GPA"))

# Version used in the earlier extended comparisons: also imputes county
# and drops any remaining incomplete rows
model_data_extended <- prepare_data(raw_data,
                                    impute_vars = c("ACT", "HS_GPA", "COUNTY_CODE"),
                                    drop_incomplete = TRUE)

# Only students whose return outcome is known can be used to train and test
known_data <- model_data %>% filter(RETURN_OBSERVED == 1)

# Complete records only, used for variable screening and model comparison
# (Parts 4 and 5), where every candidate predictor must have a value
screen_data <- prepare_data(raw_data, impute_vars = c("ACT", "HS_GPA"),
                            drop_incomplete = TRUE) %>%
  filter(RETURN_OBSERVED == 1)


# -----------------------------------------------------------------------------
# Shared helper functions
# -----------------------------------------------------------------------------

# Split data into training and test sets, keeping the same return rate in both
split_data <- function(d, share = CONFIG$train_share) {
  idx <- createDataPartition(factor(d[[OUTCOME]]), p = share, list = FALSE)
  list(train = d[idx, ], test = d[-idx, ])
}

# Build a model formula such as YEAR_RETRN_IND ~ A + B + C
make_formula <- function(predictors) {
  as.formula(paste(OUTCOME, "~", paste(predictors, collapse = " + ")))
}

# Make the outcome a factor with levels "0" and "1" (needed by some models)
as_outcome_factor <- function(y) factor(as.character(y), levels = c("0", "1"))

# Cutoff from the ROC curve using pROC's "best" point (Youden index).
# Falls back to 0.5 if a cutoff cannot be found.
roc_best_cutoff <- function(prob, actual) {
  r <- roc(as.numeric(as.character(actual)), as.numeric(prob), quiet = TRUE)
  cut <- coords(r, "best", ret = "threshold", transpose = TRUE)[1]
  if (is.na(cut) || length(cut) != 1) cut <- 0.5
  cut
}

# Score predictions against actual outcomes.
#   prob       predicted chance of returning
#   actual     true outcome (0/1)
#   threshold  cutoff above which a student is predicted to return
# Returns accuracy, sensitivity, specificity, and RMSE.
# Here "positive" means the student returned (1).
evaluate <- function(prob, actual, threshold = 0.5) {
  prob <- as.numeric(prob)
  pred <- factor(ifelse(prob > threshold, "1", "0"), levels = c("0", "1"))
  cm <- confusionMatrix(pred, as_outcome_factor(actual), positive = "1")
  c(Accuracy    = unname(cm$overall["Accuracy"]),
    Sensitivity = unname(cm$byClass["Sensitivity"]),
    Specificity = unname(cm$byClass["Specificity"]),
    RMSE        = sqrt(mean((prob - as.numeric(as.character(actual)))^2)))
}


# -----------------------------------------------------------------------------
# PART 4. Screen the candidate predictors
# -----------------------------------------------------------------------------

# Step 4.1 Correlation between numeric columns
numeric_cols <- screen_data[, sapply(screen_data, is.numeric)]
numeric_cols <- numeric_cols[, setdiff(names(numeric_cols), ID_COLUMNS)]
cor_matrix   <- cor(numeric_cols, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "blue", tl.srt = 45)

# Step 4.2 List pairs of variables that are highly correlated (above 0.7)
high_cor <- which(abs(cor_matrix) > 0.7 & abs(cor_matrix) < 1, arr.ind = TRUE)
high_cor_pairs <- data.frame(
  Variable1   = rownames(cor_matrix)[high_cor[, 1]],
  Variable2   = colnames(cor_matrix)[high_cor[, 2]],
  Correlation = cor_matrix[high_cor]
)
print(high_cor_pairs)

set.seed(CONFIG$seed)
screen_split <- split_data(screen_data)

# Step 4.3 Check for overlap between predictors (VIF above about 5 is a concern)
vif_model <- lm(make_formula(c("ACT", "HS_GPA", "AGE", "ENTRY_CREDITS",
                               "INVOL_IND", "REPORT_CREDIT_HRS", "PELL_IND",
                               "REG_TIMEDIFF_DAYS", "DROP_COUNT", "YEARAID",
                               "YEARCOA_ALL")),
                data = screen_split$train)
print(vif(vif_model))

# Step 4.4 Logistic regression with all candidates, then stepwise selection
# by AIC. Each round below drops variables that add little and refits.
logit_rounds <- list(
  round1 = PREDICTORS_CANDIDATE,
  round2 = c("HS_GPA", "ENTRY_CREDITS", "INVOL_IND", "REPORT_CREDIT_HRS",
             "REG_TIMEDIFF_DAYS", "PELL_IND", "DROP_HOURS", "YEARAID",
             "YEARCOA_ALL"),
  round3 = c("HS_GPA", "ENTRY_CREDITS", "INVOL_IND", "REPORT_CREDIT_HRS",
             "PELL_IND", "DROP_HOURS", "YEARAID", "YEARCOA_ALL"),
  round4 = PREDICTORS_FINAL
)

screening_results <- list()
for (r in names(logit_rounds)) {
  fit <- glm(make_formula(logit_rounds[[r]]), data = screen_split$train,
             family = binomial)
  stepwise <- stepAIC(fit, direction = "both", trace = FALSE)
  prob <- predict(fit, screen_split$test, type = "response")
  screening_results[[r]] <- c(Predictors = length(logit_rounds[[r]]),
                              evaluate(prob, screen_split$test[[OUTCOME]], 0.5))
  cat("\n", r, ": variables kept by stepwise AIC:\n")
  print(names(coef(stepwise))[-1])
}
print(do.call(rbind, screening_results))


# -----------------------------------------------------------------------------
# PART 5. Compare models on one train/test split
# -----------------------------------------------------------------------------

# fit_and_predict() trains one model type and returns predicted
# probabilities of returning for the test set.
fit_and_predict <- function(model_type, train, test, predictors) {
  f <- make_formula(predictors)

  if (model_type == "logistic") {
    m <- glm(f, data = train, family = binomial)
    return(predict(m, test, type = "response"))
  }

  if (model_type == "decision_tree") {
    train[[OUTCOME]] <- as_outcome_factor(train[[OUTCOME]])
    m <- rpart(f, data = train, method = "class")
    return(predict(m, test, type = "prob")[, 2])
  }

  if (model_type == "random_forest") {
    train[[OUTCOME]] <- as_outcome_factor(train[[OUTCOME]])
    m <- randomForest(f, data = train, ntree = 500, mtry = 4, importance = TRUE)
    return(predict(m, test, type = "prob")[, 2])
  }

  if (model_type == "gbm") {
    # 100 trees, depth 3, learning rate 0.01, 5 fold cross validation
    m <- gbm(f, data = train, distribution = "bernoulli", n.trees = 100,
             interaction.depth = 3, shrinkage = 0.01, cv.folds = 5,
             verbose = FALSE)
    return(predict(m, test, n.trees = 100, type = "response"))
  }

  if (model_type == "gbm_tuned") {
    # 300 trees; the best number of trees is chosen by cross validation
    m <- gbm(f, data = train, distribution = "bernoulli", n.trees = 300,
             interaction.depth = 3, shrinkage = 0.01, cv.folds = 5)
    best_trees <- gbm.perf(m, method = "cv", plot.it = FALSE)
    return(predict(m, test, n.trees = best_trees, type = "response"))
  }

  if (model_type %in% c("ridge", "lasso")) {
    # Ridge (alpha = 0) shrinks all coefficients; Lasso (alpha = 1) can
    # drop variables entirely. Penalty strength chosen by cross validation.
    alpha <- ifelse(model_type == "ridge", 0, 1)
    x_train <- as.matrix(train[, predictors])
    x_test  <- as.matrix(test[, predictors])
    m <- cv.glmnet(x_train, as_outcome_factor(train[[OUTCOME]]),
                   alpha = alpha, family = "binomial")
    return(as.numeric(predict(m, s = m$lambda.min, newx = x_test,
                              type = "response")))
  }

  if (model_type == "ensemble_average") {
    # Average of three models: Gradient Boosting, Random Forest, logistic
    p_gbm   <- fit_and_predict("gbm", train, test, predictors)
    p_rf    <- fit_and_predict("random_forest", train, test, predictors)
    p_logit <- fit_and_predict("logistic", train, test, predictors)
    return((p_gbm + p_rf + p_logit) / 3)
  }

  stop("Unknown model type: ", model_type)
}

# Stacked ensemble: a logistic "meta model" learns how to weigh the three
# base models' predictions.
fit_stacked_ensemble <- function(train, test, predictors) {
  f <- make_formula(predictors)
  train_rf <- train
  train_rf[[OUTCOME]] <- as_outcome_factor(train[[OUTCOME]])

  m_gbm   <- gbm(f, data = train, distribution = "bernoulli", n.trees = 100,
                 interaction.depth = 3, shrinkage = 0.01, cv.folds = 5,
                 verbose = FALSE)
  m_rf    <- randomForest(f, data = train_rf, ntree = 500, mtry = 4)
  m_logit <- glm(f, data = train, family = binomial)

  base_preds <- function(d) data.frame(
    gbm_pred   = predict(m_gbm, d, n.trees = 100, type = "response"),
    rf_pred    = predict(m_rf, d, type = "prob")[, 2],
    logit_pred = predict(m_logit, d, type = "response")
  )

  train_stack <- base_preds(train)
  train_stack$actual <- train[[OUTCOME]]
  meta <- glm(actual ~ gbm_pred + rf_pred + logit_pred, data = train_stack,
              family = binomial)
  predict(meta, base_preds(test), type = "response")
}

set.seed(CONFIG$seed)
compare_split <- split_data(screen_data)

comparison_models <- c("logistic", "decision_tree", "random_forest", "gbm",
                       "ridge", "lasso")
comparison <- list()
for (set_name in c("candidate", "final")) {
  preds <- if (set_name == "candidate") PREDICTORS_CANDIDATE else PREDICTORS_FINAL
  for (mt in comparison_models) {
    prob <- fit_and_predict(mt, compare_split$train, compare_split$test, preds)
    comparison[[paste(mt, set_name)]] <-
      evaluate(prob, compare_split$test[[OUTCOME]], 0.5)
  }
}
prob_stack <- fit_stacked_ensemble(compare_split$train, compare_split$test,
                                   PREDICTORS_FINAL)
comparison[["stacked_ensemble final"]] <-
  evaluate(prob_stack, compare_split$test[[OUTCOME]], 0.5)

comparison_table <- data.frame(Model = names(comparison),
                               do.call(rbind, comparison), row.names = NULL)
print(comparison_table)
write_xlsx(comparison_table,
           file.path(CONFIG$output_dir, "model_comparison_single_split.xlsx"))


# -----------------------------------------------------------------------------
# PART 6. Resampling study: test each model many times
# -----------------------------------------------------------------------------
# Each iteration draws a random sample of students (with replacement),
# splits it 70/30, trains the model, and records accuracy, sensitivity,
# specificity, and RMSE on the 30%. Averages across iterations show how
# stable each model is.
#
# Note: because sampling is with replacement, the same student record can
# appear in both parts of one iteration. The final model in Part 7 is
# checked on a separate test set of unique records.

run_resampling <- function(model_type, data, predictors, iterations,
                           sample_size, cutoff = c("fixed", "roc")) {
  cutoff <- match.arg(cutoff)
  results <- matrix(NA, nrow = iterations, ncol = 4,
                    dimnames = list(NULL, c("Accuracy", "Sensitivity",
                                            "Specificity", "RMSE")))
  for (i in seq_len(iterations)) {
    sample_rows <- data[sample(nrow(data), sample_size, replace = TRUE), ]
    s <- split_data(sample_rows)
    prob <- fit_and_predict(model_type, s$train, s$test, predictors)
    thr <- if (cutoff == "roc") roc_best_cutoff(prob, s$test[[OUTCOME]]) else 0.5
    results[i, ] <- evaluate(prob, s$test[[OUTCOME]], thr)
  }
  colMeans(results, na.rm = TRUE)
}

# Settings used in the original resampling runs.
#   iterations  = number of repeated samples
#   sample_size = students drawn in each sample
# These runs are slow (some take hours). To test the script quickly,
# set QUICK_TEST to TRUE, which runs 3 iterations of 1,000 students each.
QUICK_TEST <- CONFIG$use_demo_data

resampling_plan <- data.frame(
  model       = c("random_forest", "random_forest", "gbm_tuned", "ridge",
                  "lasso", "ensemble_average", "ensemble_average"),
  predictors  = c("extended", "final", "final", "final",
                  "final", "extended", "extended"),
  cutoff      = c("fixed", "roc", "roc", "roc", "roc", "fixed", "fixed"),
  iterations  = c(500, 500, 1000, 1000, 500, 1000, 500),
  sample_size = c(30000, 10000, 30000, 30000, 10000, 10000, 20000),
  stringsAsFactors = FALSE
)

resampling_results <- list()
for (k in seq_len(nrow(resampling_plan))) {
  p <- resampling_plan[k, ]
  preds <- if (p$predictors == "final") PREDICTORS_FINAL else PREDICTORS_EXTENDED
  d     <- if (p$predictors == "final") known_data else
             filter(model_data_extended, RETURN_OBSERVED == 1)
  iters <- if (QUICK_TEST) 3 else p$iterations
  n     <- if (QUICK_TEST) 1000 else p$sample_size

  set.seed(CONFIG$seed)
  avg <- run_resampling(p$model, d, preds, iters, n, p$cutoff)
  resampling_results[[k]] <- data.frame(p, t(avg))
  print(resampling_results[[k]])
}
resampling_table <- do.call(rbind, resampling_results)
write_xlsx(resampling_table,
           file.path(CONFIG$output_dir, "resampling_study_results.xlsx"))


# -----------------------------------------------------------------------------
# PART 7. Final model: train, choose a cutoff, and score all students
# -----------------------------------------------------------------------------

# Step 7.1 Split students with known outcomes into training (70%) and test (30%)
set.seed(CONFIG$seed)
final_split <- split_data(known_data)
train_data <- final_split$train
test_data  <- final_split$test
train_data[[OUTCOME]] <- as_outcome_factor(train_data[[OUTCOME]])
test_data[[OUTCOME]]  <- as_outcome_factor(test_data[[OUTCOME]])

# Step 7.2 Train the Random Forest on the six final predictors
#   500 trees; 4 predictors tried at each split
rf_model <- randomForest(make_formula(PREDICTORS_FINAL), data = train_data,
                         ntree = 500, mtry = 4, importance = TRUE)

# Step 7.3 Predicted chance of returning for each test student
test_data$Prob_Score <- predict(rf_model, test_data, type = "prob")[, 2]

# Step 7.4 Choose the cutoff: the point on the ROC curve closest to a
# perfect model (top left corner), balancing sensitivity and specificity
roc_curve <- roc(response = test_data[[OUTCOME]], predictor = test_data$Prob_Score,
                 quiet = TRUE)
roc_df <- data.frame(threshold   = roc_curve$thresholds,
                     sensitivity = roc_curve$sensitivities,
                     specificity = roc_curve$specificities)
roc_df$distance <- sqrt((1 - roc_df$specificity)^2 + (1 - roc_df$sensitivity)^2)
optimal_threshold <- roc_df$threshold[which.min(roc_df$distance)]

# Step 7.5 Measure performance on the test set only
final_metrics <- evaluate(test_data$Prob_Score, test_data[[OUTCOME]],
                          optimal_threshold)
metrics_df <- data.frame(
  Metric = c("Cutoff", "AUC", names(final_metrics),
             "Training records", "Test records"),
  Value  = c(optimal_threshold, as.numeric(auc(roc_curve)), final_metrics,
             nrow(train_data), nrow(test_data))
)
print(metrics_df)

# Step 7.6 Which predictors matter most to the model
importance_df <- data.frame(Variable = rownames(importance(rf_model)),
                            importance(rf_model), row.names = NULL)
print(importance_df)

# Step 7.7 Score every student, including the current term whose outcome
# is not yet known. Students below the cutoff are flagged for outreach.
model_data$Prob_Score      <- predict(rf_model, model_data, type = "prob")[, 2]
model_data$Predicted_Class <- factor(ifelse(model_data$Prob_Score > optimal_threshold,
                                            "1", "0"), levels = c("0", "1"))
model_data$Outreach_Flag   <- ifelse(model_data$Predicted_Class == "0", 1, 0)

# Step 7.8 Save results
# Summary results (no student data): safe to share
write_xlsx(list(ModelMetrics      = metrics_df,
                FeatureImportance = importance_df),
           file.path(CONFIG$output_dir, "final_model_summary.xlsx"))

# Student level scores: FERPA protected. Save only to an approved location.
student_scores <- model_data %>%
  select(all_of(ID_COLUMNS), TERM_CODE, Prob_Score, Predicted_Class, Outreach_Flag)
write_xlsx(student_scores,
           file.path(CONFIG$output_dir, "student_risk_scores_CONFIDENTIAL.xlsx"))

# Step 7.9 Plot the ROC curve for the test set
plot(roc_curve, main = "ROC Curve (Test Set)", col = "red")

cat("Done. Summary results and student scores saved in:", CONFIG$output_dir, "\n")
