# Retention Risk Model

An open, documented framework that helps colleges and universities identify, early in a term, the students most likely not to return the following year, so advisors know whom to contact first.

## What it does

For each first time, full time student enrolled at the fall census, the model estimates the probability that the student returns the following year. Scores are updated after the spring census. Students below a chosen cutoff are flagged for advisor outreach.

The model is a Random Forest classifier that uses six pieces of information colleges already keep:

| Variable | Meaning |
|---|---|
| `INVOL_IND` | Engagement index, 0 to 3: lives on campus, fraternity or sorority member, joined a campus engagement program |
| `REPORT_CREDIT_HRS` | Credit hours enrolled in the term |
| `PELL_IND` | Pell Grant recipient (1 = yes) |
| `DROP_HOURS` | Credit hours dropped during the term |
| `YEARAID` | Total financial aid for the year |
| `YEARCOA_ALL` | Cost of attendance for the year |

The model does not use gender, race or ethnicity, citizenship, or first generation status as predictors.

## What it is not

The model does not replace advising judgment, and it does not decide who receives support. It ranks students so that limited advising time goes first to those most likely to leave.

## Who it is for

Institutional research offices of any size, including one person offices. It runs in R, which is free. No commercial software is needed.

## How to use it

1. Install R and these packages: `readxl`, `writexl`, `dplyr`, `VIM`, `caret`, `randomForest`, `gbm`, `glmnet`, `pROC`, `MASS`, `car`, `rpart`, `corrplot`.
2. **Try it first with made-up data.** Open `retention_model.R`, set `use_demo_data = TRUE` in the `CONFIG` section, and run the script. No student records are needed.
3. **Run it on your own data.** Build a student by term extract with the columns described in the script, set `data_file` and `first_unobserved_term` in `CONFIG`, and run the script. It prepares the data, trains the model on your students, chooses a cutoff, and scores current students.

## What the script includes

| Part | Step |
|---|---|
| 1 to 3 | Load data, check missing values, fill in missing ACT and GPA (kNN), build the engagement index |
| 4 | Screen predictors: correlation, VIF, stepwise logistic regression |
| 5 | Compare logistic regression, decision tree, Random Forest, Gradient Boosting, Ridge, Lasso, and ensembles |
| 6 | Resampling study: retrain each model many times to check stability |
| 7 | Final model: train, choose a cutoff from the ROC curve, score all students |

## Data privacy

This repository contains **no student data**. The script writes two outputs:

* `final_model_summary.xlsx`: metrics and variable importance only; safe to share.
* `student_risk_scores_CONFIDENTIAL.xlsx`: individual scores; protected under FERPA. Store it only in a location your institution approves.

## Background

Developed by Emmanuel Assefuah Ocran, Senior Research Analyst in Institutional Research. A full technical report describes the data, methods, validation, and results.

## License

Released under the MIT License. See `LICENSE`. You may use and adapt this work freely with attribution.
