# From-Genes-to-Diagnosis-Classifying-Brain-Cancer-Subtypes-with-R-and-Machine-Learning

This project uses gene expression data from the CuMiDa database (GSE50161) to build ML models for classifying brain cancer subtypes. It involves data cleaning, dimensionality reduction (e.g., PCA), and model training (e.g., SVM, RF, Logistic Regression).

## 📥 Dataset Download

Due to GitHub’s file size limits, the dataset is not included in this repository.

## 🔗 Dataset Source
- [Kaggle Link](https://www.kaggle.com/datasets/brunogrisci/brain-cancer-gene-expression-cumida)

**After downloading:**
- Create a folder named `data/` in the project root (if it doesn't already exist).
- Move the downloaded `Brain_GSE50161.csv` into the `data/` directory.

## 📦 Setup Instructions

Before running any scripts, install all required R packages by executing the following command in your R console:

```r
source("scripts/00_install_packages.R")


## 📁 Project Structure
```plaintext
From Genes to Diagnosis: Classifying Brain Cancer Subtypes with R and Machine Learning/
├── data/                                   # Contains raw and cleaned dataset files
│   └── Brain_GSE50161.csv                  # Main dataset used in the project
├── scripts/                                # All R scripts for each pipeline stage
│   ├── 01_data_cleaning.R                  # Script for loading and cleaning the dataset
│   ├── 02_exploration_visualization.R      # EDA and plots to understand data distribution
│   ├── 03_dimensionality_reduction.R       # PCA, LASSO, or t-SNE for feature reduction
│   ├── 04_model_training_evaluation.R      # Builds and evaluates classification models
│   └── 05_interpretation_visualization.R   # Gene importance, visualization, clinical insight
├── output/                                 # Results from model runs and visualizations
│   ├── graphs/                             # All plots (PCA, decision boundaries, etc.)
│   └── models/                             # Saved models and metrics
├── report/                                 # Final technical report in R Markdown
│   └── final_report.Rmd                    # Reproducible report with code and results
├── presentation/                           # Presentation slides for stakeholders
│   └── executive_summary_presentation.pptx # 10–15 min summary in plain language
├── README.md                               # Project overview and setup instructions
├── .gitignore                              # Specifies files/folders to exclude from Git
└── requirements.txt                        # (Optional) List of R packages required
```
