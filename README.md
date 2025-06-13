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
```

## 📁 Project Structure
```plaintext
From-Genes-to-Diagnosis-Classifying-Brain-Cancer-Subtypes-with-R-and-Machine-Learning/
│
├── .gitignore                      # Git ignore rules
├── .RData                          # R session data (auto-generated)
├── .Rhistory                       # R command history (auto-generated)
│
├── LICENSE                         # Licensing info
├── README.md                       # Project overview and setup guide
├── requirements.txt               # List of required R packages
│
├── data/                           # Original and preprocessed datasets
│   └── original_data.csv           # Raw or imported dataset (130 samples)
│
├── output/                         # Outputs from preprocessing, models, and results
│   ├── cleaned_data.csv            # Processed version of the dataset
│   ├── dim_reduction/              # PCA, t-SNE, etc. outputs
│   ├── graphs/                     # Visualization outputs
│   └── model/                      # Trained models and results
│       ├── graphs/                 # Training or performance graphs
│       └── metrics/                # Accuracy, confusion matrix, F1, etc.
│
├── scripts/                        # ⚙️ Data processing, modeling, and evaluation scripts
│   ├── 00_download_data.R          # Optional: if dataset is fetched online
│   ├── 00_install_packages.R       # Installs all required R packages
│   ├── 01_data_cleaning.R          # Cleans raw data
│   ├── 002_exploration_visualization.R
│   ├── 02_exploration_visualization.R
│   ├── 03_dimensionality_reduction.R
│   ├── 04_compare_models.R         # Compare performance across models
│   ├── 04_eval_rf.R                # Random Forest evaluation
│   ├── 04_eval_SVM.R               # SVM evaluation
│   ├── 04_eval_NB.R                # Naive Bayes evaluation
│   ├── 04_eval_xgb.R               # XGBoost evaluation
│   ├── 04_eval_nn.R                # Neural Net evaluation
│   └── 05_model_training_evaluation.R  # Final training + evaluation logic
│
├── presentation/                   # Slide decks or visual summaries for stakeholders
│
├── report/                         # Final reports, PDFs, academic papers
│
├── secrets/                        # Credentials or secure files (should be .gitignored)
│
├── shiny_app/                      # Shiny web app for end-user prediction
│   ├── app.R                       # Main Shiny file (UI + server logic)
│   ├── global.R                    # Shared setup: model load, theme, libraries
│
│   ├── modules/                    # Modular code for cleaner app logic
│   │   └── mod_prediction.R        # Contains UI + server for input & prediction output
│
│   ├── model/                      # Final trained model saved as .rds
│   │   └── brain_model.rds         # Used by the Shiny app to make predictions
│
│   └── www/                        # Static assets for UI
│       ├── styles.css              # Custom styles (e.g., background, colors)
│       └── background.jpg          # Background image for modern look
│
└── From–Genes–to–Diagnosis–Classifying–Bra...  # (Temp file, safe to delete or rename)
```
