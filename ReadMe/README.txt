# SDHSA Milestone 3 – Group 13

## Structure
- `artefacts/` – saved Random Forest model and preprocessing recipe from Task 2
- `app/app.R` – Shiny deployment interface (Task 4)
- `reports/` – Milestone 3 report PDFs for Tasks 1–4
- `monitoring/monitoring_log.csv` – scoring log
- `ethics_ai_log.md` – AI and ethics record
- `R/` – preparation, modelling and evaluation scripts

## How to run

1. Open RStudio in the repository root.
2. Set the working directory to `app/`.
3. Run:
   ```r
   shiny::runApp()
