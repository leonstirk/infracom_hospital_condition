# Hospital Building Condition and Seismic Vulnerability – Interview Task

This repository contains the analysis and report prepared for an interview exercise with the New Zealand Infrastructure Commission (Infracom).  
The task was to assess the condition of the national hospital estate and provide a view on where investment should be directed, using a dataset of hospital buildings supplied by Infracom.

---

## Project Overview

The analysis is framed as a **sandbox exercise** based solely on the dataset provided. No supplementary data sources were incorporated. The work focuses on:

1. **Condition and seismic vulnerability** of hospital buildings.
2. **Prioritisation of investment need** using a transparent, non-weighted Pareto banding method.
3. **Identification of data gaps** that limit confidence in prioritisation and represent investment needs in their own right.

---

## Data

The dataset covers hospital buildings across New Zealand with variables including:

- **Condition**: numeric score and categorical grade.
- **Seismic performance**: % New Building Standard (%NBS), graded NBS, Seismic Importance Level (IL1–IL4), Seismic Zone.
- **Gross Floor Area (GFA)**: as a proxy for building scale.
- **Other descriptors**: region, DHB, campus, building name, service role.

Data cleaning steps included handling placeholders (`#N/A`, `Nil`, `Not NMH`, etc.), coercing variables to numeric or ordered factors, and identifying missing values.

---

## Methods

- **Descriptive profiling**: summary statistics and visualisations of condition, seismic ratings, and vulnerabilities of IL3/IL4 (high importance) buildings.
- **Pareto banding**: non-dominated sorting across three seismic variables (graded NBS, IL, seismic zone) to rank buildings into bands of combined need.  
  - Band 1 = assets with the highest joint vulnerability.  
  - Subsequent bands reflect progressively lower combined need.
- **Data gaps**: highlighted missing %NBS and/or condition data for 48 IL4 buildings.

All analysis was performed in **R** using `tidyverse`, `huxtable`, `ggplot2`, and `emoa`.

---

## Outputs

- **Report (`report.pdf`)**: a two-page briefing note with tables and figures.
- **R Markdown (`report.Rmd`)**: source file for the report.
- **Data cleaning and analysis scripts**: contained in the `scripts/` folder.
- **Figures and tables**: produced in-line from the `.Rmd`.

---

## Key Findings

- Some IL4 assets combine poor condition with low seismic strength, representing high investment priority.
- Pareto banding provides a transparent, replicable way to prioritise without arbitrary weighting.
- Missing seismic and condition data for critical buildings is itself a major risk; targeted assessments are recommended.

---

## Structure

|-- data/ # Clean dataset
|-- scripts/ # Data cleaning (from raw xlsx/csv) and analysis scripts
|-- report.Rmd # R Markdown source for the report
|-- report.pdf # Final report output
`-- README.md # This file

---

## Disclaimer

This analysis was prepared solely for the purposes of the interview exercise. It is illustrative, limited to the provided dataset, and not intended to represent a complete investment prioritisation framework.
