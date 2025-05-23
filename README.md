# Bellabeat Case Study: Smart Device Usage Analysis

This case study analyzes smart device data from Bellabeat - a high-tech wellness company - using tools like R, Tableau, and RMarkdown.
The goal is to generate insights that can help Bellabeat make data-driven decisions for future product marketing strategies.

---

## **Introduction**

Bellabeat is a wellness technology company that manufactures health-focused smart products primarily for women. This case study explores Fitbit usage data from 30 users to help Bellabeat understand user behavior and improve marketing strategies.

---

## **Dataset**

The dataset was originally generated by a survey conducted by **Mobius** and made publicly available on **Kaggle** by **Arash Nik**.

- Dataset Source: [Fitbit Fitness Tracker Data](https://www.kaggle.com/arashnic/fitbit)
- Duration: March 12, 2016 – May 12, 2016
- Sample Size: 30 Fitbit users

---

## **Objective**

Analyze smart device usage data to uncover trends in:

- Physical activity (steps, distance, calories)
- Sleep behavior (duration, efficiency)
- Heart rate patterns
- Weekly and daily usage habits

These insights will help Bellabeat better understand its users and inform its product development and marketing efforts.

---

## **Files in this Repository**
**Key files included in this project:**
- `Bellabeat.R` → Contains R code for data cleaning, merging, and preprocessing
- `Bellabeat_Analysis.Rmd` → Full RMarkdown report with code, insights, and visualizations
- `Bellabeat_Analysis.pdf` → Final rendered report (easier to view)
- `cleaned_combined_data.csv` → Final cleaned dataset used for Tableau visualizations
- `Activity Overview.png` → Dashboard 1: User Activity Trends
- `Sleep Analysis.png` → Dashboard 2: Sleep Behavior Patterns
- `Heart Rate and Comparison.png` → Dashboard 3: Heart Rate & Lifestyle Insights

---

## **Tools & Technologies Used**

- **R** – Data cleaning, transformation, and visualization
- **Tidyverse**, **lubridate**, **janitor** – R packages used
- **RMarkdown** – Report generation
- **Tableau** – Interactive dashboards and visualizations
- **GitHub** – Project sharing and version control

---

## **Data Cleaning & Preparation**

- Merged relevant CSV files: `dailyActivity`, `sleepDay`, and `heartrate_seconds`
- Converted date and time columns using `lubridate`
- Aggregated heart rate data to daily level
- Handled missing values appropriately
- Exported cleaned data to CSV for Tableau visualization

---

## **Visualizations**

A total of **12 visualizations** were created to analyze activity, sleep, and heart rate data.

### R Visuals:
- Steps Over Time (Line)
- Calories vs Steps (Scatter)
- Sleep Duration Distribution (Histogram)
- Avg Steps by Weekday (Bar)
- Sleep vs Time in Bed (Scatter)
- Heart Rate by Weekday (Bar)
- Boxplot of Steps by Weekday
- Dual Axis Line: Steps & Sleep

### Tableau Visuals:
- Interactive dashboards with filters
- Scatter plots, bar charts, line graphs
- Box plots and histograms

---

## **Key Insights**

- **Activity**: Users are most active mid-week (Tuesday to Thursday), with step counts peaking on Wednesdays.
- **Sleep**: Average sleep duration is ~6.9 hours. Users generally underutilize recommended sleep time.
- **Heart Rate**: Elevated average heart rates correlate with lower activity levels in some users.
- **Efficiency**: There’s a notable gap between time in bed vs. actual time asleep, indicating sleep efficiency issues.

---

## **Dashboards**

Interactive dashboards were created in Tableau to present the findings:

- **[Dashboard 1: Activity Overview](https://public.tableau.com/views/BellabeatAnalysis_17449580469280/ActivityOverview?:language=en-GB&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link)**  
- **[Dashboard 2: Sleep Analysis](https://public.tableau.com/views/BellabeatAnalysis_17449580469280/SleepAnalysis?:language=en-GB&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link)**
- **[Dashboard 3: Heart Rate and Comparison](https://public.tableau.com/views/BellabeatAnalysis_17449580469280/HeartRateandComparison?:language=en-GB&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link)**

---

## **Conclusion**

The analysis helped uncover user behavior patterns that Bellabeat can leverage to:

- Design more personalized wellness plans
- Push targeted fitness and sleep reminders
- Improve app engagement by syncing insights with smart devices

---

## **Acknowledgements**

- Fitbit Dataset made publicly available by [Kaggle - Arash Nik](https://www.kaggle.com/arashnic)
- Case Study inspired by Google Data Analytics Capstone Project
