# Otomoto Data Analysis Report

## Overview
This project involves analyzing a dataset from the Otomoto website, focusing on various car features and characteristics. The data is sourced from `otomoto_web2020.csv`, and the analysis is performed using R programming language. The report details the tasks undertaken to process the dataset and visualize the results.

## Requirements
- R version 4.0 or higher
- R packages: `tidyverse`, `stringi`, `ggplot2`

## Installation
To install the required packages, run the necessary commands in your R console.

## Data Import
The dataset is imported using the `read_csv()` function from the `tidyverse` package. The imported data frame is named `df_auto`.

## Tasks
The analysis consists of several key tasks, each outlined below:

### Task 1: Rename Variables
Rename the variables in the `df_auto` data frame according to a specified mapping. 

### Task 2: Process Variables
Ensure the variables `mileage`, `engine_capacity`, and `horse_power` are of the correct numeric type.

### Task 3: Transform Price Variable
Split the `price` variable into two columns: `priceValue` (numeric) and `priceCurrency` (character), converting `priceValue` to PLN.

### Task 4: Create Color ASCII Column
Convert the `color` column to lowercase, remove Polish diacritics, and recode it to English names.

### Task 5: Extract Unique Offer ID
Create a new column `NoOffer` to store unique offer identification numbers extracted from the `url` column.

### Task 6: Feature Processing
Process the `features` column to create a new data frame with binary indicators for each feature, and incorporate this into the `df_auto` dataset.

## Visualization
Utilize the `ggplot2` package to visualize the results of the analysis, focusing on the features present in the dataset.

## Conclusion
This analysis provides insights into the characteristics of cars listed on the Otomoto platform. Through data processing and visualization, we can better understand the market offerings and their features.
