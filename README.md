Problem Statement
Predicting the price of a used car involves numerous variables, including the car's age, mileage, brand, and more. With new car prices on the rise, consumers increasingly turn to used cars, necessitating a reliable prediction model for used car prices to assist buyers and sellers.

Methodology
Data Collection and Preprocessing:

Utilized a dataset from Kaggle with 4340 records and 8 attributes, including car brand, model, year, mileage, fuel type, seller type, transmission, and number of owners.
Conducted data cleaning to remove duplicates and handle missing values, resulting in 3577 records.
Data Distribution Analysis:

Analyzed the distribution of key variables, identifying trends such as the most common car brands and fuel types, and average car mileage.
Data Transformation:

Encoded categorical variables for regression analysis and dropped highly unique columns that could hinder model performance.
Machine Learning Models:

Developed and evaluated three models: Simple Linear Regression, Regression Tree, and a Regression Tree with Bootstrap Aggregation (Bagging).
Performance Metrics:
Simple Linear Regression: R² = 53.5%, RMSE = 280,359.7
Regression Tree: R² = 59.5%, RMSE = 261,760.8
Bagged Regression Tree: R² = 67.5%, RMSE = 95,239.01
Key Findings
Top Predictors: Car brand, year, and mileage were the most significant predictors of selling price across all models.
Model Performance: The Bagged Regression Tree model significantly outperformed the others, providing more accurate price predictions.
Conclusion
This project demonstrates the effective use of machine learning to predict used car prices, offering valuable insights for consumers and businesses. By identifying key price determinants and employing advanced regression techniques, we provide a robust tool for navigating the used car market.
