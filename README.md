# ML-Titanic

In this project I followed a tutorial for the infamous Titanic/kaggle competition.

I learnt the following:
- How to inspect data using various functions such as head(), sum(), is.na(), str()
- How to use visualisations to explore the relationship between DV (survival) and IVs (such as Age, Sex etc)
- How to use functions such as cut2 to manipulate data into a format that can be visualised
- How to carry out statistical tests and statistical measures e.g. logistic regression and correlation 
- Feature engineering and how to classify data with a simple logistic regression model

- Data mining techniques e.g... 
* using grep() to search for matches for certain elements in strings
* replace unique name prefix with standard form e.g. Mrs as opposed to Mme. Ms. Lady. 
* creating new tables from specific variables
* replacing missing values with mean age

I used the following packages:
- RColorBrewer = provides colour palettes which enhance visualisations
- ggplot2 = to create sophisticated visualisations
- haven = to experiment with converting character variables to factors
- gridExtra = provides grid graphics for visualisations
- Hmisc = to cut a numeric variable into intervals 

I generated the following visualisations:
- barplots
- mosaic plots
- boxplots
- density plot
- linear regression


Contains 3 folders:

Data - 'train.csv', 'test.csv'
Code - 'ML-Titanic' R script
Output - 'titanic.csv'
