# Prediction_on_Flight_Delay

- The goal of this project is to predict the departure delays (dep_delay) for flights. The dataset being used is from the nycflights13 packages. 

- The original dataset have exactly 200,000 rows of data with 43 columns joined from flights, weather, airports, and planes packages. Some of the columns, especially the ones from the planes packages have significant amount of data missing. 

- Linear Regression, Stepwise Regression by AIC, Generalized Additive Model, Ridge and Lasso Regression are fitted in to the data. GAM seems to be the best model based on its MSE.
