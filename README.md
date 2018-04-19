# Springboard-Capstone

# Problem

Trying to predict in which country a new user on Airbnb, will make his or her first booking. There are 11 potential countries along with a 12th class - NDF (No Destination Found), indicating the user did not make any booking.

New users on Airbnb can book a place to stay in 34,000+ cities across 190+ countries. By accurately predicting where a new user will book their first travel experience, Airbnb can share more personalized content with their community, decrease the average time to first booking, and better forecast demand.

URL -> https://www.kaggle.com/c/airbnb-recruiting-new-user-bookings

# Data

A list of users along with their demographics, web session records, and some summary statistics are provided by Airbnb. The challenge is to predict which country a new user's first booking destination will be. All the users in this dataset are from the USA.
URL -> https://www.kaggle.com/c/airbnb-recruiting-new-user-bookings/data

There are 12 possible outcomes of the destination country: 'US', 'FR', 'CA', 'GB', 'ES', 'IT', 'PT', 'NL','DE', 'AU', 'NDF' (no destination found), and 'other'. Destination 'NDF' is different from 'other' because 'other' means there was a booking, but is to a country not included in the list, while 'NDF' means there was no booking. 

The data consists of user characteristics like language, age, browser, date-of-account-creation, OS, etc. for the train and test users. There is data on the actions taken by users on the website along with the details of the action and duration.

# Approach

The following steps highlight the strategy to be adopted for carrying out the analysis for the Capstone project:
1.	Data Wrangling and Cleaning
o	Deal with missing values: Explore the data to determine if missing values should be discarded or hold significance
o	Dropping columns that are irrelevant to the analysis
o	Rearrange and transform dataset for cleaner analysis.
2.	Exploratory Data Analysis
o	Perform Regression Analysis to determine factors that most influence the outcome of booking a particular destination
o	Identify patterns and correlation between the different variables
o	Use data visualization for graphical analysis to identify trends and answer questions on the dataset
4.	Feature Engineering
o	The success of all Machine Learning algorithms depends on how the data is presented. The features in the data directly influences the predictive models used and the results achieved.
o	Identify great features that describe the structures inherent in the data and significantly affect the result
o	Transform data from a raw state to a state suitable for modelling
5.	Predictive Analysis
o	Our machine learning approach will be supervised classification machine learning because the datasets are designed for this method since we have access to a training set with the correct outcomes and a test set without outcomes. 
o	The algorithm that we are going to use have to be fast and not use up a lot of memory since we have a very large amount of data to process. 
o	We also want multi classification which means that we want to basically find out how probable it is that the user wants to travel to all destinations and not just which destination has the highest probability.

