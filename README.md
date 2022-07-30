# RFM-Analysis
RFM (Recency, Frequency & Monetary) analysis: “is a behavior based technique used to segment customers by examining their transaction history.”

->It is based on the marketing axiom that 80% of your business comes from 20% of your customers. ->The 80-20 rule maintains that 80% of outcomes (outputs) come from 20% of causes (inputs). In the 80-20 rule, you prioritize the 20% of factors that will produce the best results. A principle of the 80-20 rule is to identify an entity's best assets and use them efficiently to create maximum value.

Key RFM Metrics:

1.Recency (How recently a customer has purchased?)

2.Frequency (How often do they purchase?)

3.Monetary (How much the customer spends?)

Performing RFM ANALYSIS in R language: 1- Load the libraries in R studio: library(tidyverse) library(Hmisc) library(janitor) library(plotly)

2- Load dataset in R in the environment section. You can do this by first setting your working directory in R (where dataset is stored ) than using csv function to read. A short cut to add your current working directory id by pressing: Shift+Ctrl+H

3- To view your dataset : view(sales_data) The first thing to do with your data is to understand it, by looking at this dataset what comes in mind? No idea! Don't worry. It's a sales data of a store. The rows/observations in this dataset, is the combination of an InvoiceNo and a StockCode , but wait , analysis of this dataset would be hectic, so first we would do a bit cleaning by summarizing the distinct InvoiceNo to a unique one as it has spread our data out.

RFM ANALYSIS Steps:

1 - As mentioned above , first step is to collect data from the database of your client that, is showed above in the from of collected dataset.

2 - Second step is to calculate RFM Metrics which have already been defined above. In R we use the code that is provided in the repository to calculate these metrics.

Functions used in code and their description:

mutate: to add new column. subtotal: to calculate purchase amount by invoice ( not a function but another column) strptime: R by default treat every character as string , strptime function is used to tell R to treat InoiceDate column as numeric also by specifying format. group_by : is used to view other variables according to specific one. In our case, as RFM is customer based analysis we group by Customer_ID summarize: to summarize data , we've calculated recency as days_since_last_purchase, frequency as n_transactions and monetary value as total_revenue. max: is used to calculate last_purchase_date difftime: is used to calculate recency as days_since_last_purchase by putting in date_today(analysis date) , last_purchase_date and units = "days" as, recency is defined: How recently a customer has purchased? The above steps then stored as df_rfm_1

3 - Next comes RFM Scoring, which is done in two steps:

-> Assigning bins to customers or make RFM quintiles , Why we do this? let's say in the total revenue section , the customers revenue distribution would be like: someone spent $10, another one $50, $100, $250, $500, $700, and so on.. So, it wouldn't be wise or even impractical to analyze each and every one besides we bin them, so that every bin/quintile would be the representative of chunk of customers fall within specific monetary value and can be treated accordingly, same goes for other two metrics.

In R we did it using cut2 function and the results is stored as df_rfm_2

-> Assign scores to these bins, which is simply like marking test scores of a class in the school, the customer with rfm score of etc would be this(segment name) and with rfm score of etc would be that (segment name). In R we did it using as.integer function and dense_rank for recency metric and stored the result as df_rfm_3.

4 - Now comes the final step where we define segments and segment customers based upon their RFM score.

A segment must contain following details:

the name of the segment the definition of the segment the intervals for the recency, frequency & monetary scores

In R, we use ifelse condition to label the customers by defined segment and result is stored as df_rfm_4 and then finaly visulize it using ggplot.
