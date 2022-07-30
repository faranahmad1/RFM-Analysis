# load libraries ####
library(tidyverse)
library(Hmisc)
library(janitor)
library(plotly)
library(ggplot2)
library(tidyr)

# load data and date(date of analysis) ####

date_today <- lubridate::dmy("30/12/2011")
sales_data <- read.csv("storeData.csv")

# calculating RFM metrics ####

sales_data |> 
  dplyr::mutate(subtotal=Quantity*UnitPrice,
                invoicedate= strptime(InvoiceDate,format("%m/%d/%Y %H:%M"))) |> 
  dplyr::group_by(CustomerID) |> 
  dplyr::summarise(total_revenue=sum(subtotal),
                   n_transactions=dplyr::n_distinct(InvoiceNo),
                   last_purchase_date= max(invoicedate)) |>
  dplyr::mutate(days_since_last_purchase=
                  difftime(date_today,last_purchase_date, units = "days")) |> 
  dplyr::mutate(days_since_last_purchase= as.integer(days_since_last_purchase)) |> 
  janitor::clean_names() |>
  drop_na() |> 
  filter(total_revenue > 0) -> rfm_1

#eighty-twenty-principle

plotly::ggplotly(
  ggplot(rfm_1 |> 
           filter(total_revenue <50000), aes(x= total_revenue)) +
    geom_histogram(binwidth = 200)
)
# quintiles ####

rfm_1 |> 
  mutate(monetary_quintile = cut2(total_revenue, g =5),
         frequency_quintile = cut2(n_transactions, g = 5),
         recency_quintile = cut2(days_since_last_purchase, g = 5)) -> rfm_2

levels(rfm_2$recency_quintile)

# assigning scores to quintiles ####

rfm_2 |> 
  mutate(recency_score = as.integer(recency_quintile),
         frequency_score = as.integer(frequency_quintile),
         monetary_score = as.integer(monetary_quintile)) |> 
  mutate(recency_score = dense_rank(desc(recency_score))) -> rfm_3

# assigning segments/labels ####
rfm_3 |>
  dplyr::mutate(RFM = recency_score * 100 + frequency_score * 10 + monetary_score) |> 
  dplyr::mutate(labels = ifelse(recency_score >= 4 & frequency_score >= 3 & monetary_score >= 4, "Champions",
                                ifelse((recency_score >= 4) & (frequency_score <= 2) & (monetary_score >= 4), "High Spending New Customers",
                                       ifelse((recency_score >= 4) & (frequency_score >= 4) & (monetary_score == 3), "Average Spending Champions",
                                              ifelse((recency_score >= 2 & recency_score <= 4) & (frequency_score >= 3 & frequency_score <= 5) & (monetary_score >= 4), "Loyal Customers", 
                                                     ifelse((recency_score >= 3) & (frequency_score >= 1 & frequency_score <= 3) & (monetary_score >= 1 & monetary_score <= 3), "Potential Loyalists",
                                                            ifelse((recency_score >= 4 & recency_score <= 5) & (frequency_score < 2) & (monetary_score < 2), "New Customers",
                                                                   ifelse((recency_score >= 3 & recency_score <= 4) & (frequency_score < 2) & (monetary_score < 2), "Promising",
                                                                          ifelse((recency_score >= 3 & recency_score <= 4) & (frequency_score >= 2 & frequency_score <= 4) & (monetary_score >= 3 & monetary_score <= 5), "Need attention",
                                                                                 ifelse((recency_score >= 2 & recency_score <= 3) & (frequency_score < 3) & (monetary_score < 3), "About to sleep",
                                                                                        ifelse((recency_score < 3) & (frequency_score >=2 & frequency_score <= 5) & (monetary_score >= 2 & monetary_score <= 5), "At risk",
                                                                                               ifelse((recency_score < 2) & (frequency_score >= 4 & frequency_score <= 5) & (monetary_score >= 4 & monetary_score <= 5), "Can't loose them",
                                                                                                      ifelse((recency_score >= 2 & recency_score <=3) & (frequency_score >= 2 & frequency_score <= 3) & (monetary_score >= 2 & monetary_score <= 3), "Hibernating",
                                                                                                             ifelse((recency_score <= 2) & (frequency_score <= 2) & (monetary_score >= 4), "High Value Lost",
                                                                                                                    ifelse((recency_score < 2) & (frequency_score <= 3) & (monetary_score <= 2), "Low Value Lost",
                                                                                                                           ifelse((recency_score == 3) & (frequency_score < 2) & (monetary_score >= 4), "High Spending New Customers",
                                                                                                                                  ifelse((recency_score <= 2) & (frequency_score < 2) & (monetary_score == 3), "Average Spending Lost",
                                                                                                                                         ifelse((recency_score <= 2) &(frequency_score <= 4) &(monetary_score == 1), "Low Value Hibernating",
                                                                                                                                                ifelse((recency_score <= 3) &(frequency_score >= 4) &(monetary_score <=3), "Average Spending Need Attention", "Low Spending Champions"))))))))))))))))))) -> rfm_result
plotly::ggplotly(
  ggplot(rfm_result, aes(x = labels, fill= labels)) +
    geom_bar() +
    theme(axis.text.x = element_text(angle = 90))
)
write.csv(rfm_result, file = "rfm_analysis.csv") 
