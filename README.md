# Hotel Booking Cancellation Analysis 🏨
*Predictive Modeling to Optimize Revenue and Operations*

## 📊 Project Dashboard
![Final Dashboard](hotel_dashboard_final.png)

## 🎯 The Problem
High cancellation rates disrupt hotel operations, leading to lost revenue and inefficient staffing. This project identifies key **"behavioral anchors"** to predict guest check-ins versus cancellations, enabling data-driven overbooking and deposit strategies.

## 💡 Key Business Insights
* **The Power of Commitment:** Guests requesting parking are **79% less likely** to cancel.
* **Engagement as Glue:** Each special request acts as "psychological glue," reducing cancellation odds by **68%**.
* **Lead Time Risk:** Risk increases by **1.6%** per day of lead time, suggesting the need for stricter deposit policies for long-term bookings.

## 🛠️ Technical Achievements
* **Accuracy:** 80.6%
* **Balanced Strategy:** Tuned to a **0.42 threshold** to maximize the **F1-Score (0.71)**. This optimization balances the cost of "False Alarms" against the need to identify high-risk cancellations.
* **Statistical Integrity:** Resolved **Singularity Errors** by diagnosing and removing perfectly collinear variables (like `total_stay`), ensuring the mathematical stability of the Logistic Regression model.
* **Stack:** R (tidyverse, patchwork, caret, kableExtra, ROCR)
