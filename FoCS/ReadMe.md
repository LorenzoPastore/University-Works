<p float="left">
 <img src="https://github.com/LorenzoPastore/FoCS_2019-20/blob/master/images/DS%20Logo.png" width = "500"/>
 <img src="https://github.com/LorenzoPastore/FoCS_2019-20/blob/master/images/Bicocca%20Logo.png" width = "100" align="right"/>
</p>

# Foundation of Computer Science

The final project consists in 12 task to develop on jupyter notebook working on the [Kiva dataset](https://www.kaggle.com/kiva/data-science-for-good-kiva-crowdfunding), which is available on the Kaggle web page.

- Normalize the loan_lenders table. In the normalized table, each row must have one loan_id and one lender.
- For each loan, add a column duration corresponding to the number of days between the disburse time and the planned expiration time. If any of those two dates is missing, also the duration must be missing.
- Find the lenders that have funded at least twice.
- For each country, compute how many loans have involved that country as borrowers.
- For each country, compute the overall amount of money borrowed.
- Like the previous point, but expressed as a percentage of the overall amount lent.
- Like the three previous points, but split for each year (with respect to disburse time).
- For each lender, compute the overall amount of money lent. For each loan that has more than one lender, you must assume that all lenders contributed the same amount.
- For each country, compute the difference between the overall amount of money lent and the overall amount of money borrowed. Since the country of the lender is often unknown, you can assume that the true distribution among the countries is the same as the one computed from the rows where the country is known.
- Which country has the highest ratio between the difference computed at the previous point and the population?
- Which country has the highest ratio between the difference computed at point 9 and the population that is not below the poverty line?
- For each year, compute the total amount of loans. Each loan that has planned expiration time and disburse time in different years must have its amount distributed proportionally to the number of days in each year. For example, a loan with disburse time December 1st, 2016, planned expiration time January 30th 2018, and amount 5000USD has an amount of 5000USD 31 / (31+365+30) = 363.85 for 2016, 5000USD 365 / (31+365+30) = 4284.04 for 2017, and 5000USD * 30 / (31+365+30) = 352.11 for 2018.


For any doubt and/or perplexity, do not hesitate to contact:
- Lorenzo Pastore - l.pastore6@campus.unimib.it
