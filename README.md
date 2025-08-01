# Analyzing ICWA Caselaw with CourtListener’s API in R

This repository shows how to pull Indian Child Welfare Act (ICWA) caselaw metadata from the Free Law Project’s [CourtListener](https://www.courtlistener.com/) API and analyze it in R.  

It includes:
- Querying the CourtListener Search API  
- Creating a metadata table of case information  
- Downloading opinion text by court  
- Example charts: cases filed by year and cases per court  

Read the write-up on Medium: [How to analyze caselaw using CourtListener’s API in R](https://bigdataforsocialjustice.medium.com/how-to-analyze-caselaw-using-courtlisteners-api-in-r-cb88ef28d00a)

---

## Requirements

Install these R packages:

```r
install.packages(c(
  "httr2","purrr","dplyr","tibble","tidyr",
  "stringr","ggplot2","lubridate","forcats",
  "readr","digest"
))

