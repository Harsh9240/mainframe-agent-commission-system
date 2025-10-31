Agent Commission & Loan Report Automation System

📘 Overview

This project automates the process of calculating agent commissions and generating monthly loan reports for a banking client.
Before this automation, the team handled everything manually — commission calculations were done using spreadsheets and separate SQL queries, which often caused data mismatches and delays.
The goal of this project was to bring everything together on the mainframe, making it faster, accurate, and fully automated using COBOL, JCL, DB2, and VSAM.

🎯 Objective

Calculate agent commissions automatically based on the number of active policies and their status.
Generate formatted DB2 reports for loans approved in a given month.
Eliminate manual work and improve data reliability.

⚙️ How It Works

1. Main COBOL Program – CA11G115

Reads input data from a sequential file (TI001-PS).
Validates agent information like policy count, status, and commission rate.
Calls the subprogram CA21G115 to calculate total commission.
Writes the processed data into a VSAM KSDS file (TO001-KS).

2. Subprogram – CA21G115

Performs the actual commission calculation:
Total Commission = Policy Count × Commission Rate

Returns the computed value back to the main program.

3. JCL Jobs
   
JCL Name	Purpose
COMP	Compiles and links COBOL programs into the load library
JA11G115	Creates a VSAM KSDS cluster using IDCAMS
JA21G115	Sorts input policy data before processing
RUN	Runs the main COBOL job end-to-end

4. DB2 Report Program – REPORT / REPORT1

Connects to DB2 tables APPLICANT_DB and LOAN_DB.
Fetches all loan approvals for applicants from Chennai in November.
Generates a detailed report with headers, date/time, and page numbers.
Handles errors gracefully using SQLCA and DSNTIAR.

🧰 Tech Stack

COBOL, JCL, DB2, VSAM (KSDS), IDCAMS, SORT, z/OS


📈 Results

Reduced manual processing effort by around 80%.
Improved accuracy in agent commission payouts.
Delivered faster and consistent monthly loan reports.
Enabled easier audit tracking with standardized report formats.

🧠 What I Learned

Writing modular COBOL programs and handling file I/O efficiently.
Building end-to-end JCL workflows for compilation, sorting, and execution.
Integrating COBOL with DB2 for dynamic report generation.
Implementing structured error handling in production-like environments.
