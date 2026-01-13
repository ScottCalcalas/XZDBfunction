
# 'XZDBfunction' R Package
- A user-friendly database website browser application for searching, annotating, and exporting genomic and proteomic metadata across multiple datasets.
- Only dependency is R (Shiny UI), work for Windows, Mac, and linux.
- Easy lunch script available (both Windows and MacOS system).
- Also contains daily analysis functions for statistics and biology.


This document provides complete instructions for starting the website browser application and its functions.

## Quick Tour 
(*Contains setup code for R, lunching the website browser directly with an example dataset*) 
```r
install.packages("remotes")
remotes::install_github("scottcalcalas/XZDBfunction")
library(XZDBfunction)
XZDB.Run() #Run a demo - with example datasets
```

## Quick Use 
(*Step by step setup instruction for R, and how to lunch the database browser directly with example/yours dataset*) 

### 1.Installation at R

```r
install.packages("remotes") # Skip this step if you already have "remotes" package

remotes::install_github("scottcalcalas/XZDBfunction")

library(XZDBfunction)
```



### 2.Get protocol and Set up your own database
```r
?xzdb.help()
xzdb.help() #Get protocols and setup instructions

#transfer your datasets to current folder, check everything ok to run next step.
xiaopei.input.all()    #Build database in current folder and copy them to R package.
#After this step, R always use your own dataset instead of example dataset
```

### 3.Use Genomic website browser

```r
XZDB.Run()                 # lunches browser, if you already finished running xiaopei.input.all(), it will use your own dataset instead of example dataset

XZDB.Run(use_current = T)  # Force running on current path. Requires setup datasets.
```

---

 # Contents (*for database website browser functions*)
  - [1. Print/Export](#function-1-printexport)
  - [2. Check](#function-2-check)
  - [3. Load Index File](#function-3-load-index-file)
  - [4. Output Files](#function-4-output-files)
  - [5. Update Database](#function-5-update-database)
  - [6. UniProt / MINT Comparison](#function-6-uniprot--mint-comparison)
  - [Browser Files: Add Your Own Dataset & Dataset Information](#browser-files)

## Function 1 — Print/Export
Use the **Print/Export** tab to obtain detailed search results.

Supported search options:
- Gene Symbol — *exact*, *family*, *fuzzy*
- Protein ID — *exact match*
- ENSEMBL Gene ID — *exact match*

Example: searching **CDK13** and **NCBP2** returns all matching rows across all datasets.

The app generates three files automatically:
- Full results  
- Details for Gene 1  
- Details for Gene 2  

## Function 2 — Check
Use the **Check** tab to:
- See which datasets include a specific gene  
- Quickly confirm if the gene exists in the database  


## Function 3 — Load Index File
On the left panel, use **Load Data info** to view dataset metadata and structure.


## Function 4 — Output Files
Open the **Output files** tab to:
- Preview a result file (using the red *Preview Selected* button)  
- Download results (using the *Download* button in each row)

**Note:**  
Not all outputs can be previewed.  
The output folder may be cleared monthly—please save results you want to keep.

## Function 5 — Update Database
*(Optional — only needed if updating dataset metadata)*

After updating:
- `datasets/`
- `Datasets infomation.xlsx`

Run "Rebuild EVERYTHING" at Administrator Operations in the left panel:


## Function 6 — UniProt / MINT Comparison
- **UniProt:** retrieves protein entry information  
- **MINT:** retrieves protein–protein interaction data based on UniProt ID  

Requires an internet connection.



# Browser files

To use your own dataset and dataset information(saving to package storage or run on current path), use:
```r
xiaopei.input.all()
```

Use helper to create your own dataset, run:
```r
xzdb.help()
xiaopei.input.all()    # Run this after modified copied files
```

Example package storage location, inside R package:
```r
 R\R-4.4.3\library\XZDBfunction\shinyapp
```
You can find Datasets infomation.xlsx ; For putting dataset, goes to dataset folder
After done those, start broswer can run "Rebuild EVERYTHING" at Administrator Operations

---

# Local use protocol

(If you want to use it as one click, instead of open R)

**Author:** Xiaopei  
**Updated:** 2025-11-20  


### **Contents**
- [0. Configure R file](#0-configure-r-file)
- [1. Auto Start](#1-auto-start)
  - [On Windows](#on-windows)
  - [On Mac](#on-mac)
- [2.Start & Package Installation](#2start--package-installation)
- [3.Last Step](#3last-step)
- [Appendix — Windows & Mac Shortcuts](#appendix)



## 0. Configure R file

To make an quick start R file, put this two lines:
```r
library(XZDBfunction)
XZDB.Run()
```

For example, it can be called Quick_Start.R. The next step is just setting up to run this file using R software(Not R-studio).


## 1. Auto Start

*(Check examples at Appendix)*

*On Windows*: 

A desktop shortcut can automatically launch the Genomic DB Browser, as long as **R is installed**.



*On Mac*:

1. Ensure **R** is installed.  
2. Open the project folder.  
3. Double-click **MAC_Start.command**.
*If the Mac start script fails, use the backup version inside the `App_Info` folder.*


## 2.Start & Package Installation
On the first run, the script will automatically install any missing R packages.

Once installation finishes, the app will open in your browser automatically.



## 3.Last Step
After finishing:
- Close the browser tab  
- Close R / RStudio / Terminal



## *Appendix*

#### A1.Windows Shortcut Example
Modify the shortcut **Target** to point to Rscript and your Quick Start script: (For example:)
```
C:\Users\jcc1885\AppData\Local\Programs\R\R-4.4.3\bin\Rscript.exe "R:\Basic_Sciences\Pharm\Borden_Lab\borden\Database\Genomic result Browser\Quick Start.R"
```


#### A2.Mac “One-Click Start” Command File
Create a file named `something.command` (e.g. `MAC_Start.command`) on the Desktop:

```bash
#!/bin/bash
# Launch script
# ...
```
Then run
```bash
cd ~/Desktop
chmod +x something.command
```

After those steps, Double-click the file can luanch the website browser application.

#### A3. Update local R script (For Borden Lab at Northwestern University)

Copy and replace your local scripts using the scripts at github location: XZDBfunction/inst/shinyapp/



