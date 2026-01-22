
# 'XZDBfunction' R Package: Genomic Database Website Browser
- A user-friendly database website browser application for searching, annotating, and exporting genomic and proteomic metadata across multiple datasets.
- Supports gene expressing & protein interaction database. 
- The only dependency is R ; works on Windows, macOS, and Linux. Easy to launch.
- Also contains daily analysis functions for statistical and biological analysis.


| Overview | Start Page |
|---|---|
| ![Shiny UI Overview](man/figures/xzdbGraph_Explain.png) | ![Shiny UI FirstPage](man/figures/FirstPage.png) |

This document provides complete instructions for starting the website browser application and its functions.

## Quick Tour 
(*Contains setup code for R, launching the website browser directly with an example dataset*) 
```r
install.packages("remotes") # Skip this step if you already have "remotes" package
remotes::install_github("scottcalcalas/XZDBfunction")
library(XZDBfunction)

XZDB.Run() #Launch a demo - with example datasets
```

## Setup Database 
(*Step-by-step setup instructions for R, and how to launch the database browser directly with example/your dataset*) 

### 1. Get protocol and set up your own database
```r
?xzdb.help()
xzdb.help() #Get protocols and setup instructions

# Transfer your datasets to the current folder, check everything is OK to run next step.
xiaopei.input.all()    #Build database in current folder and copy them to R package.
# After this step, R always uses your own dataset instead of the example dataset
```

([See details](#setup-a-new-database))

### 2. Launch the genomic website browser in two different ways

```r
XZDB.Run()                 # launches browser; if you already ran xiaopei.input.all(), it will use your own dataset instead of the example dataset

XZDB.Run(use_current = T)  # Force running on current path. Requires setup datasets.
```

---


## Browser Functions
 
  - [1. Search](#function-1-Search)
  - [2. Check](#function-2-check)
  - [3. Load Data information](#function-3-data-information)
  - [4. Output Files](#function-4-output-files)
  - [5. Update Database](#function-5-update-database)
  - [6. UniProt / MINT Comparison](#function-6-uniprotmint-comparison)
  - [Setup a New Database](#setup-a-new-database)


### Function 1. Search
Use the **Search** tab to obtain detailed search results.

Supported search options:
- Gene Symbol — *exact*, *family*, *fuzzy*
- Protein ID — *exact match*
- ENSEMBL Gene ID — *exact match*

Example: searching **CDK13** and **NCBP2** returns all matching rows across all datasets.

The app generates three files automatically:
- Full results  
- Details for Gene 1  
- Details for Gene 2  

![Shiny UI function1](man/figures/function1.png)

Each line represents a row founded in our current database, with that database’s description at the beginning.

<br><br>

### Function 2. Check
Use the **Check** tab to:
- See which datasets include a specific gene  
- Confirm if the gene exists in the database  

![Shiny UI function2](man/figures/function2.png)


<br><br>

### Function 3. Data information
On the **Data info** tab, use **"Load Data info"** button to view dataset metadata and structure.

![Shiny UI function3](man/figures/function3.png)


<br><br>

### Function 4. Output Files
Open the **Output files** tab to:
- Preview a result file (using the red *Preview Selected* button)  
- Download results (using the *Download* button in each row)

**Note:**  
Not all outputs can be previewed.  
The output folder may be cleared monthly—please save results you want to keep.

![Shiny UI function4](man/figures/function4.png)


<br><br>

### Function 5. Update Database
*(Optional — only needed if updating dataset metadata)*

After updating:
- `datasets/`
- `Datasets information.xlsx`

Run "Rebuild EVERYTHING" at Administrator Operations in the left panel:

![Shiny UI function5](man/figures/function5.png)

<br><br>

### Function 6. UniProt/MINT Comparison
- **UniProt:** retrieves protein entry information  
- **MINT:** retrieves protein–protein interaction data based on UniProt ID  

Requires an internet connection.

**If search from current result:**
![Shiny UI function6.1](man/figures/function61.png)

<br>

**If type the ID:**
![Shiny UI function6.2](man/figures/function62.png)

<br>

**The summary table and the Venn diagram will be like:**
![Shiny UI function6.3](man/figures/function63.png)

<br><br>

## Setup a New Database

Use helper to create your own dataset, run:
```r
xzdb.help()
xiaopei.input.all()        # Run this after modifying the copied files
```

Synchronize all datasets and index files(include Datasets information.xlsx) into the package shinyapp directly
```r
xiaopei.sync.to.shinyapp() #No need to run if you already run xiaopei.input.all()
```

Get the current using datasets inside the package location (If you want to confirm it's copied succuessfully)
```r
xzdb.nowDataset()          #It copies current using datasets to your working path
```

Example package storage location, inside R package:
```r
 R\R-4.4.3\library\XZDBfunction\shinyapp
```
After those steps, start the browser and run "Rebuild EVERYTHING" at Administrator Operations.



---




# Local usage protocol

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

To make a quick start R file, put these two lines:
```r
library(XZDBfunction)
XZDB.Run()
```

For example, it can be called Quick_Start.R. The next step is just setting up to run this file using R (not RStudio).


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

After those steps, double-click the file to launch the website browser application.

#### A3. Update local R script (For Borden Lab at Northwestern University)

Copy and replace your local scripts using the scripts at github location: XZDBfunction/inst/shinyapp/
