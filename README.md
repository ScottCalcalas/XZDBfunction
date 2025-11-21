
# XZDBfunction


## Installation at R

```r
remotes::install_github("scottcalcalas/XZDBfunction")
```

## Use Genomic website browser

```r
library(XZDBfunction)
XZDB.Run()
```

---

## **Functions Contents**
  - [1. Print/Export](#function-1-printexport)
  - [2. Check](#function-2-check)
  - [3. Load Index File](#function-3-load-index-file)
  - [4. Output Files](#function-4-output-files)
  - [5. Update Database](#function-5-update-database)
  - [6. UniProt / MINT Comparison](#function-6-uniprot--mint-comparison)

---

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

---

## Function 2 — Check
Use the **Check** tab to:
- See which datasets include a specific gene  
- Quickly confirm if the gene exists in the database  

---

## Function 3 — Load Index File
On the left panel, use **Load index file** to view dataset metadata and structure.

---

## Function 4 — Output Files
Open the **Output files** tab to:
- Preview a result file (using the red *Preview Selected* button)  
- Download results (using the *Download* button in each row)

**Note:**  
Not all outputs can be previewed.  
The output folder may be cleared monthly—please save results you want to keep.

---

## Function 5 — Update Database
*(Optional — only needed if updating dataset metadata)*

After updating:
- `datasets/`
- `Datasets infomation.xlsx`

Run these actions in the left panel:
1. **Build index (all)**  
2. **Build searching_GeneID.csv**  
3. **Build searching_geneSymbol.csv**

---

## Function 6 — UniProt / MINT Comparison
- **UniProt:** retrieves protein entry information  
- **MINT:** retrieves protein–protein interaction data based on UniProt ID  

Requires an internet connection.





---
---
# Local use protocol
## If you want to use it locally, instead of a package
**Author:** Xiaopei  
**Updated:** 2025-11-20  

A user-friendly Shiny application for searching, annotating, and exporting genomic and proteomic metadata across multiple datasets.  
This README provides complete instructions for starting the application and using all its functions.

### **Contents**
- [A. Auto Start](#a-auto-start)
  - [On Windows](#on-windows)
  - [On Mac](#on-mac)
- [B. Manual Start](#b-manual-start)
  - [Step 0 — Return Users](#step-0-return-users)
  - [Step 1 — Download / Open Folder](#step-1-download--open-folder)
  - [Step 2 — Open Quick Start.R](#step-2-open-quick-startr)
  - [Step 3 — Run the Script](#step-3-run-the-script)
- [Start & Package Installation](#start--package-installation)
- [Last Step](#last-step)
- [Appendix — Windows & Mac Shortcuts](#appendix)
---

# A. Auto Start

## On Windows
A desktop shortcut can automatically launch the Genomic DB Browser, as long as **R is installed**.

## On Mac
1. Ensure **R** is installed.  
2. Open the project folder.  
3. Double-click **MAC_Start.command**.

*If the Mac start script fails, use the backup version inside the `App_Info` folder.*

---

# B. Manual Start

### Step 0 — Return Users
Close all running R sessions, including RStudio and R terminal windows.

### Step 1 — Download / Open Folder
Download or open the entire Genomic DB Browser folder on your computer.

### Step 2 — Open `Quick Start.R`
Open the script using **R** or **RStudio**.

### Step 3 — Run the Script
Running **one line** from `Quick Start.R` will launch the browser interface.

---

# Start & Package Installation
On the first run, the script will automatically install any missing R packages.

Once installation finishes, the app will open in your browser automatically.


---

# Last Step
After finishing:
- Close the browser tab  
- Close R / RStudio / Terminal


---

# Appendix

## Windows Shortcut Example
Modify the shortcut **Target** to point to Rscript and your Quick Start script: (For example:)
```
C:\Users\jcc1885\AppData\Local\Programs\R\R-4.4.3\bin\Rscript.exe "R:\Basic_Sciences\Pharm\Borden_Lab\borden\Database\Genomic result Browser\Quick Start.R"
```

## Mac “One-Click Start” Command File
Create a file named `something.command` on the Desktop:

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

After those steps, Double-click the file can run it.



