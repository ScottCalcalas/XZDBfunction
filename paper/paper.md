---
title: 'XZDBfunction: An Open-Source Platform for Integrative Exploration and Discovery of Genomic and Protein Interaction Networks'
tags:
  - R
  - Shiny
  - genomics
  - proteomics
  - protein interaction
  - research database
authors:
  - name: Xiaopei Zhang
    orcid: 0009-0002-7980-8653
    corresponding: true
    affiliation: 1
  - name: Caleb Embree
    affiliation: 1
affiliations:
  - name: Department of Pharmacology, Northwestern University Feinberg School of Medicine, United States
    index: 1
date: 27 April 2026
bibliography: paper.bib
---

# Summary

`XZDBfunction` is an R package and Shiny application for building a searchable
browser over genomic, proteomic, and protein interaction datasets. A user can add
CSV, TXT, or XLSX files together with a simple metadata spreadsheet, rebuild the
database index, search by gene symbol or identifier, inspect dataset-level
metadata, and export result tables for downstream analysis. The browser supports
exact, family, and fuzzy gene-symbol search modes, gene presence checks across
datasets, and comparison of local findings with UniProt and MINT protein
interaction information [@uniprot2025; @mint2012].

The package is intended for research groups that need to compare laboratory
results with previous experiments or published resources but do not want every
query to require custom code. For example, a bench scientist can use the Shiny
interface to ask whether a gene or protein appears in any indexed dataset, view
the supporting rows, and export the matching evidence. A more technical user can
use the R functions directly to rebuild indexes, update datasets, and manage
local or packaged deployments.

# Statement of need

Modern molecular biology projects often generate many spreadsheets and tabular
files that are useful beyond the analysis in which they were first created.
These files may contain gene identifiers, gene symbols, proteomics measurements,
or interaction evidence, but they are usually stored with different column names,
sheet structures, and metadata conventions. Comparing a new laboratory result
with this collection can require repeated manual searching or ad hoc scripts.
Those approaches are slow, difficult to reproduce, and hard for collaborators
without coding experience to audit.

`XZDBfunction` addresses this need by turning a folder of heterogeneous datasets
and a metadata workbook into a local, searchable research database. The metadata
workbook records which columns contain gene identifiers and symbols, while the R
package builds normalized index files and the Shiny application presents them in
a point-and-click browser. This design lets researchers integrate newly produced
data with previously published or externally curated data and then compare
findings across sources without reimplementing the same import and search logic.

The package is especially useful in collaborative laboratory settings where one
person may curate and update the database while many others search it. It lowers
the barrier for non-programming users, preserves exportable search results, and
keeps the database rebuild process reproducible through explicit R functions.

# State of the field

Several established tools support interactive biological data exploration or
network analysis. Cytoscape is a widely used environment for biological network
integration and visualization [@cytoscape2003]. The iSEE package provides a
general Shiny-based interface for exploring data stored in Bioconductor
`SummarizedExperiment` objects [@isee2018], and ShinyCell provides shareable
single-cell expression browsers [@shinycell2021]. Curated resources such as
UniProt and MINT provide high-quality protein annotation and protein-protein
interaction records [@uniprot2025; @mint2012].

These tools solve important parts of the broader problem, but they do not target
the specific workflow that motivated `XZDBfunction`: maintaining a lab-scale
search browser over many simple files, including both local experiments and
published comparison datasets, with minimal setup for non-programming users.
`XZDBfunction` is not intended to replace Cytoscape for network modeling or
Bioconductor tools for structured assay analysis. Instead, it fills an upstream
curation and triage role: researchers can quickly identify which datasets contain
a gene or protein of interest, recover the original rows, and compare local
hits with interaction evidence before deciding whether a more specialized
analysis environment is needed.

# Software design

`XZDBfunction` is implemented as an R package built around a small set of core
indexing functions and a bundled Shiny application [@R; @shiny]. The main data
model separates raw datasets, dataset metadata, derived index files, and exported
search results. Users can call `xzdb.help()` to copy example files and protocol
materials, `xzdb.input.all()` to build indexes for all listed datasets,
`xzdb.sync.to.shinyapp()` to synchronize the indexed database into the packaged
application, and `XZDB.Run()` to launch the browser.

The central design trade-off is to prefer portable files and spreadsheet-based
configuration over a database server. This keeps installation and sharing simple
for small research groups, but it also means that the package is best suited to
local or departmental deployments rather than high-concurrency public web
services. The Shiny interface exposes common operations as tabs: search, check,
dataset information, output files, database update, and UniProt/MINT comparison.
The R functions remain available for users who want scripted rebuilds or local
customization.

During indexing, the package reads the dataset metadata, detects file type,
extracts the configured gene identifier and gene-symbol columns, normalizes
blank-like values, fills missing identifier-symbol pairs where possible, and
writes per-dataset index files. Search results preserve links back to the
original dataset rows, so exported evidence can be checked against the source
table instead of becoming a detached summary.

# Research impact statement

`XZDBfunction` was developed to support molecular pharmacology research workflows
where laboratory findings must be compared with previous experiments and
interaction databases. Its immediate impact is practical: it allows collaborators
without coding experience to perform cross-dataset searches, inspect source
evidence, and export comparison tables using a local browser. The bundled example
database and protocol materials provide reproducible reference material for
reviewers and new users.

The package also has credible near-term scholarly significance because it
encodes a reusable pattern common to many biomedical groups: converting scattered
analysis outputs into a searchable evidence browser that can combine current
laboratory datasets with published data. This supports more transparent
comparison of new findings against existing evidence and reduces the amount of
one-off code required for each query. Future releases can strengthen this impact
case by documenting external adoption, adding more public example datasets, and
archiving tagged releases with a software DOI.


# Acknowledgements

We thank members of the Borden Lab for feedback on laboratory data exploration
workflows and for motivating the need for an accessible comparison browser.

# References
