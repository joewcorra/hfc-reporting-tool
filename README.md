# HFC Emissions Reporting Tool

> **Status: Beta** — Active development. Intended for internal review and 
> collaborator testing. Not yet recommended for official reporting use.

A Shiny application for estimating hydrofluorocarbon (HFC) emissions from 
consumption data for use in national greenhouse gas inventory reporting under 
the IPCC methodology framework.

---

## Overview

This tool implements the IPCC Tier 1 consumption-based approach for estimating 
HFC emissions. Users enter Kigali Amendment consumption data (imports, exports, 
production, destruction) for individual HFC substances and the tool calculates 
emissions estimates suitable for national GHG inventory reporting.

Designed for use by inventory compilers working under the AIM Act and Montreal 
Protocol/Kigali Amendment reporting obligations.

---

## Features

- Enter HFC consumption data by substance and year
- Automatic emissions estimation using IPCC consumption-based methodology
- Handles net consumption logic (sign conventions for imports/exports/destruction)
- Output formatted for IPCC inventory reporting
- [Additional features — add as applicable]

---

## Requirements

- R (>= 4.1)
- The following R packages:
  - `shiny`
  - `tidyverse`
  - `[add others as needed]`

---

## Installation & Local Use

Clone the repository and run locally in R:
```r
# Install required packages if needed
install.packages(c("shiny", "tidyverse"))

# Run the app
shiny::runApp()
```

> **Hosted version:** A hosted version of this app is forthcoming. 
> See [Issues](#) for status updates.

---

## Usage

1. Launch the app
2. Enter HFC substance consumption data for the reporting year
3. Review calculated emissions estimates
4. Export results for use in inventory submissions

---

## Status & Roadmap

This tool is under active development. Current priorities:

- [ ] Finalize emissions calculation methodology
- [ ] Add data validation and input checks
- [ ] Identify and configure a hosting solution
- [ ] Complete internal review before broader release

---

## Contributing

This is currently a private repository for EPA collaborators. If you have 
access and wish to contribute, please open an issue or submit a pull request.

---

## Contact

Joe Corra

---

