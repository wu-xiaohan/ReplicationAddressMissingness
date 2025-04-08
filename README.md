# Replication Files for Addressing Missingness in Serialized Bureaucratic Data: The Case of Chinese Courts

## Overview
This repository contains replication files for the paper **"Addressing Missingness in Serialized Bureaucratic Data: The Case of Chinese Courts"** by Xiaohan Wu, Margaret E. Roberts, Rachel E. Stern, Benjamin L. Liebman, Amarnath Gupta, Luke Sanford, published in Sociological Methods & Research in 2025. The files provided allow users to replicate the results presented in the paper. Due to GitHub's file size limitations, the replication data is shared via google drive. Please download the zip file using google cloud link, unzip and replace the data folder in this repository. The complete dataset for missingness estimation is available for download at the following link:https://drive.google.com/drive/folders/10m0h5PhBVUiJn8N01rCU4N9dbDQ8TZGx?usp=sharing.

## Repository Structure
The repository is organized as follows:

```
├── data/                # Cleaned datasets containing case ids and registartion date from Chinese court decisions
│   ├── court13-17/      # Case id and registration date from civil, criminal and administrative litigation cases filed from 2013 to 2017
│   ├── court18-22/      # Case id from civil, criminal and administrative first instance litigation cases filed from 2018 to 2022
│   ├── validation/      # data for validation plots
│   ├── courts.csv       # manually collected information from over 3500 courts in China
│   ├── workday.csv      # calendar day and workday convertion, a day is labeled as "holiday" if it is weekend or a public holiday in China
├── code/                # Scripts for estimating court missingness and time to decision
│   ├── court13-17/      # Scripts for estimating court missingness and time to decision for year 2013-2017
│   ├── court18-22/      # Scripts for estimating case filing date
│   ├── plots/           # Scripts for generating figures
├── results/          # Output files (tables, figures, etc.)
├── README.md         # This file
└── LICENSE           # License information
``` 

## Data
- **Source:** The case id and registration date are collected and cleaned by authors. The original case decisions are publicly available on China Judgements Online(https://wenshu.court.gov.cn/). 

## Running the Code
To replicate the results, follow these steps:

1. Clone the repository
2. Navigate to the repository directory
3. Download data from google drive and replace the data folder
4. Install required dependencies
5. Read the notes at the top in each scripts carefully
6. Run scripts from the code folder

## Contact
For any questions or issues, please contact:
- **Xiaohan Wu:** xiaohanwuresearch@gmail.com
- **GitHub Issues:** Open an issue [here](https://github.com/wu-xiaohan/ReplicationAddressMissingness/issues).

## License
This project is licensed under the **[License Name]** - see the [LICENSE](LICENSE) file for details.

---
**Citation:** If you use these replication files in your work, please cite:

> [Author(s)]. ([Year]). *[Paper Title]*. [Journal/Conference], [Volume]([Issue]), [Page Numbers]. [DOI/Link]

