# Replication Files for Addressing Missingness in Serialized Bureaucratic Data: The Case of Chinese Courts

## Overview
This repository contains replication files for the paper **"Addressing Missingness in Serialized Bureaucratic Data: The Case of Chinese Courts"** by Xiaohan Wu, Margaret E. Roberts, Rachel E. Stern, Benjamin L. Liebman, Amarnath Gupta, Luke Sanford, published in Sociological Methods & Research in 2025. The files provided allow users to replicate the results presented in the paper. Due to GitHub's file size limitations, this repository includes a smaller subset of the data for testing and demonstration purposes. The complete dataset for missingness estimation is available for download at the following link:

## Repository Structure
The repository is organized as follows:

```
├── data/                # Cleaned datasets containing case ids and registartion date from Chinese court decisions
│   ├── court13-17/      # Case id and registration date from civil, criminal and administrative litigation cases filed from 2013 to 2017
│   ├── court18-22/      # Case id from civil, criminal and administrative first instance litigation cases filed from 2018 to 2022
├── code/                # Scripts for estimating court missingness and time to decision
│   ├── transparency/    # Scripts for estimating court missingness
│   ├── time-to-decision/# Scripts for estimating case filing date
│   ├── plots/           # Scripts for generating figures
│   ├── tables/          # Scripts for generating tables
├── results/          # Output files (tables, figures, logs, etc.)
├── README.md         # This file
└── LICENSE           # License information
```

## Requirements
To run the code and replicate the results, you will need the following software and packages:

- **Programming Language:** R
- **Required Packages:** 
  - `ggplot`
  - `lmtest` 

## Data
- **Source:** The case id and registration data is collected and processed by authors. The original case decisions are publicly available on China Judgements Online(https://wenshu.court.gov.cn/). 

## Running the Code
To replicate the results, follow these steps:

1. Clone the repository:
   ```sh
   git clone https://github.com/[username]/[repository-name].git
   ```
2. Navigate to the repository directory:
   ```sh
   cd [repository-name]
   ```
3. Install required dependencies:
   ```sh
   [Command to install dependencies, e.g., `pip install -r requirements.txt`]
   ```
4. Run the main script:
   ```sh
   [Command to run the script, e.g., `python main.py` or `stata -b do analysis.do`]
   ```
5. The results will be saved in the `results/` directory.

## Expected Output
- **Tables:** Replication of tables presented in the paper.
- **Figures:** Graphs and charts illustrating key results.
- **Logs:** Any logs or messages generated during execution.

## Contact
For any questions or issues, please contact:
- **Xiaohan Wu:** xiaohanwuresearch@gmail.com
- **GitHub Issues:** Open an issue [here](https://github.com/wu-xiaohan/ReplicationAddressMissingness/issues).

## License
This project is licensed under the **[License Name]** - see the [LICENSE](LICENSE) file for details.

---
**Citation:** If you use these replication files in your work, please cite:

> [Author(s)]. ([Year]). *[Paper Title]*. [Journal/Conference], [Volume]([Issue]), [Page Numbers]. [DOI/Link]

