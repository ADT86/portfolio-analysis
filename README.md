## Industry Portfolios Analysis

### Description:
This repository contains an analysis of 17 industry portfolios. The goal is to compute statistics like average return, volatility, and momentum, and to construct various portfolio strategies based on these statistics. The analysis delves into the relationships between returns and volatility and employs momentum-based ranking to create investment strategies.

### Contents:
- **Data Loading and Preprocessing**:
  - Data from 1960 loaded and formatted.
  - Date formatting for clear time series analysis.
   
- **Statistical Analysis**:
  - Computation of average returns, log returns, and volatilities for each industry.
  - Correlation analysis between returns/log returns and volatility.
   
- **Visualizations**:
  - Scatter plots displaying the relationship between average log returns and volatility.
  - Time series plots showcasing cumulative log returns for different portfolio strategies.
   
- **Portfolio Strategies**:
  - Equal-weighted portfolio construction.
  - Risk-adjusted momentum-based portfolio strategies.
  - Computation of Sharpe ratios for the portfolios.
   
- **Dependencies**:
  - R (Ensure you have the latest version installed).
  - Libraries: matrixStats, matrixcalc, zoo.

### Setup:
1. **Clone the Repository**:

2. **Data**:
Ensure `industries17modified.csv` is in the root directory or adjust the path in the script.

3. **Run the Analysis**: 
Use your preferred R environment to run the analysis script.

### Contributing:
Pull requests are welcome. For major changes, please open an issue first to discuss what you'd like to change.


# Result summary table:
<img width="553" alt="image" src="https://github.com/ADT86/portfolio-analysis/assets/102257932/8741bccd-5f9a-4992-84c6-c096b9f8ce76">
