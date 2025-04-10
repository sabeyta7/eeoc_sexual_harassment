# Sexual Harassment Data Analysis Project

## Project Overview
This project analyzes sexual harassment charge data in relation to labor statistics, industry-specific employment data, and unionization rates. The analysis integrates multiple datasets from various sources including EEOC sexual harassment charges, BLS employment data, industry wage information, and union membership statistics.

## Directory Structure
```
SEXUAL HARASSMENT/
├── code/
│   ├── 0a.cleaning_sh_data.py
│   ├── 0b.bls_data.py
│   ├── 0c.wage_data.py
│   ├── 0d.union_data.py
│   ├── 1.merging_labor_data.py
│   ├── analyzing_data.py
│   ├── multilevel_data.R
│   ├── multilevel_second_round.R
│   └── stan_brms.R
├── data/
│   ├── bls_data/
│   │   ├── bls_*.txt.txt (Years 1995-2016)
│   │   └── combined_bls_data.csv
│   ├── emp_wage/
│   │   ├── nat*.xls (Various wage data files)
│   │   └── wages_industry_year.csv
│   ├── union_data/
│   │   ├── ind_*.xlsx (Years 1995-2016)
│   │   ├── processed/
│   │   │   └── union_coverage_1995_2016.csv
│   │   └── combined_union_industry.csv
│   ├── combined_bls_data.csv
│   ├── combined_union_industry.csv
│   ├── level_two_labor.csv
│   ├── merged_sh_labor.csv
│   ├── SH Charge Receipts.xlsx
│   └── wages_industry_year.csv
└── output/
```

## Data Sources

### 1. Sexual Harassment Charge Data
- **Source**: Equal Employment Opportunity Commission (EEOC)
- **Files**: `SH Charge Receipts.xlsx` (3 sheets)
- **Time Period**: Multiple years of sexual harassment charges
- **Key Variables**: 
  - Charge filing dates
  - Respondent (employer) NAICS codes
  - Complainant demographics (sex, race, national origin, age)
  - Employer size
  - Case status and resolution information

### 2. BLS Employment Data
- **Source**: Bureau of Labor Statistics
- **Files**: Text files (`bls_*.txt.txt`) for years 1995-2016
- **Time Period**: Annual data from 1995-2016
- **Key Variables**:
  - Total employment by industry
  - Percentage of women in workforce by industry
  - Percentage of Black workers by industry
  - Percentage of Hispanic workers by industry
  - Percentage of Asian workers by industry (starting in 2003)
- **Formats**: Various over time (pipe-delimited, fixed-width, HTML-derived text)

### 3. Wage Data
- **Source**: National Bureau of Economic Research (NBER) and BLS
- **Files**: Excel files in `emp_wage/` directory
- **Time Period**: 1997-2012
- **Key Variables**:
  - Industry employment totals
  - Hourly mean wages
  - Annual mean wages
  - Hourly median wages
  - Annual median wages
  - Wage percentiles (10th, 25th, 75th, 90th)
- **Classification**: Uses both SIC (pre-2002) and NAICS (post-2002) codes

### 4. Union Data
- **Source**: Union Membership and Coverage Database
- **Files**: Excel files (`ind_*.xlsx`) for years 1995-2016
- **Time Period**: Annual data from 1995-2016
- **Key Variables**:
  - Industry employment (in thousands)
  - Union members (in thousands)
  - Workers covered by collective bargaining (in thousands)
  - Percentage of workers who are union members
  - Percentage of workers covered by collective bargaining

## Data Processing Scripts

### 1. Data Cleaning and Preparation

#### 0a.cleaning_sh_data.py
Processes the EEOC sexual harassment charge data:
- Reads in `SH Charge Receipts.xlsx` and combines data from multiple sheets
- Extracts NAICS codes (first two digits) from the detailed NAICS fields
- Processes charge filing dates to extract years
- Creates demographic variables through dummy coding for:
  - Race (Asian, Black, White, Biracial, Other, Unknown)
  - Sex (Male, Female, Unknown)
  - Employee size categories (1-5 scale based on company size)
- Calculates age-related variables from dates of birth
- Converts industry codes between NAICS and SIC based on filing year
- Standardizes industry codes to match the format in other datasets
- Merges with level_two_labor.csv data
- Outputs merged_sh_labor.csv with standardized codes and demographic data

#### 0b.bls_data.py
Processes Bureau of Labor Statistics employment demographic data:
- Handles multiple file formats (pipe-delimited, text, HTML) across years 1995-2016
- Extracts employment statistics by demographic groups (women, Black, Hispanic, Asian)
- Standardizes column names and data formats across years
- Parses various BLS text file formats with custom parsers for each period
- Maps industry categories to standard industry codes
- Uses Holt forecasting to fill missing years (2000-2001)
- Creates consistent time series of demographic data by industry
- Calculates weighted demographic percentages
- Outputs combined_bls_data.csv with standardized industry codes and demographic statistics

#### 0c.wage_data.py
Processes industry wage data:
- Extracts employment and wage information from various file formats
- Standardizes column names using a comprehensive mapping dictionary
- Aggregates industry codes to appropriate level (SIC/NAICS)
- Handles transition between SIC (pre-2002) and NAICS (post-2002) code systems
- Calculates employment-weighted wage averages
- Projects wage data backwards using Holt's method for missing years
- Includes detailed data validation and error handling
- Creates visualization plots of wage trends by industry
- Outputs wages_industry_year.csv with standardized wage data by industry and year

#### 0d.union_data.py
Processes union membership and coverage data:
- Reads Excel files with union data from 1995-2016
- Maps industry classifications between different years
- Handles the transition from SIC to NAICS codes in 2002
- Creates industry-specific mappings for the transition year
- Aggregates certain manufacturing and service categories
- Performs special processing for 2002 data (subtracting specific categories)
- Recalculates coverage percentages after aggregation
- Outputs combined_union_industry.csv with union membership and coverage rates

### 2. Data Integration

#### 1.merging_labor_data.py
Integrates all data sources into a comprehensive dataset:
- Merges sexual harassment charge data with industry labor statistics
- Joins BLS demographic data, wage data, and union coverage information
- Harmonizes industry codes across all datasets
- Creates standardized industry mappings for cross-year analysis
- Handles edge cases and missing data
- Performs data validation on the merged dataset
- Creates industry-level aggregated statistics
- Outputs a final merged dataset (merged_sh_labor.csv) with all variables

### 3. Analysis

#### analyzing_data.py
Main analysis script that implements:
- Descriptive statistics of sexual harassment charges by industry
- Trend analysis of charges over time
- Correlation analysis between harassment rates and industry characteristics
- Demographic breakdowns of charge statistics
- Statistical tests for industry differences
- Data visualization of key relationships

#### 1.0multi_level_idea.R
Implements an intersectional analysis of sexual harassment claims:
- Uses multiple imputation to handle missing demographic data
- Creates a four-category outcome variable (Black Women, Non-Black Women, Black Non-Women, Non-Black Non-Women)
- Fits separate multilevel models for each demographic category
- Tests interactions between industry characteristics and demographic compositions
- Creates visualizations of temporal trends and interaction effects
- Applies Rubin's rules to pool results across imputed datasets

#### multilevel_data.R
Implements hierarchical/multilevel modeling analysis:
- Creates multi-level models with industry, year, and demographic predictors
- Accounts for nested data structure (charges within industries within years)
- Estimates random effects for industries and years
- Tests hypotheses about industry-level predictors of harassment charges
- Includes model diagnostics and validation
- Creates visualizations of model results

#### multilevel_second_round.R
Follow-up multilevel analysis with enhanced models:
- Addresses limitations identified in initial models
- Adds additional predictor variables
- Tests alternative model specifications
- Conducts sensitivity analyses
- Creates refined visualizations of relationships
- Performs cross-validation of model predictions

#### stan_brms.R
Implements Bayesian regression models using Stan and brms:
- Defines Bayesian multilevel models for harassment charge data
- Uses MCMC sampling to estimate model parameters
- Sets appropriate priors for model parameters
- Conducts posterior predictive checks
- Calculates Bayesian model comparison metrics
- Visualizes posterior distributions and predictions

## Key Data Transformations

### Industry Code Standardization
- Converts between SIC codes (pre-2002) and NAICS codes (2002 onwards)
- Implements comprehensive mapping dictionaries for industry classification systems
- Aggregates detailed industry classifications to broad categories (from ~300 to ~20 categories)
- Handles special cases for 2002 data when classification systems changed:
  - Identifies and correctly categorizes ambiguous industries
  - Applies specific mappings for manufacturing subcategories
  - Recalculates industry statistics after reallocating certain categories
- Creates standardized codes that allow for consistent cross-year analysis
- Manages industry code ranges (like '31-33' for manufacturing) and combination categories
- Aligns with standard government classification systems while maintaining analytical consistency

### Demographic Data Processing
- Processes race, gender, and employee size information with sophisticated handling of:
  - Missing/NA values with designated default categories
  - Multi-value categories (e.g., biracial individuals)
  - Ambiguous or partial demographic information
- Creates standardized dummy variables for categorical data using custom mapping functions
- Calculates employment-weighted demographic percentages
- Handles date conversions for age calculations with robust error handling
- Implements special handling for "Unknown" categories to avoid data loss
- Maintains demographic data integrity across multiple source formats and years
- Calculates age at time of charge from birth dates and filing dates

### Time Series Projection
- Uses Holt's method to project data backwards for missing years (particularly 2000-2001)
- Implements customized time series analysis with:
  - Optimized smoothing parameters (α=0.8, β=0.2-0.4) for trend sensitivity
  - Separate projections for each industry and metric
  - Employment-weighted projections for demographic percentages
- Ensures consistent methodology across industry categories
- Validates projections against available data points
- Creates complete time series spanning 1995-2016 for all industry metrics
- Generates visualizations to validate projection accuracy
- Handles the SIC to NAICS transition period with special methodology

## Generated Datasets

### combined_bls_data.csv
Comprehensive dataset of employment demographics by industry:
- Standardized industry codes across all years (SIC and NAICS)
- Complete time series for 1995-2016 (including projected years)
- Employment totals by industry and year
- Demographic percentages (women, Black, Hispanic, Asian workers)
- Classification system indicators (SIC/NAICS)
- Used for industry-level demographic control variables

### combined_union_industry.csv
Consolidated union statistics by industry and year:
- Industry codes standardized to match other datasets
- Employment totals by industry-year
- Union membership counts and percentages
- Collective bargaining coverage counts and percentages
- Special handling of 2002 transition year
- Consistent industry categorization across 1995-2016

### level_two_labor.csv
Industry-level dataset prepared for multilevel modeling:
- Aggregated labor market characteristics for each industry-year
- Includes union, wage, and demographic variables
- Pre-processed variables ready for statistical modeling
- Standardized industry classification
- Used as the second level in hierarchical models

### merged_sh_labor.csv
Integrated dataset combining sexual harassment charges with labor statistics:
- Individual-level sexual harassment charge data
- Industry-level characteristics from labor datasets
- Complainant demographic information
- Industry employment and unionization metrics
- Wage statistics matched to charge industry-years
- Complete set of control variables for analysis
- Used as primary dataset for multilevel models

### wages_industry_year.csv
Standardized wage information by industry and year:
- Hourly and annual wages (mean and median)
- Wage distribution percentiles (10th, 25th, 75th, 90th)
- Total employment by industry-year
- Consistent industry codes matching other datasets
- Complete time series with projected values for missing years
- Used for wage-related predictors in analysis models

## Technical Notes and Methodology

### Industry Classification Handling
- The project spans years before and after a major change in industry classification systems:
  - Standard Industrial Classification (SIC) used before 2002
  - North American Industry Classification System (NAICS) used from 2002 onwards
- The transition required extensive mapping work to maintain consistency:
  - Created detailed crosswalk tables between classification systems
  - Special attention to industries that were split or combined in the transition
  - Particularly complex for 2002 data, which sometimes contained elements of both systems
- Industry codes are aggregated to meaningful broader categories to:
  - Enable comparison across years and classification systems
  - Ensure sufficient sample sizes for analysis
  - Focus on theoretically relevant industry distinctions

### Missing Data Strategy
- Multiple approaches to handle missing data:
  - Time-series forecasting (Holt's method) for systematic year gaps
  - Employment-weighted imputation for missing demographic percentages
  - Special handling of "Unknown" categories with dedicated dummy variables
  - Careful documentation of all imputation decisions
- Years 2000-2001 received particular attention as they fell during the classification transition
- Alternative imputation methods were tested and validated before selection

### Statistical Weighting
- All percentages and averages use appropriate employment weights:
  - Demographic percentages are calculated using total employment as weights
  - Industry-level statistics are weighted by size within aggregated categories
  - Union coverage calculations account for both membership and contract coverage
- This ensures that statistics represent the true employment distribution and aren't skewed by small industries

### Data Validation Procedures
- Each processing script includes:
  - Explicit validation steps for intermediate outputs
  - Error checking for unexpected values or patterns
  - Logging of potential issues for human review
  - Comparison of results to known benchmarks when available
- Consistency checks were performed between datasets to ensure:
  - Industry totals matched across sources
  - Demographic distributions were plausible
  - Time trends were consistent with published statistics

## Requirements and Dependencies

### Python Environment (3.6+)
- **Data Manipulation**:
  - pandas (1.0.0+): Core data manipulation and analysis
  - numpy (1.18.0+): Numerical computations and array handling
  
- **Data Visualization**:
  - matplotlib (3.1.0+): Plotting and visualization
  
- **Statistical Analysis**:
  - statsmodels (0.11.0+): Statistical models, time series analysis
  - jellyfish: String matching and fuzzy comparison algorithms
  
- **File Processing**:
  - openpyxl: Excel file handling (especially for newer .xlsx files)
  - pathlib: Path management with object-oriented interface
  
- **Additional Tools**:
  - requests: HTTP requests for potential data retrieval
  - re: Regular expression pattern matching for text processing
  - collections: Specialized container datatypes (defaultdict)
  - datetime: Date and time handling
  - prettytable: ASCII table formatting for console output

### R Environment (4.0+)
- **Multilevel Modeling**:
  - lme4: Linear mixed-effects models
  - brms: Bayesian regression models using Stan
  
- **Visualization**:
  - ggplot2: Data visualization
  - bayesplot: Plotting for Bayesian models
  
- **Additional Packages**:
  - dplyr: Data manipulation
  - tidyr: Data tidying
  - rstan: Stan interface for R
  - sjPlot: Visualization for statistical models
  - DHARMa: Residual diagnostics for hierarchical models

## Project Execution

### Data Processing Workflow
1. **Data Preparation** (Run in sequence):
   ```
   python 0a.cleaning_sh_data.py
   python 0b.bls_data.py
   python 0c.wage_data.py
   python 0d.union_data.py
   ```

2. **Data Integration**:
   ```
   python 1.merging_labor_data.py
   ```

3. **Data Analysis**:
   ```
   python analyzing_data.py
   ```

4. **Statistical Modeling** (R scripts):
   ```
   Rscript multilevel_data.R
   Rscript multilevel_second_round.R
   Rscript stan_brms.R
   ```

### Recommended Execution Environment
- Use consistent Python and R environments across all scripts
- Consider using virtual environments (Python) or renv (R) for reproducibility
- Memory requirements: Minimum 8GB RAM recommended for processing larger datasets
- Storage: Approximately 1GB for raw and processed data files
- Some scripts may take several minutes to run due to time-series modeling and data transformation complexity