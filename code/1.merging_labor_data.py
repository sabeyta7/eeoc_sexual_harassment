# Importing the necessary libraries
import pandas as pd
import numpy as np
import os
import json
import matplotlib.pyplot as plt
import jellyfish  # for fuzzy matching
from collections import defaultdict
from datetime import datetime
import re
import requests
from time import sleep
import prettytable
from pathlib import Path
from statsmodels.tsa.holtwinters import Holt
import openpyxl


##=======================================================================##
##======================Setting all the directories======================##
##=======================================================================##

# Set up directory paths
current_dir = os.getcwd()
project_root = os.path.dirname(current_dir)
data_folder = os.path.join(project_root, "data")

# Pulling in wage_industry_year, combined_union_industry, osha_industry_year, combined_bls_data
wage_industry_year = pd.read_csv(os.path.join(data_folder, "wages_industry_year.csv"))
combined_union_industry = pd.read_csv(os.path.join(data_folder, "combined_union_industry.csv"))
osha_industry_year = pd.read_csv(os.path.join(data_folder, "osha_industry_year.csv"))
combined_bls_data = pd.read_csv(os.path.join(data_folder, "combined_bls_data.csv"))

# Looking at columns in each dataframe
print(wage_industry_year.columns)
print(combined_union_industry.columns)
print(osha_industry_year.columns)
print(combined_bls_data.columns)

# First, standardize the column names in each dataframe
# Make copies to avoid modifying originals
wage_df = wage_industry_year.copy()
union_df = combined_union_industry.copy()
osha_df = osha_industry_year.copy()
bls_df = combined_bls_data.copy()


# Standardize column names first
union_df = union_df.rename(columns={
    'year': 'YEAR',
    'industry_code': 'INDUSTRY_CODE',
    'Employment (in 1000s)': 'UNION_EMPLOYMENT',
    'Members (in 1000s)': 'UNION_MEMBERS',
    'Covered (in 1000s)': 'UNION_COVERED',
    '% Mem': 'UNION_MEMBER_PERCENT',
    '% Cov': 'UNION_COVERAGE_PERCENT'
})

osha_df = osha_df.rename(columns={
    'year': 'YEAR',
    'industry_code': 'INDUSTRY_CODE',
    'inspection_count': 'OSHA_INSPECTIONS',
    'violation_count': 'OSHA_VIOLATIONS',
    'avg_penalty': 'OSHA_AVG_PENALTY',
    'avg_gravity': 'OSHA_AVG_GRAVITY',
    'accident_count': 'OSHA_ACCIDENTS',
    'avg_injury_degree': 'OSHA_AVG_INJURY'
})

bls_df = bls_df.rename(columns={
    'Year': 'YEAR',
    'Industry_Code': 'INDUSTRY_CODE',
    'Total_Employed': 'BLS_TOTAL_EMPLOYED',
    'Women_Percent': 'WOMEN_PERCENT',
    'Black_Percent': 'BLACK_PERCENT',
    'Asian_Percent': 'ASIAN_PERCENT',
    'Hispanic_Percent': 'HISPANIC_PERCENT'
})

# Convert types for merging
wage_df['INDUSTRY_CODE'] = wage_df['INDUSTRY_CODE'].astype(str)
union_df['INDUSTRY_CODE'] = union_df['INDUSTRY_CODE'].astype(str)
osha_df['INDUSTRY_CODE'] = osha_df['INDUSTRY_CODE'].astype(str)
bls_df['INDUSTRY_CODE'] = bls_df['INDUSTRY_CODE'].astype(str)

# Ensure YEAR is integer
wage_df['YEAR'] = wage_df['YEAR'].astype(int)
union_df['YEAR'] = union_df['YEAR'].astype(int)
osha_df['YEAR'] = osha_df['YEAR'].astype(int)
bls_df['YEAR'] = bls_df['YEAR'].astype(int)

# Check for duplicates in each dataset
print("Duplicates in wage data:")
print(wage_df[wage_df.duplicated(['YEAR', 'INDUSTRY_CODE'], keep=False)].sort_values(['YEAR', 'INDUSTRY_CODE']))

print("\nDuplicates in union data:")
print(union_df[union_df.duplicated(['YEAR', 'INDUSTRY_CODE'], keep=False)].sort_values(['YEAR', 'INDUSTRY_CODE']))

print("\nDuplicates in OSHA data:")
print(osha_df[osha_df.duplicated(['YEAR', 'INDUSTRY_CODE'], keep=False)].sort_values(['YEAR', 'INDUSTRY_CODE']))

print("\nDuplicates in BLS data:")
print(bls_df[bls_df.duplicated(['YEAR', 'INDUSTRY_CODE'], keep=False)].sort_values(['YEAR', 'INDUSTRY_CODE']))


# Now let's modify our merge without the validation
merged_df = wage_df.merge(
    union_df,
    on=['YEAR', 'INDUSTRY_CODE'],
    how='left'
).merge(
    osha_df,
    on=['YEAR', 'INDUSTRY_CODE'],
    how='left'
).merge(
    bls_df,
    on=['YEAR', 'INDUSTRY_CODE'],
    how='left'
)

# After merging, let's check for any unexpected duplicates
print("\nDuplicates in final merged data:")
print(merged_df[merged_df.duplicated(['YEAR', 'INDUSTRY_CODE'], keep=False)].sort_values(['YEAR', 'INDUSTRY_CODE']))

# Save the merged data to a CSV file
merged_df.to_csv(os.path.join(data_folder, "level_two_labor.csv"), index=False)