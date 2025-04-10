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

# Get current directory and construct paths
current_dir = os.getcwd()
project_root = os.path.dirname(current_dir)
data_folder = os.path.join(project_root, "data")

# Reading in SH Charge Recipients.xlsx. Appending each sheet into one dataframe
sh_data_1 = pd.read_excel(os.path.join(data_folder, "SH Charge Receipts.xlsx"), sheet_name='Sheet 1')
sh_data_2 = pd.read_excel(os.path.join(data_folder, "SH Charge Receipts.xlsx"), sheet_name='Sheet 2')
sh_data_3 = pd.read_excel(os.path.join(data_folder, "SH Charge Receipts.xlsx"), sheet_name='Sheet 3')

sh_data = pd.concat([sh_data_1, sh_data_2, sh_data_3], ignore_index=True)

# Pulling in the level 2 data called level_two_labor.csv
level_two_labor = pd.read_csv(os.path.join(data_folder, "level_two_labor.csv"))

# Getting a column that is just the first two digits R_NAICS_CODE
sh_data['naics'] = sh_data['R_NAICS_CODE'].astype(str).str[:2]

# Value counts of the first two digits of the NAICS code
sh_data['naics'].value_counts()

# Subsetting where naics is not "na"
sh_data = sh_data[sh_data['naics'] != 'na']

# Pulling out the year from CHARGE_FILING_DATE
sh_data['CHARGE_FILING_DATE'] = pd.to_datetime(sh_data['CHARGE_FILING_DATE'])
sh_data['Year'] = sh_data['CHARGE_FILING_DATE'].dt.year

# Creating a table of the breakdown of naics by CHARGE_FILING_DATE year
naics_year_table = sh_data.groupby(['Year', 'naics']).size().unstack().fillna(0)

# Displaying the table
print(naics_year_table)

# Value counts of CP_SEX CP_NATIONAL_ORIGIN, CP_RACE_STRING, R_NUMBER_OF_EMPLOYEES
print(sh_data['CP_SEX'].value_counts())
print(sh_data['CP_NATIONAL_ORIGIN'].value_counts())
print(sh_data['R_NUMBER_OF_EMPLOYEES'].value_counts())
print(sh_data['CP_RACE_STRING'].value_counts())
def create_dummy_columns(df, column, mapping):
    """
    Generic function to create dummy columns based on a mapping dictionary
    
    Args:
        df: DataFrame containing the column to be dummy coded
        column: Name of the column to dummy code
        mapping: Dictionary with category mappings
    """
    def dummy_code(value, categories):
        # Initialize all categories to 0
        result = {cat: 0 for cat in categories}
        
        # Handle NaN values
        if pd.isna(value):
            result[mapping.get('na_category', 'Unknown')] = 1
            return result
            
        if isinstance(value, str):
            # For racial categories, check if multiple races are indicated
            if 'multivalue_category' in mapping and len(set(value)) > 1:
                # Reset all to 0 and set only biracial to 1
                for cat in categories:
                    result[cat] = 0
                result[mapping['multivalue_category']] = 1
            else:
                # Single category mapping
                mapped = False
                for key, category in mapping['values'].items():
                    if any(value.startswith(k) for k in key):
                        result[category] = 1
                        mapped = True
                        break
                
                # If no mapping found, use default category
                if not mapped and 'default_category' in mapping:
                    result[mapping['default_category']] = 1
                
        return result

    # Create dummy columns
    dummies = df[column].apply(lambda x: dummy_code(x, set(mapping['values'].values()) | 
                                                  {mapping.get('multivalue_category')} | 
                                                  {mapping.get('default_category')} - {None}))
    dummy_df = pd.DataFrame(dummies.tolist(), index=df.index)
    
    return pd.concat([df, dummy_df], axis=1)

# Mapping dictionaries
RACE_MAPPING = {
    'values': {
        ('A', 'S'): 'Asian',
        ('B',): 'Black',
        ('W',): 'White',
        ('Z', 'N'): 'Unknown',
    },
    'multivalue_category': 'Biracial',
    'default_category': 'Other',
    'na_category': 'Unknown'
}

SEX_MAPPING = {
    'values': {
        ('Male',): 'Male',
        ('Female',): 'Female',
        ('CP Sex Not Available/Applicable',): 'Unknown'
    },
    'na_category': 'Unknown'
}

EMPLOYEE_SIZE_MAPPING = {
    'Under 15 Employees': 1,
    '15 - 100 Employees': 2,
    '101 - 200 Employees': 3,
    '201 - 500 Employees': 4,
    '501+ Employees': 5,
    'Unknown Number Of Employees': 6
}

def process_data(df):
    """Main function to process all categorical columns"""
    # Process race and sex columns
    df = create_dummy_columns(df, 'CP_RACE_STRING', RACE_MAPPING)
    df = create_dummy_columns(df, 'CP_SEX', SEX_MAPPING)
    
    # Process employee size
    df['R_NUMBER_OF_EMPLOYEES_ORDERED'] = df['R_NUMBER_OF_EMPLOYEES'].map(EMPLOYEE_SIZE_MAPPING)
    
    return df

# Example usage:
sh_data = process_data(sh_data)

print("\nRace categories:")
print(sh_data[['Asian', 'Black', 'White', 'Unknown', 'Biracial', 'Other']].sum())
print("\nSex categories:")
print(sh_data[['Male', 'Female', 'Unknown']].sum())

def calculate_ages(df, dob_column='CP_DOB', year_column='Year'):
    """
    Calculate ages from DOB, letting invalid dates become NaN
    """
    # Convert dates with coerce to handle invalid dates as NaT
    df['CP_DOB_clean'] = pd.to_datetime(df[dob_column], errors='coerce')
    
    # Calculate ages - invalid dates will automatically become NaN
    df['Age'] = 2016 - df['CP_DOB_clean'].dt.year
    df['Age_at_time_of_charge'] = df[year_column] - df['CP_DOB_clean'].dt.year
    
    # Remove the intermediate date column
    df = df.drop('CP_DOB_clean', axis=1)
    
    return df

# Usage:
sh_data = calculate_ages(sh_data)

def convert_industry_codes(df):
    """
    Convert industry codes between NAICS and SIC based on year,
    ensuring every NAICS code maps to one of the specified SIC codes
    """
    # Target SIC codes
    sic_codes = ['07', '10', '15', '20', '40', '48', '49', '50', '52', 
                 '60', '72', '75', '79', '80', '82', '83', '89', '90']
    
    # Comprehensive NAICS to SIC mapping
    naics_to_sic = {
        # Agriculture, Forestry, Fishing and Hunting
        '11': '07',  # Agriculture
        
        # Mining, Utilities, Construction
        '21': '10',  # Mining
        '22': '49',  # Utilities map to transportation/communication/utilities
        '23': '15',  # Construction
        
        # Manufacturing
        '31': '20',
        '32': '20',
        '33': '20',
        
        # Trade and Transportation
        '42': '50',  # Wholesale Trade
        '44': '52',  # Retail Trade
        '45': '52',  # Retail Trade
        '48': '40',  # Transportation
        '49': '48',  # Transportation/Warehousing
        
        # Information, Finance, Real Estate
        '51': '48',  # Information -> communication
        '52': '60',  # Finance and Insurance
        '53': '60',  # Real Estate -> finance
        
        # Professional and Business Services
        '54': '83',  # Professional Services
        '55': '83',  # Management of Companies
        '56': '75',  # Administrative and Support Services
        
        # Education and Health Services
        '61': '82',  # Educational Services
        '62': '80',  # Health Care
        
        # Leisure and Hospitality
        '71': '79',  # Arts, Entertainment, and Recreation
        '72': '72',  # Accommodation and Food Services
        
        # Other Services
        '81': '89',  # Other Services
        '92': '90',  # Public Administration
        
        # Catch-all for any other codes (map to Other Services)
        '00': '89',
        '99': '89'
    }
    
    # Create function to handle any unmapped codes
    def map_to_sic(naics):
        if pd.isna(naics):
            return '89'  # Map NaN to Other Services
        
        naics_2digit = str(naics)[:2]
        return naics_to_sic.get(naics_2digit, '89')  # Default to Other Services if no match
    
    # Create a new column for the standardized industry code
    df['industry_code_std'] = df['naics'].copy()
    
    # Convert to SIC for years 1995-2001
    mask = df['Year'].between(1995, 2001)
    df.loc[mask, 'industry_code_std'] = df.loc[mask, 'naics'].apply(map_to_sic)
    
    # Verify mapping
    early_years = df[mask]
    print("\nIndustry code distribution for 1995-2001:")
    print(early_years['industry_code_std'].value_counts().sort_index())
    
    # Verify all codes are in the expected list
    unexpected = set(early_years['industry_code_std'].unique()) - set(sic_codes)
    if unexpected:
        raise ValueError(f"Found unexpected SIC codes: {unexpected}")
    
    # Verify no null values
    if early_years['industry_code_std'].isna().any():
        raise ValueError("Found null values in industry codes")
    
    return df

# Example usage:
sh_data = convert_industry_codes(sh_data)

print("sh_data years:")
print(sh_data['Year'].value_counts().sort_index())
print("\nlevel_two_labor years:")
print(level_two_labor['YEAR'].value_counts().sort_index())

def standardize_for_merge(df):
    """
    Convert individual NAICS codes to their range categories
    """
    # Map individual codes to ranges
    range_mappings = {
        '31': '31-33',
        '32': '31-33',
        '33': '31-33',
        '44': '44-45',
        '45': '44-45',
        '48': '48-49',
        '49': '48-49'
    }
    
    # Create new column with range mappings
    df['industry_code_merged'] = df['industry_code_std'].map(lambda x: range_mappings.get(x, x))
    
    return df

# Apply the mapping and merge
sh_data = standardize_for_merge(sh_data)

# Merge with correct column name case
merged_data = pd.merge(sh_data, level_two_labor,
                      left_on=['Year', 'industry_code_merged'],
                      right_on=['YEAR', 'INDUSTRY_CODE'],
                      how='left')

# Check merge results
print("\nMerge verification:")
print(f"Original sh_data rows: {len(sh_data)}")
print(f"Merged data rows: {len(merged_data)}")


# Clean up duplicate columns
# Drop the None column and rename duplicates
merged_data = merged_data.loc[:, ~merged_data.columns.isnull()]  # Remove None column
merged_data = merged_data.rename(columns={'Unknown': 'Unknown_Race'}).rename(columns={'Unknown': 'Unknown_Sex'})

# Now fill OSHA values
for col in merged_data.columns:
    if col.startswith('OSHA_'):
        merged_data[col] = merged_data[col].fillna(0)
        
# Save the merged data to a CSV file
merged_data.to_csv(os.path.join(data_folder, "merged_sh_labor.csv"), index=False)

