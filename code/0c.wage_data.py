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
from typing import List, Tuple


##=======================================================================##
##======================Setting all the directories======================##
##=======================================================================##

# Get current directory and construct paths
current_dir = os.getcwd()
project_root = os.path.dirname(current_dir)
data_folder = os.path.join(project_root, "data", "emp_wage")

# Column mapping dictionary
column_mapping = {
    # Industry codes
    'NAICS': 'INDUSTRY_CODE',
    'naics': 'INDUSTRY_CODE',
    'sic': 'INDUSTRY_CODE',
    
    # Industry titles
    'NAICS_TITLE': 'INDUSTRY_TITLE',
    'naics_title': 'INDUSTRY_TITLE',
    'sic_title': 'INDUSTRY_TITLE',
    'sic_title ': 'INDUSTRY_TITLE',
    
    # Occupation
    'occ_code': 'OCC_CODE',
    'occ_title': 'OCC_TITLE',
    'occ_titl': 'OCC_TITLE',
    'GROUP': 'OCC_GROUP',
    'OCC_GROUP': 'OCC_GROUP',
    
    # Employment
    'tot_emp': 'TOT_EMP',
    'pct_total': 'PCT_TOTAL',
    'pct_rpt': 'PCT_RPT',
    'emp_prse': 'EMP_PRSE',
    'mean_prse': 'MEAN_PRSE',
    
    # Wages
    'h_mean': 'H_MEAN',
    'a_mean': 'A_MEAN',
    'h_median': 'H_MEDIAN',
    'a_median': 'A_MEDIAN',
    'h_pct10': 'H_PCT10',
    'h_pct25': 'H_PCT25',
    'h_pct75': 'H_PCT75',
    'h_pct90': 'H_PCT90',
    'a_pct10': 'A_PCT10',
    'a_pct25': 'A_PCT25',
    'a_pct75': 'A_PCT75',
    'a_pct90': 'A_PCT90'
}

def extract_year(filename):
    """Extract year from filename, handling different formats."""
    parts = filename.split('_')
    for part in parts:
        clean_part = ''.join(filter(str.isdigit, part))
        if len(clean_part) >= 4:
            year = clean_part[:4]
            if 1990 <= int(year) <= 2025:
                return year
    return None

def clean_numeric_columns(df):
    """Convert all numeric columns to float, handling any string values"""
    numeric_cols = ['TOT_EMP', 'H_MEAN', 'A_MEAN', 'H_MEDIAN', 'A_MEDIAN', 
                   'H_PCT10', 'H_PCT25', 'H_PCT75', 'H_PCT90',
                   'A_PCT10', 'A_PCT25', 'A_PCT75', 'A_PCT90']
    
    for col in numeric_cols:
        if col in df.columns:
            df[col] = pd.to_numeric(df[col], errors='coerce')
    return df

def aggregate_codes(df, filename):
    """Aggregate industry codes with proper type handling"""
    print(f"\nProcessing {filename}")
    
    try:
        # Convert numeric columns first
        df = clean_numeric_columns(df)
        
        # For SIC files (pre-2002)
        if any(f"sic_{year}" in filename.lower() for year in ['1997', '1998', '1999', '2000', '2001']):
            print("Aggregating SIC codes to categories")
            
            # Reset index to handle potential index issues
            df = df.reset_index(drop=True)
            
            # First extract the SIC code from the INDUSTRY_TITLE if INDUSTRY_CODE is null
            if df['INDUSTRY_CODE'].isna().all():
                df['INDUSTRY_CODE'] = df['INDUSTRY_TITLE'].str.extract(r'^(\d+)', expand=True)
            
            # Clean and standardize industry codes
            df['INDUSTRY_CODE'] = df['INDUSTRY_CODE'].fillna('')
            df['INDUSTRY_CODE'] = df['INDUSTRY_CODE'].astype(str)
            df['INDUSTRY_CODE'] = df['INDUSTRY_CODE'].str.extract(r'(\d+)', expand=True).fillna('')
            df['INDUSTRY_CODE'] = df['INDUSTRY_CODE'].str.zfill(4)
            
            # Check for reversed codes by looking at first code
            first_code = df['INDUSTRY_CODE'].iloc[0] if len(df) > 0 else ''
            if first_code.startswith('00'):
                print("Found reversed SIC codes, fixing...")
                df['INDUSTRY_CODE'] = df['INDUSTRY_CODE'].apply(
                    lambda x: x[2:4] + x[0:2]
                )
            
            print(f"Unique raw industry codes after standardization: {sorted(df['INDUSTRY_CODE'].unique())}")
            
            
            # Your original mappings
            detailed_to_broad = {
                # [Previous mapping dictionary stays exactly the same]
                # Agriculture & Forestry (07-09)
                '0700': 'AGRICULTURE',
                '0800': 'FORESTRY AND FISHERIES',
                '0900': 'FORESTRY AND FISHERIES',
                
                # Mining (10-14)
                '1000': 'MINING',
                '1200': 'MINING',
                '1300': 'MINING',
                '1400': 'MINING',
                
                # Construction (15-17)
                '1500': 'CONSTRUCTION',
                '1600': 'CONSTRUCTION',
                '1700': 'CONSTRUCTION',
                
                # Manufacturing - both durable and nondurable map to 20
                '2000': 'MANUFACTURING, NONDURABLE GOODS',
                '2100': 'MANUFACTURING, NONDURABLE GOODS',
                '2200': 'MANUFACTURING, NONDURABLE GOODS',
                '2300': 'MANUFACTURING, NONDURABLE GOODS',
                '2400': 'MANUFACTURING, DURABLE GOODS',
                '2500': 'MANUFACTURING, DURABLE GOODS',
                '2600': 'MANUFACTURING, NONDURABLE GOODS',
                '2700': 'MANUFACTURING, NONDURABLE GOODS',
                '2800': 'MANUFACTURING, NONDURABLE GOODS',
                '2900': 'MANUFACTURING, NONDURABLE GOODS',
                '3000': 'MANUFACTURING, NONDURABLE GOODS',
                '3100': 'MANUFACTURING, DURABLE GOODS',
                '3200': 'MANUFACTURING, DURABLE GOODS',
                '3300': 'MANUFACTURING, DURABLE GOODS',
                '3400': 'MANUFACTURING, DURABLE GOODS',
                '3500': 'MANUFACTURING, DURABLE GOODS',
                '3600': 'MANUFACTURING, DURABLE GOODS',
                '3700': 'MANUFACTURING, DURABLE GOODS',
                '3800': 'MANUFACTURING, DURABLE GOODS',
                '3900': 'MANUFACTURING, DURABLE GOODS',
                
                # Transportation & Utilities
                '4000': 'TRANSPORTATION',
                '4100': 'TRANSPORTATION',
                '4200': 'TRANSPORTATION',
                '4400': 'TRANSPORTATION',
                '4500': 'TRANSPORTATION',
                '4600': 'TRANSPORTATION',
                '4700': 'TRANSPORTATION',
                '4800': 'COMMUNICATIONS',
                '4900': 'UTILITIES AND SANITARY SERVICES',
                
                # Trade
                '5000': 'WHOLESALE TRADE',
                '5100': 'WHOLESALE TRADE',
                '5200': 'RETAIL TRADE',
                '5300': 'RETAIL TRADE',
                '5400': 'RETAIL TRADE',
                '5500': 'RETAIL TRADE',
                '5600': 'RETAIL TRADE',
                '5700': 'RETAIL TRADE',
                '5800': 'RETAIL TRADE',
                '5900': 'RETAIL TRADE',
                
                # Finance
                '6000': 'FINANCE, INSURANCE, AND REAL ESTATE',
                '6100': 'FINANCE, INSURANCE, AND REAL ESTATE',
                '6200': 'FINANCE, INSURANCE, AND REAL ESTATE',
                '6300': 'FINANCE, INSURANCE, AND REAL ESTATE',
                '6400': 'FINANCE, INSURANCE, AND REAL ESTATE',
                '6500': 'FINANCE, INSURANCE, AND REAL ESTATE',
                '6700': 'FINANCE, INSURANCE, AND REAL ESTATE',
                
                # Services
                '7200': 'PERSONAL SERVICES',
                '7300': 'BUSINESS AND REPAIR SERVICES',
                '7500': 'BUSINESS AND REPAIR SERVICES',
                '7600': 'BUSINESS AND REPAIR SERVICES',
                '7800': 'ENTERTAINMENT AND RECREATION SERVICES',
                '7900': 'ENTERTAINMENT AND RECREATION SERVICES',
                '8000': 'MEDICAL SERVICES, EXCEPT HOSPITALS',
                '8100': 'OTHER PROFESSIONAL SERVICES',
                '8200': 'EDUCATIONAL SERVICES',
                '8300': 'SOCIAL SERVICES',
                '8400': 'OTHER PROFESSIONAL SERVICES',
                '8600': 'OTHER PROFESSIONAL SERVICES',
                '8700': 'OTHER PROFESSIONAL SERVICES',
                '8900': 'OTHER PROFESSIONAL SERVICES',
                
                # Public Admin
                '9000': 'PUBLIC ADMINISTRATION'
            }

            # Second mapping: broad categories to final codes
            broad_to_simple = {
                'AGRICULTURE': '07',
                'FORESTRY AND FISHERIES': '07',
                'MINING': '10',
                'CONSTRUCTION': '15',
                'MANUFACTURING, NONDURABLE GOODS': '20',
                'MANUFACTURING, DURABLE GOODS': '20',
                'TRANSPORTATION': '40',
                'COMMUNICATIONS': '48',
                'UTILITIES AND SANITARY SERVICES': '49',
                'WHOLESALE TRADE': '50',
                'RETAIL TRADE': '52',
                'FINANCE, INSURANCE, AND REAL ESTATE': '60',
                'BUSINESS AND REPAIR SERVICES': '75',
                'PERSONAL SERVICES': '72',
                'ENTERTAINMENT AND RECREATION SERVICES': '79',
                'MEDICAL SERVICES, EXCEPT HOSPITALS': '80',
                'HOSPITALS': '80',
                'OTHER PROFESSIONAL SERVICES': '89',
                'EDUCATIONAL SERVICES': '82',
                'SOCIAL SERVICES': '83',
                'PUBLIC ADMINISTRATION': '90'
            }

            # Map to categories
            df['broad_category'] = df['INDUSTRY_CODE'].map(detailed_to_broad)
            df['INDUSTRY_CODE'] = df['broad_category'].map(broad_to_simple)
            
            # Clean up and aggregate
            df = df.drop('broad_category', axis=1, errors='ignore')
            
            # Group and aggregate
            agg_dict = {
                'TOT_EMP': 'sum',
                'H_MEAN': lambda x: np.average(x, weights=df.loc[x.index, 'TOT_EMP']),
                'A_MEAN': lambda x: np.average(x, weights=df.loc[x.index, 'TOT_EMP']),
                'H_MEDIAN': lambda x: np.average(x, weights=df.loc[x.index, 'TOT_EMP']),
                'A_MEDIAN': lambda x: np.average(x, weights=df.loc[x.index, 'TOT_EMP'])
            }
            
            # Only aggregate columns that exist
            agg_dict = {k: v for k, v in agg_dict.items() if k in df.columns}
            
            # Group by appropriate columns
            if 'year' in df.columns:
                grouped = df.groupby(['INDUSTRY_CODE', 'year'], as_index=False).agg(agg_dict)
            else:
                grouped = df.groupby('INDUSTRY_CODE', as_index=False).agg(agg_dict)
            
            print(f"Final categories after SIC aggregation: {sorted(grouped['INDUSTRY_CODE'].unique())}")
            return grouped

        # For NAICS files (2002 onwards) - exactly as before
        elif '4d_2002_dl' in filename or '3d_may2003_dl' in filename:
            print("Aggregating NAICS codes to 2-digit")
            df['INDUSTRY_CODE'] = df['INDUSTRY_CODE'].astype(str).str.zfill(4).str[:2]
            
            naics_mapping = {
                '31': '31-33', '32': '31-33', '33': '31-33',
                '44': '44-45', '45': '44-45',
                '48': '48-49', '49': '48-49'
            }
            df['INDUSTRY_CODE'] = df['INDUSTRY_CODE'].map(lambda x: naics_mapping.get(x, x))
        
            agg_dict = {
                'TOT_EMP': 'sum',
                'H_MEAN': lambda x: np.average(x, weights=df.loc[x.index, 'TOT_EMP']),
                'A_MEAN': lambda x: np.average(x, weights=df.loc[x.index, 'TOT_EMP']),
                'H_MEDIAN': lambda x: np.average(x, weights=df.loc[x.index, 'TOT_EMP']),
                'A_MEDIAN': lambda x: np.average(x, weights=df.loc[x.index, 'TOT_EMP'])
            }
            
            agg_dict = {k: v for k, v in agg_dict.items() if k in df.columns}
            
            if 'year' in df.columns:
                grouped = df.groupby(['INDUSTRY_CODE', 'year'], as_index=False).agg(agg_dict)
            elif 'YEAR' in df.columns:
                grouped = df.groupby(['INDUSTRY_CODE', 'YEAR'], as_index=False).agg(agg_dict)
            else:
                grouped = df.groupby('INDUSTRY_CODE', as_index=False).agg(agg_dict)
                
            print(f"Final categories after NAICS aggregation: {sorted(grouped['INDUSTRY_CODE'].unique())}")
            return grouped
            
        return df

    except Exception as e:
        print(f"Detailed error during aggregation: {str(e)}")
        print(f"DataFrame info:")
        print(df.info())
        print("First few rows of INDUSTRY_CODE:")
        print(df['INDUSTRY_CODE'].head())
        return df

# Process files
all_dfs = []

for filename in os.listdir(data_folder):
    if filename.endswith('.xls') or filename.endswith('.xlsx'):
        filepath = os.path.join(data_folder, filename)
        try:
            # Choose engine based on file extension
            engine = 'xlrd' if filename.endswith('.xls') else 'openpyxl'
            
            # Read the file
            df = pd.read_excel(filepath, engine=engine)
            print(f"\nSuccessfully loaded {filename}")
            
            # Rename columns
            df = df.rename(columns=column_mapping)
            
            # Filter for Industry Total and create copy
            df_filtered = df[df['OCC_TITLE'] == 'Industry Total'].copy()
            
            # Aggregate if needed
            df_filtered = aggregate_codes(df_filtered, filename)
            
            # Add year
            year = extract_year(filename)
            if year:
                df_filtered.loc[:, 'YEAR'] = year
            
            all_dfs.append(df_filtered)
            print(f"Records in processed file: {len(df_filtered)}")
            
        except Exception as e:
            print(f"Error processing {filename}: {str(e)}")

# Combine all dataframes
final_df = pd.concat(all_dfs, ignore_index=True)
print("\nFinal dataframe shape:", final_df.shape)
print("Records per year:")
print(final_df.groupby('YEAR').size())

# Print industry codes for the 1990s
print("\nIndustry codes for the 1990s:")
print(final_df[final_df['YEAR'] < '2002']['INDUSTRY_CODE'].unique())

def prepare_time_series(df: pd.DataFrame, sic_code: str, column: str) -> Tuple[pd.Series, List[int]]:
    """
    Prepare time series data for a specific SIC code and column.
    """
    # Filter for specific SIC code and sort by year
    temp_df = df[df['INDUSTRY_CODE'] == sic_code].sort_values('YEAR')
    # Convert years to integers
    years = temp_df['YEAR'].astype(int).tolist()
    values = temp_df[column].tolist()
    return pd.Series(values, index=years), years

def project_backwards(series: pd.Series, years_to_project: int = 2, 
                     alpha: float = 0.8, beta: float = 0.2) -> pd.Series:
    """
    Project time series backwards using Holt's method.
    """
    # Reverse the series for backwards projection
    reversed_series = pd.Series(series.values[::-1], 
                              index=range(len(series)))
    
    # Fit Holt's model
    model = Holt(reversed_series, initialization_method="estimated")
    fit = model.fit(smoothing_level=alpha, smoothing_slope=beta)
    
    # Make prediction
    forecast = fit.forecast(years_to_project)
    
    # Reverse back and combine
    projected_values = forecast.values[::-1]
    
    # Create years for projection (now with integer index)
    min_year = int(min(series.index))
    projected_years = range(min_year - years_to_project, min_year)
    
    # Create projected series
    projected_series = pd.Series(projected_values, index=projected_years)
    
    return pd.concat([projected_series, series])

def project_all_metrics(df: pd.DataFrame, metrics: List[str], 
                       sic_codes: List[str]) -> pd.DataFrame:
    """
    Project all metrics for all SIC codes.
    """
    projected_dfs = []
    
    for sic in sic_codes:
        for metric in metrics:
            # Prepare time series
            series, years = prepare_time_series(df, sic, metric)
            
            # Project backwards
            projected = project_backwards(series)
            
            # Create temporary DataFrame with projections
            temp_df = pd.DataFrame({
                'YEAR': projected.index,
                'INDUSTRY_CODE': sic,
                metric: projected.values
            })
            
            projected_dfs.append(temp_df)
    
    # Combine all projections
    projected_df = pd.concat(projected_dfs)
    
    # Pivot to get final format
    final_df = projected_df.pivot_table(
        index=['YEAR', 'INDUSTRY_CODE'],
        columns=None,
        values=metrics,
        aggfunc='first'
    ).reset_index()
    
    return final_df.sort_values(['YEAR', 'INDUSTRY_CODE'])

def plot_projections(original_df: pd.DataFrame, projected_df: pd.DataFrame, 
                    metric: str, sic_codes: List[str], 
                    figsize: tuple = (15, 8)) -> None:
    """
    Plot original data and projections for a specific metric.
    
    Args:
        original_df: Original DataFrame
        projected_df: DataFrame with projections
        metric: Metric to plot
        sic_codes: List of SIC codes to include
        figsize: Figure size (width, height)
    """
    # Convert YEAR to int in both dataframes
    original_df = original_df.copy()
    projected_df = projected_df.copy()
    original_df['YEAR'] = pd.to_numeric(original_df['YEAR'])
    projected_df['YEAR'] = pd.to_numeric(projected_df['YEAR'])
    plt.figure(figsize=figsize)
    
    # Set style
    #sns.set_style("whitegrid")
    
    # Plot for each SIC code
    for sic in sic_codes:
        # Original data
        orig = original_df[original_df['INDUSTRY_CODE'] == sic]
        proj = projected_df[projected_df['INDUSTRY_CODE'] == sic]
        
        # Plot original data
        plt.plot(orig['YEAR'], orig[metric], 
                marker='o', label=f'SIC {sic} (Original)')
        
        # Plot projected data (only the projected years)
        proj_only = proj[proj['YEAR'] < min(orig['YEAR'])]
        if not proj_only.empty:
            plt.plot(proj_only['YEAR'], proj_only[metric], 
                    linestyle='--', marker='s', 
                    label=f'SIC {sic} (Projected)')
    
    plt.title(f'{metric} by SIC Code: Original Data and Projections')
    plt.xlabel('Year')
    plt.ylabel(metric)
    plt.legend(bbox_to_anchor=(1.05, 1), loc='upper left')
    plt.tight_layout()
    plt.show()

# Example usage:
# Define your metrics and SIC codes
metrics = ['TOT_EMP', 'H_MEAN', 'A_MEAN', 'H_MEDIAN', 'A_MEDIAN',
          'H_PCT10', 'H_PCT25', 'H_PCT75', 'H_PCT90',
          'A_PCT10', 'A_PCT25', 'A_PCT75', 'A_PCT90']

sic_codes = ['07', '10', '15', '20', '40', '48', '49', '50', '52', '60', '72', '75', '79', '80', '82', '83', '89', '90']


# Filter out those years from the original df and ensure only 2002 and later
new_years = final_df[final_df['YEAR'] >= '2002']

# Filter out those years from the original df and ensure only before 2002
old_years = final_df[final_df['YEAR'] < '2002']

# Project all metrics
projected_df = project_all_metrics(old_years, metrics, sic_codes)

# Create visualizations for specific metrics
# For example, to visualize hourly mean wages:
plot_projections(old_years, projected_df, 'H_MEAN', sic_codes)

# To visualize total employment:
plot_projections(old_years, projected_df, 'TOT_EMP', sic_codes)

# Combining new_years and projected_df
final_df = pd.concat([new_years, projected_df], axis=0)

# Looking a the number of unique industry codes by year
print(final_df.groupby('YEAR')['INDUSTRY_CODE'].nunique())


# Check for duplicates in each dataset
print("Duplicates in wage data:")
print(final_df[final_df.duplicated(['YEAR', 'INDUSTRY_CODE'], keep=False)].sort_values(['YEAR', 'INDUSTRY_CODE']))

# If INDUSTRY_CODE is 99 replace with 92
final_df['INDUSTRY_CODE'] = final_df['INDUSTRY_CODE'].replace('99', '92')


# Saving the final df to the data folder as wage_industry_year.csv
final_df.to_csv(os.path.join(data_folder, "wages_industry_year.csv"), index=False)