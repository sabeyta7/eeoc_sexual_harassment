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


##=======================================================================##
##======================Setting all the directories======================##
##=======================================================================##

# Get current directory and construct paths
current_dir = os.getcwd()
project_root = os.path.dirname(current_dir)
data_folder = os.path.join(project_root, "data", "bls_data")

def parse_bls_pipe_table(raw_text, year):
    """Parse BLS pipe-delimited format (1995-1999)"""
    data = []
    started = False
    
    # Split into lines
    lines = raw_text.split('\n')
    
    for line in lines:
        # Skip until we find the first data row
        if 'Total, 16 years and over' in line:
            started = True
            
        if not started or not line.strip() or '_' in line:
            continue
            
        # Split by pipes and clean
        parts = [p.strip() for p in line.split('|')]
        parts = [p for p in parts if p.strip()]  # Remove empty parts
        
        # Need at least industry name + 4 numbers
        if len(parts) >= 5:
            try:
                # Get industry name (remove trailing dots)
                industry = re.sub(r'\.+$', '', parts[0]).strip()
                
                # Only process if we have a real industry name
                if industry and industry not in ['Industry', 'Occupation']:
                    # Get the values
                    total = int(float(parts[1].replace(',', ''))) if parts[1].strip() else None
                    women = float(parts[2]) if parts[2].strip() else None
                    black = float(parts[3]) if parts[3].strip() else None
                    hispanic = float(parts[4]) if parts[4].strip() else None
                    
                    # Calculate indentation level from original line
                    leading_spaces = len(line) - len(line.lstrip())
                    
                    data.append({
                        'Industry': industry,
                        'Total_Employed': total,
                        'Women_Pct': women,
                        'Black_Pct': black,
                        'Hispanic_Pct': hispanic,
                        'Year': year,
                        'Level': leading_spaces // 2
                    })
                    
            except (ValueError, IndexError) as e:
                print(f"Error on line: {line.strip()}")
                print(f"Error: {str(e)}")
                continue
    
    return pd.DataFrame(data)

# Read your file
with open('/Users/stephenabeyta/Projects/Sexual Harassment/data/bls_data/bls_1995_txt.txt', 'r') as f:
    text = f.read()
    
# Parse it
df_95 = parse_bls_pipe_table(text, 1995)

# Read your file
with open('/Users/stephenabeyta/Projects/Sexual Harassment/data/bls_data/bls_1996_txt.txt', 'r') as f:
    text = f.read()
    
# Parse it
df_96 = parse_bls_pipe_table(text, 1996)

# Read your file
with open('/Users/stephenabeyta/Projects/Sexual Harassment/data/bls_data/bls_1997_txt.txt', 'r') as f:
    text = f.read()
    
# Parse it
df_97 = parse_bls_pipe_table(text, 1997)

def process_content(lines, year):
    """Helper function to process the content once file is read"""
    data_rows = []
    current_occupation = ""
    
    for line in lines:
        # Skip empty lines and header-like lines
        if not line.strip() or 'HOUSEHOLD DATA' in line or 'ANNUAL AVERAGES' in line or 'Table' in line:
            continue
        
        # Clean the line
        line = line.strip()
        if not line:  # Skip empty lines after stripping
            continue
            
        # Print for debugging
        print(f"Processing line: {line[:100]}...")  # Print first 100 chars
            
        # Extract occupation and data
        if line[:65].strip():  # If there's content in the occupation column
            occupation = line[:65].strip()
            # Only update current_occupation if this isn't a note or header
            if not any(x in occupation.lower() for x in ['note:', 'occupation']):
                current_occupation = occupation
                print(f"Found occupation: {current_occupation}")
        else:
            occupation = current_occupation
            
        # Extract numeric values
        values = [v for v in line[65:].strip().split() if v and v.replace('.', '').replace(',', '').isdigit()]
        
        # Only process lines that have numeric data
        if values:
            try:
                total_employed = float(values[0].replace(',', ''))
                women_pct = float(values[1]) if len(values) > 1 else None
                black_pct = float(values[2]) if len(values) > 2 else None
                
                # Handle different format years
                if year >= 2003:
                    asian_pct = float(values[3]) if len(values) > 3 else None
                    hispanic_pct = float(values[4]) if len(values) > 4 else None
                else:
                    asian_pct = None
                    hispanic_pct = float(values[3]) if len(values) > 3 else None
                    
                data_row = {
                    'Year': year,
                    'Occupation': occupation,
                    'Total_Employed': total_employed,
                    'Women_Percent': women_pct,
                    'Black_Percent': black_pct,
                    'Asian_Percent': asian_pct,
                    'Hispanic_Percent': hispanic_pct
                }
                
                data_rows.append(data_row)
                print(f"Added data row: {data_row}")
                    
            except (ValueError, IndexError) as e:
                print(f"Error processing values {values}: {str(e)}")
                continue
    
    # Create DataFrame
    df = pd.DataFrame(data_rows)
    
    # Clean up occupation names
    if not df.empty:
        df['Occupation'] = df['Occupation'].str.strip().str.rstrip('.')
        df = df[df['Occupation'].str.strip() != '']
        
        # Print summary
        print(f"\nFound {len(df)} occupations")
        print("\nFirst few rows:")
        print(df.head())
    else:
        print("No data rows were created!")
    
    return df

def process_employment_data(year, file_path):
    """Process BLS employment data file into a pandas DataFrame"""
    # Convert to Path object
    file_path = Path(file_path)
    
    print(f"Reading file: {file_path}")
    print(f"File exists: {file_path.exists()}")
    
    # Read and process the file
    try:
        with open(file_path, 'r', encoding='utf-8') as file:
            lines = file.readlines()
            print(f"Read {len(lines)} lines from file")
            df = process_content(lines, year)
        
        print(f"Successfully processed {len(df)} occupations for year {year}")
        return df
        
    except Exception as e:
        print(f"Error reading file: {str(e)}")
        raise

df_98 = process_employment_data(1998, '/Users/stephenabeyta/Projects/Sexual Harassment/data/bls_data/bls_1998_txt.txt')
df_99 = process_employment_data(1999, '/Users/stephenabeyta/Projects/Sexual Harassment/data/bls_data/bls_1999_txt.txt')
df_02 = process_employment_data(2002, '/Users/stephenabeyta/Projects/Sexual Harassment/data/bls_data/bls_2002_txt.txt')
df_03 = process_employment_data(2003, '/Users/stephenabeyta/Projects/Sexual Harassment/data/bls_data/bls_2003_txt.txt')
df_04 = process_employment_data(2004, '/Users/stephenabeyta/Projects/Sexual Harassment/data/bls_data/bls_2004_txt.txt')
df_05 = process_employment_data(2005, '/Users/stephenabeyta/Projects/Sexual Harassment/data/bls_data/bls_2005_txt.txt')
df_06 = process_employment_data(2006, '/Users/stephenabeyta/Projects/Sexual Harassment/data/bls_data/bls_2006_txt.txt')
df_07 = process_employment_data(2007, '/Users/stephenabeyta/Projects/Sexual Harassment/data/bls_data/bls_2007_txt.txt')
df_08 = process_employment_data(2008, '/Users/stephenabeyta/Projects/Sexual Harassment/data/bls_data/bls_2008_txt.txt')
df_09 = process_employment_data(2009, '/Users/stephenabeyta/Projects/Sexual Harassment/data/bls_data/bls_2009_txt.txt')
df_10 = process_employment_data(2010, '/Users/stephenabeyta/Projects/Sexual Harassment/data/bls_data/bls_2010_txt.txt')


import pandas as pd
from pathlib import Path

def process_employment_data_html(year, file_path):
    """
    Process BLS employment data from 2012+ format files into a pandas DataFrame.
    Handles format where each value is on a separate line.
    """
    data_rows = []
    current_industry = None
    current_numbers = []
    data_started = False
    
    with open(file_path, 'r', encoding='utf-8') as file:
        lines = file.readlines()
        
        i = 0
        while i < len(lines):
            line = lines[i].strip()
            
            # Skip empty lines and headers
            if not line or any(x in line.lower() for x in ['household data', 'annual averages']):
                i += 1
                continue
            
            # Check for start of data
            if "Total, 16 years and over" in line:
                current_industry = line
                data_started = True
                i += 1
                continue
            
            if not data_started:
                i += 1
                continue
            
            # If line is a number (with possible commas)
            if line.replace(',', '').replace('.', '').replace('-', '').strip().isdigit() or line == '-':
                current_numbers.append(line)
                
                # If we have all 5 numbers for a row
                if len(current_numbers) == 5:
                    # Convert numbers, handling dashes
                    try:
                        total = float(current_numbers[0].replace(',', ''))
                        women = float(current_numbers[1]) if current_numbers[1] != '-' else None
                        black = float(current_numbers[2]) if current_numbers[2] != '-' else None
                        asian = float(current_numbers[3]) if current_numbers[3] != '-' else None
                        hispanic = float(current_numbers[4]) if current_numbers[4] != '-' else None
                        
                        if current_industry:  # Only add if we have an industry name
                            data_rows.append({
                                'Year': year,
                                'Industry': current_industry,
                                'Total_Employed': total,
                                'Women_Percent': women,
                                'Black_Percent': black,
                                'Asian_Percent': asian,
                                'Hispanic_Percent': hispanic
                            })
                    except (ValueError, IndexError) as e:
                        print(f"Error processing numbers {current_numbers} for {current_industry}: {e}")
                    
                    # Reset for next row
                    current_numbers = []
                
            else:
                # If not a number and we're in data section, it might be a new industry
                if not any(x in line.lower() for x in ['note:', 'footnote', 'last modified']):
                    current_industry = line
                current_numbers = []  # Reset number collection for new industry
            
            i += 1
    
    # Create DataFrame
    df = pd.DataFrame(data_rows)
    
    # Clean up industry names
    if not df.empty:
        df['Industry'] = df['Industry'].str.strip()
        
        # Remove rows that are just numbers or empty
        df = df[df['Industry'].str.strip() != '']
        df = df[~df['Industry'].str.match(r'^\d+\.?\d*$')]
        
        print(f"\nSuccessfully processed {len(df)} industries for year {year}")
        print("\nFirst few rows:")
        print(df.head())
    else:
        print("No data was processed!")
    
    return df
    
#df_11 = process_employment_data_html(2011, '/Users/stephenabeyta/Projects/Sexual Harassment/data/bls_data/bls_2011_html.txt')
df_12 = process_employment_data_html(2012, '/Users/stephenabeyta/Projects/Sexual Harassment/data/bls_data/bls_2012_html.txt')
df_13 = process_employment_data_html(2013, '/Users/stephenabeyta/Projects/Sexual Harassment/data/bls_data/bls_2013_html.txt')
df_14 = process_employment_data_html(2014, '/Users/stephenabeyta/Projects/Sexual Harassment/data/bls_data/bls_2014_html.txt')
df_15 = process_employment_data_html(2015, '/Users/stephenabeyta/Projects/Sexual Harassment/data/bls_data/bls_2015_html.txt')
#df_16 = process_employment_data_html(2016, '/Users/stephenabeyta/Projects/Sexual Harassment/data/bls_data/bls_2016_html.txt')


def read_bls_data(year, file_path):
    """
    Process BLS employment data from file.
    
    Args:
        year (int): The year of the data
        file_path (str): Path to the BLS data file
        
    Returns:
        pandas.DataFrame: Processed BLS employment data
    """
    data_rows = []
    data_started = False
    current_industry = None
    
    with open(file_path, 'r', encoding='utf-8') as f:
        for line in f:
            line = line.strip()
            
            # Skip empty lines
            if not line:
                continue
                
            # Check for start of data
            if "Total, 16 years and over" in line:
                data_started = True
                
            if not data_started:
                continue
                
            # Check for end of data section
            if "n.e.c. = not elsewhere classified" in line:
                break
            
            # Split by tabs
            parts = line.split('\t')
            
            if len(parts) == 1:
                # This is an industry name line
                current_industry = parts[0].strip()
            elif len(parts) > 1 and current_industry:
                # This is a data line
                try:
                    # Clean and convert the numbers
                    numbers = [p.strip() for p in parts if p.strip()]
                    if len(numbers) >= 5:
                        total = float(numbers[0].replace(',', '')) if numbers[0] != '-' else None
                        women = float(numbers[1]) if numbers[1] != '-' else None
                        black = float(numbers[2]) if numbers[2] != '-' else None
                        asian = float(numbers[3]) if numbers[3] != '-' else None
                        hispanic = float(numbers[4]) if numbers[4] != '-' else None
                        
                        data_rows.append({
                            'Year': year,
                            'Industry': current_industry,
                            'Total_Employed': total,
                            'Women_Percent': women,
                            'Black_Percent': black,
                            'Asian_Percent': asian,
                            'Hispanic_Percent': hispanic
                        })
                except (ValueError, IndexError) as e:
                    print(f"Error processing line: {line}\nError: {str(e)}")
                    continue

    # Create DataFrame
    df = pd.DataFrame(data_rows)
    
    if not df.empty:
        print(f"\nProcessed {len(df)} rows for year {year}")
        print("\nFirst few rows:")
        print(df.head())
        
        # Basic data validation
        print("\nColumn names:", df.columns.tolist())
        print("\nSummary statistics:")
        print(df.describe())
        
    return df
# Example usage:
path_2011 = '/Users/stephenabeyta/Projects/Sexual Harassment/data/bls_data/bls_2011_html.txt'
df_11 = read_bls_data(2011, path_2011)
df_16 = read_bls_data(2016, '/Users/stephenabeyta/Projects/Sexual Harassment/data/bls_data/bls_2016_html.txt')

# List of all the dataframes
dfs = [df_95, df_96, df_97, df_98, df_99, df_02, df_03, df_04, df_05, df_06, df_07, df_08, df_09, df_10, df_11, df_12, df_13, df_14, df_15, df_16]
# Printing the head of each dataframe
for df in dfs:
    print(df.head())
    
import pandas as pd
import numpy as np

def standardize_and_combine_bls_dfs(*dataframes):
    """
    Standardizes column names and types, then combines multiple BLS DataFrames.
    
    Args:
        *dataframes: Variable number of pandas DataFrames to combine
        
    Returns:
        pandas.DataFrame: Combined DataFrame with standardized columns
    """
    standardized_dfs = []
    
    for df in dataframes:
        # Create a copy to avoid modifying original
        df = df.copy()
        
        # Standardize column names
        column_mapping = {
            'Occupation': 'Industry',  # Some years use Occupation instead of Industry
            'Women_Pct': 'Women_Percent',
            'Black_Pct': 'Black_Percent',
            'Hispanic_Pct': 'Hispanic_Percent',
            'Total_employed': 'Total_Employed'
        }
        
        df = df.rename(columns=column_mapping)
        
        # Ensure all expected columns exist
        expected_columns = [
            'Year', 
            'Industry', 
            'Total_Employed',
            'Women_Percent',
            'Black_Percent',
            'Asian_Percent',
            'Hispanic_Percent'
        ]
        
        for col in expected_columns:
            if col not in df.columns:
                df[col] = np.nan
        
        # Convert numeric columns to float
        numeric_columns = [
            'Total_Employed',
            'Women_Percent',
            'Black_Percent',
            'Asian_Percent',
            'Hispanic_Percent'
        ]
        
        for col in numeric_columns:
            df[col] = pd.to_numeric(df[col], errors='coerce')
        
        # Ensure Year is integer
        df['Year'] = df['Year'].astype(int)
        
        # Ensure Industry is string
        df['Industry'] = df['Industry'].astype(str)
        
        standardized_dfs.append(df)
    
    # Combine all standardized DataFrames
    combined_df = pd.concat(standardized_dfs, ignore_index=True)
    
    # Sort by Year and Industry
    combined_df = combined_df.sort_values(['Year', 'Industry'])
    
    # Remove any duplicate rows
    combined_df = combined_df.drop_duplicates()
    
    # Reorder columns
    combined_df = combined_df[expected_columns]
    
    print("\nData Summary:")
    print(f"Combined {len(dataframes)} DataFrames")
    print(f"Final DataFrame shape: {combined_df.shape}")
    print("\nYears covered:", sorted(combined_df['Year'].unique()))
    print("Number of unique industries:", combined_df['Industry'].nunique())
    print("\nColumn data types:")
    print(combined_df.dtypes)
    
    # Check for any remaining inconsistencies
    print("\nMissing value counts:")
    print(combined_df.isnull().sum())
    
    return combined_df

# Example usage:
combined_df = standardize_and_combine_bls_dfs(df_95, df_96, df_97, df_98, df_99, df_02, df_03, df_04, df_05, df_06, df_07, df_08, df_09, df_10, df_11, df_12, df_13, df_14, df_15, df_16)

# Printing all the unique industries
print("\nUnique Industries:")
print(combined_df['Industry'].unique())

def standardize_industry_codes(df):
    """
    Maps industries to appropriate classification system based on year.
    Pre-2000:
    - Uses 6 major SOC (Standard Occupational Classification) groups
    2000 and later:
    - Uses 2-digit NAICS codes (12-13 sectors)
    """
    # SOC major groups for 1995-1999
    soc_mapping = {
        'Farming, forestry, and fishing': '07',
        'Extractive occupations': 10,
        'Construction trades': 15,
        'Handlers, equipment cleaners, helpers, and laborers': 15,
        'Mechanics and repairers': 75,
        'Transportation and material moving occupations': 40,
        'Service occupations, except private household and protective service': 72,
        'Service occupations, except private household and protective serv' : 72,
        'Service occupations': 90,
        'Protective service': 90,
        'Operators, fabricators, and laborers': 20,
        'Teachers, college and university': 82,
        'Teachers, except college and university': 82,
        'Counselors, educational and vocational': 82,
        'Librarians, archivists, and curators': 82,
        'Writers, artists, entertainers, and athletes': 79,
        'Health diagnosing occupations': 80,
        'Health assessment and treating occupations': 80,
        'Engineers, architects, and surveyors': 89,
        'Mathematical and computer scientists': 89,
        'Natural scientists': 89,
        'Social scientists and urban planners': 89,
        'Social, recreation, and religious workers': 83,
        'Lawyers and judges': 89,
        'Sales representatives, commodities, except retail': 50,
        'Sales workers, retail and personal services': 52,
        'Sales representatives, finance and business services': 60,
        'Supervisors and proprietors': 60,
        'Executive, administrative, and managerial': 83,
        'Health technologists and technicians': 80,
        'Engineering and science technicians': 89,
        'Science technicians': 89,
        'Technicians, except health, engineering, and science': 89,
        'Personal household': 89,
        # Adding administrative support categories:
        'Administrative support supervisors': 83,
        'Computer equipment operators': 89,
        'Secretaries, stenographers, and typists': 75,
        'Information clerks': 75,
        'Records processing clerks': 75,
        'Financial records processing': 60,
        'Communications equipment operators': 48,
        'Mail and message distributing': 43,
        'Material recording, scheduling, and distributing clerks': 40,
        'Adjusters and investigators': 60,
        'Miscellaneous administrative support': 75,
        'Plant and system operators': 49,
        'Precision production occupations': 20,
    }
    
    # NAICS codes for 2000+ (standard mappings only)
    naics_mapping = {
        'Agriculture, forestry, fishing, and hunting': '11',
        'Mining, quarrying, and oil and gas extraction': '21',
        'Mining': '21',
        'Utilities': '22',
        'Construction': '23',
        'Manufacturing': '31-33',
        'Wholesale trade': '42',
        'Retail trade': '44-45',
        'Transportation and warehousing': '48-49',
        'Information': '51',
        'Finance and insurance': '52',
        'Real estate and rental and leasing': '53',
        'Professional and technical services': '54',
        'Management, administrative, and waste services': '56',
        'Educational services': '61',
        'Health care and social assistance': '62',
        'Arts, entertainment, and recreation': '71',
        'Accommodation and food services': '72',
        'Other services': '81',
        'Public administration': '92'
    }

    def assign_code(row):
        year = row['Year']
        industry = row['Industry']
        
        if year == 2002:
            # Special mapping for 2002-specific industry names
            variant_2002_mapping = {
                'Agriculture': '11',
                'Forestry and fisheries': '11',
                'Mining': '21',
                'Utilities and sanitary services': '22',
                'Construction': '23',
                'Manufacturing': '31-33',
                'Wholesale trade': '42',
                'Retail trade': '44-45',
                'Transportation': '48-49',
                'Communications': '51',
                'Banking': '52',
                'Savings institutions, including credit unions': '52',
                'Security, commodity brokerage, and investment companies': '52',
                'Insurance': '52',
                'Real estate, including real estate-insurance offices': '53',
                'Other professional services': '54',
                'Educational services': '61',
                'Hospitals': '62',
                'Health services, except hospitals': '62',
                'Social services': '62',
                'Entertainment and recreation services': '71',
                'Personal services, except private household': '72',
                'Business, automobile, and repair services': '81',
                'Private households': '81',
                'Public administration': '92'
            }
            # First try 2002-specific mapping, if not found use general NAICS mapping
            return variant_2002_mapping.get(industry, naics_mapping.get(industry, None))
        elif year < 2000:
            # Use SOC mapping for years before 2000
            return soc_mapping.get(industry, None)
        elif year >= 2000:
            # Use standard NAICS mapping for years 2000 and later (except 2002)
            return naics_mapping.get(industry, None)
        else:
            # Return None if no mapping is found
            return None
    
    # Apply the mapping
    df['Industry_Code'] = df.apply(assign_code, axis=1)
    
    # Add column to indicate classification system
    df['Classification_System'] = df['Year'].map(lambda x: 'SOC' if x < 2000 else 'NAICS')
    
    # Remove rows where we couldn't assign a main category code
    df_clean = df[df['Industry_Code'].notna()].copy()
    
    df_clean = df[df['Industry_Code'].notna()].copy()
    
    # Add the reallocation code here, before the return
    if any(df_clean['Year'] < 2000):
        # Get utilities (SIC 49) data
        utilities_mask = (df_clean['Year'] < 2000) & (df_clean['Industry'] == 'Plant and system operators')
        utilities_data = df_clean[utilities_mask]
        
        # Get manufacturing (SIC 20) mask - includes both operators and precision production
        manufacturing_mask = (df_clean['Year'] < 2000) & (df_clean['Industry'].isin(['Operators, fabricators, and laborers', 'Precision production occupations']))
        
        if not utilities_data.empty and not df_clean[manufacturing_mask].empty:
            # Calculate absolute numbers for each demographic in utilities
            util_total = utilities_data['Total_Employed'].sum()
            util_women = (utilities_data['Total_Employed'] * utilities_data['Women_Percent']/100).sum()
            util_black = (utilities_data['Total_Employed'] * utilities_data['Black_Percent']/100).sum()
            util_asian = (utilities_data['Total_Employed'] * utilities_data['Asian_Percent']/100).sum()
            util_hispanic = (utilities_data['Total_Employed'] * utilities_data['Hispanic_Percent']/100).sum()
            
            # For each manufacturing row, subtract proportional share of utilities numbers
            for idx in df_clean[manufacturing_mask].index:
                row_share = df_clean.loc[idx, 'Total_Employed'] / df_clean[manufacturing_mask]['Total_Employed'].sum()
                
                # Calculate how much to subtract from this row
                subtract_total = util_total * row_share
                subtract_women = util_women * row_share
                subtract_black = util_black * row_share
                subtract_asian = util_asian * row_share
                subtract_hispanic = util_hispanic * row_share
                
                # Calculate new values
                new_total = df_clean.loc[idx, 'Total_Employed'] - subtract_total
                old_total = df_clean.loc[idx, 'Total_Employed']
                
                # Update totals and percentages
                df_clean.loc[idx, 'Total_Employed'] = new_total
                if new_total > 0:
                    df_clean.loc[idx, 'Women_Percent'] = ((old_total * df_clean.loc[idx, 'Women_Percent']/100 - subtract_women) / new_total * 100)
                    df_clean.loc[idx, 'Black_Percent'] = ((old_total * df_clean.loc[idx, 'Black_Percent']/100 - subtract_black) / new_total * 100)
                    df_clean.loc[idx, 'Asian_Percent'] = ((old_total * df_clean.loc[idx, 'Asian_Percent']/100 - subtract_asian) / new_total * 100)
                    df_clean.loc[idx, 'Hispanic_Percent'] = ((old_total * df_clean.loc[idx, 'Hispanic_Percent']/100 - subtract_hispanic) / new_total * 100)
    
    return df_clean

def analyze_codes(df):
    """Show detailed analysis of the mappings."""
    print("\nNumber of unique codes by year:")
    print(df.groupby('Year')['Industry_Code'].nunique())
    
    print("\n2002 Mappings:")
    print(df[df['Year'] == 2002][['Industry', 'Industry_Code']].sort_values('Industry_Code'))
    
    print("\n2010 Mappings (for comparison):")
    print(df[df['Year'] == 2010][['Industry', 'Industry_Code']].sort_values('Industry_Code'))
    
    

# Example usage:
df_clean = standardize_industry_codes(combined_df)
analyze_codes(df_clean)

# Define the columns that need weighted averages
percent_columns = ['Women_Percent', 'Black_Percent', 'Asian_Percent', 'Hispanic_Percent']

# Group by 'Industry' and 'Year' and aggregate
aggregated_df = df_clean.groupby(['Industry_Code', 'Year'], as_index=False).apply(
    lambda group: pd.Series({
        'Total_Employed': group['Total_Employed'].sum(),
        **{
            col: (group[col] * group['Total_Employed']).sum() / group['Total_Employed'].sum()
            for col in percent_columns
        },
        # Retain the first value for other columns
        'Industry_Code': group['Industry_Code'].iloc[0],
        'Classification_System': group['Classification_System'].iloc[0]
    })
).reset_index(drop=True)

# View the result
print(aggregated_df)

# First get our Holt forecasts (using the code we already had)
soc_data = aggregated_df[aggregated_df['Classification_System'] == 'SOC'].copy()

forecasts = []
numeric_cols = ['Total_Employed', 'Women_Percent', 'Black_Percent', 'Hispanic_Percent']

for industry in soc_data['Industry_Code'].unique():
    industry_data = soc_data[soc_data['Industry_Code'] == industry].sort_values('Year')
    
    if len(industry_data) >= 2:  
        industry_code = industry_data['Industry_Code'].iloc[0]
        
        for col in numeric_cols:
            model = Holt(industry_data[col], initialization_method="estimated")
            fitted_model = model.fit(smoothing_level=0.8, smoothing_slope=0.4)
            forecast = fitted_model.forecast(2)
            
            for year, value in zip([2000, 2001], forecast.values):
                forecasts.append({
                    'Year': year,
                    'Industry_Code': industry_code,
                    'Classification_System': 'SOC',
                    col: value
                })

forecast_df = pd.DataFrame(forecasts)
forecast_df = forecast_df.pivot_table(
    index=['Year', 'Industry_Code', 'Classification_System'],
    values=numeric_cols
).reset_index()

# Now create the complete time series
# 1. Get original data before 2000 (SOC)
pre_2000 = aggregated_df[aggregated_df['Year'] < 2000].copy()

# 2. Get original data after 2001 (NAICS)
post_2001 = aggregated_df[aggregated_df['Year'] > 2001].copy()

# 3. Combine all periods
complete_series = pd.concat([pre_2000, forecast_df, post_2001]).sort_values(['Year', 'Industry_Code'])

# If you want to verify the merge worked correctly:
print("Years in complete series:", sorted(complete_series['Year'].unique()))
print("\nNumber of rows by year:")
print(complete_series.groupby('Year').size())

# Saving as combined_bls_data.csv in the data folder
combined_file_path = os.path.join(data_folder, 'combined_bls_data.csv')
complete_series.to_csv(combined_file_path, index=False)
