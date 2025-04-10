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
data_folder = os.path.join(project_root, "data", "union_data")

def map_to_sic(industry_name):
    """Map the major categories to SIC codes for 1995-2001 data"""
    industry = industry_name.strip().upper()
    
    sic_mapping = {
        'AGRICULTURE': '07',
        'FORESTRY AND FISHERIES': '07',
        'MINING': 10,
        'CONSTRUCTION': 15,
        'MANUFACTURING, NONDURABLE GOODS': 20,
        'MANUFACTURING, DURABLE GOODS': 20,
        'TRANSPORTATION': 40,
        'COMMUNICATIONS': 48,
        'UTILITIES AND SANITARY SERVICES': 49,
        'WHOLESALE TRADE': 50,
        'RETAIL TRADE': 52,
        'FINANCE, INSURANCE, AND REAL ESTATE': 60,
        'BUSINESS AND REPAIR SERVICES': 75,
        'PERSONAL SERVICES': 72,
        'ENTERTAINMENT AND RECREATION SERVICES': 79,
        'MEDICAL SERVICES, EXCEPT HOSPITALS': 80,
        'HOSPITALS': 80,
        'OTHER PROFESSIONAL SERVICES': 89,
        'EDUCATIONAL SERVICES': 82,
        'SOCIAL SERVICES': 83,
        'PUBLIC ADMINISTRATION': 90
    }

 
    return sic_mapping.get(industry)

def process_year(year):
    # Construct file path
    filename = os.path.join(data_folder, f'ind_{year}.xlsx')
    # Read Excel file, skipping the first two rows (title and blank row)
    df = pd.read_excel(filename, skiprows=2)
    # Add year column
    df['year'] = year
    # Get major categories (rows where CIC is null but Industry exists)
    major_categories = df[pd.isna(df['CIC']) & pd.notna(df['Industry'])].copy()
    
    if year < 2002:
        # For 1995-2001: Map to SIC codes
        major_categories['industry_code'] = major_categories['Industry'].apply(map_to_sic)
        major_categories['classification'] = 'SIC'
    else:
        # For 2002-2016: Keep original NAICS categories but aggregate certain categories
        major_categories['classification'] = 'NAICS'
        major_categories['industry_code'] = major_categories['Industry']
        
        # Define categories to aggregate
        manufacturing_categories = [
            'MANUFACTURING, DURABLE GOODS',
            'MANUFACTURING, NONDURABLE GOODS',
            'DURABLE GOODS MANUFACTURING',
            'NONDURABLE GOODS MANUFACTURING'
        ]
        
        services_categories = [
            # Pre-2002 names
            'SOCIAL SERVICES',
            'MEDICAL SERVICES, EXCEPT HOSPITALS',
            'HOSPITALS',
            # Post-2002 names
            'HEALTH CARE & SOCIAL ASSISTANCE',
            'MEDICAL SERVICES EXCEPT HOSPITALS'  # Note: no comma in some versions
        ]

        agriculture_categories = [
            'AGRICULTURE',
            'FORESTRY AND FISHERIES'
        ]
        
        
        # Create category masks
        manufacturing_mask = major_categories['Industry'].isin(manufacturing_categories)
        services_mask = major_categories['Industry'].isin(services_categories)
        agriculture_mask = major_categories['Industry'].isin(agriculture_categories)
        
        # Split into different category groups
        manufacturing = major_categories[manufacturing_mask]
        services = major_categories[services_mask]
        agriculture = major_categories[agriculture_mask]
        other = major_categories[~(manufacturing_mask | services_mask | agriculture_mask)]
        
        
        # Function to aggregate a category group
        def aggregate_category(df, category_name):
            agg = df.groupby(['year', 'classification']).agg({
                'Employment (in 1000s)': 'sum',
                'Members (in 1000s)': 'sum',
                'Covered (in 1000s)': 'sum'
            }).reset_index()
            
            agg['industry_code'] = category_name
            
            # Recalculate percentages
            agg['% Mem'] = (agg['Members (in 1000s)'] / 
                          agg['Employment (in 1000s)']) * 100
            agg['% Cov'] = (agg['Covered (in 1000s)'] / 
                          agg['Employment (in 1000s)']) * 100
            return agg
        
        # Aggregate all category groups
        manufacturing_agg = aggregate_category(manufacturing, 'MANUFACTURING')
        services_agg = aggregate_category(services, 'HEALTH CARE & SOCIAL ASSISTANCE')  # Changed from 'MEDICAL SERVICES, EXCEPT HOSPITALS'        
        agriculture_agg = aggregate_category(agriculture, 'AGRICULTURE, FORESTRY, FISHING, & HUNTING')
        # Add these debug prints
        print("Services mask matches:", major_categories[services_mask]['Industry'].tolist())
        print("Other category includes:", other['Industry'].tolist())
        
        # Combine all data
        result = pd.concat([
            other[['year', 'industry_code', 'classification',
                  'Employment (in 1000s)', 'Members (in 1000s)',
                  'Covered (in 1000s)', '% Mem', '% Cov']],
            manufacturing_agg,
            services_agg,
            agriculture_agg
        ])
        
        return result

    # Group by year, industry code, and classification (this handles pre-2002 case)
    result = major_categories.groupby(['year', 'industry_code', 'classification']).agg({
        'Employment (in 1000s)': 'sum',
        'Members (in 1000s)': 'sum',
        'Covered (in 1000s)': 'sum'
    }).reset_index()
    
    # Recalculate percentages after aggregation
    result['% Mem'] = (result['Members (in 1000s)'] / result['Employment (in 1000s)']) * 100
    result['% Cov'] = (result['Covered (in 1000s)'] / result['Employment (in 1000s)']) * 100
    
    return result

# List to store ALL results
all_results = []

# Create output directory if it doesn't exist
output_folder = os.path.join(data_folder, 'processed')
os.makedirs(output_folder, exist_ok=True)

# Process each year
for year in range(1995, 2017):
    try:
        result = process_year(year)
        all_results.append(result)
        print(f"Successfully processed year {year}")
    except FileNotFoundError:
        print(f"File not found for year {year}")
        continue
    except Exception as e:
        print(f"Error processing year {year}: {str(e)}")
        continue

# Combine ALL results into ONE GODDAMN FILE
if all_results:
    combined_df = pd.concat(all_results, ignore_index=True)
    combined_df = combined_df.sort_values(['year', 'industry_code'])



# THE MAPPING
industry_to_naics = {
    # Exact 2002 finance/real estate categories with their spaces
    'Banking ': 52,
    'Credit agencies, n.e.c. ': 52,
    'Insurance ': 52, 
    'Real estate, including real estate-insurance offices ': 53,
    'Savings institutions, including credit unions ': 52,
    'Security, commodity brokerage, and investment companies ': 52,
    'Newspaper publishing and printing ': 51,
    'Printing, publishing, and allied industries, except newspapers ': 51,
    'Computer and data processing services ': 51,
    'CONSTRUCTION ': 23,  # Matches exactly what's in 2002
    'HOSPITALS ': 62, 
    'Business services, n.e.c. ': 55,
    'Management and public relations services ': 55,
    'Services to dwellings and other buildings ': 55,
    'Landscape and horticultural services ': 55,
    
    
    
    
    # Rest of the mappings
    'AGRICULTURE': 11,
    'BUSINESS AND REPAIR SERVICES': 56,
    'COMMUNICATIONS': 51,
    'EDUCATIONAL SERVICES': 61,
    'ENTERTAINMENT AND RECREATION SERVICES': 71,
    'MANUFACTURING': '31-33',
    'MINING': 21,
    'OTHER PROFESSIONAL SERVICES': 54,
    'PERSONAL SERVICES': 81,
    'PUBLIC ADMINISTRATION': 92,
    'RETAIL TRADE': '44-45',
    'TRANSPORTATION': '48-49',
    'UTILITIES AND SANITARY SERVICES': 22,
    'WHOLESALE TRADE': 42,
    'ACCOMODATION & FOOD SERVICES': 72,
    'AGRICULTURE, FORESTRY, FISHING, & HUNTING': 11,
    'ARTS, ENTERTAINMENT, & RECREATION': 71,
    'DURABLE GOODS MANUFACTURING': '31-33',
    'FINANCE & INSURANCE': 52,
    'HEALTH CARE & SOCIAL ASSISTANCE': 62,
    'INFORMATION': 51,
    'MANAGEMENT & RELATED SERVICES': 55,
    'NONDURABLE GOODS MANUFACTURING': '31-33',
    'OTHER SERVICES, EXC. PUBLIC ADMIN.': 81,
    'PROF., SCIENTIFIC, TECHNICAL SERVICES': 54,
    'REAL ESTATE & RENTAL & LEASING': 53,
    'TRANSPORTATION & WAREHOUSING': '48-49',
    'UTILITIES': 22,
    'CONSTRUCTION': 23,  # Keep this for other years
}
old_df = combined_df.copy()

def map_naics_code(row):
    """Map the industry code to NAICS code if classification is NAICS"""
    if row['classification'] == 'NAICS':
        # Don't strip - use exact string matching
        return industry_to_naics.get(str(row['industry_code']))
    return row['industry_code']

# In your code:
combined_df['industry_code'] = combined_df.apply(map_naics_code, axis=1)

# Looking for duplicate year industry_code pairs
duplicates = combined_df[combined_df.duplicated(['year', 'industry_code'], keep=False)].sort_values(['year', 'industry_code'])
print(duplicates)

# Looking at the number of unique industry codes for each year
unique_codes = combined_df.groupby('year')['industry_code'].nunique()
print(unique_codes)

# Just extract 2002 from old_df (before mapping)
df_2002 = old_df[old_df['year'] == 2002].copy()

def recalculate_2002(df):
    """Handle all 2002-specific subtractions and recalculations"""
    # Get the rows we need to adjust
    publishing_rows = df[df['industry_code'].isin([
        'Newspaper publishing and printing ',
        'Printing, publishing, and allied industries, except newspapers '
    ])].copy()
    
    business_services_rows = df[df['industry_code'].isin(['Business services, n.e.c. ', 'Computer and data processing services '])].copy()
    management = df[df['industry_code'] == 'Management and public relations services '].copy()
    dwellings = df[df['industry_code'] == 'Services to dwellings and other buildings '].copy()
    landscape = df[df['industry_code'] == 'Landscape and horticultural services '].copy()
    
    # Get the rows we need to subtract from
    manufacturing_mask = df['industry_code'] == 'MANUFACTURING'
    business_repair_mask = df['industry_code'] == 'BUSINESS AND REPAIR SERVICES'
    social_services_mask = df['industry_code'] == 'HEALTH CARE & SOCIAL ASSISTANCE'
    agriculture_mask = df['industry_code'] == 'AGRICULTURE'
    
    # Subtract values and recalculate percentages
    for mask in [manufacturing_mask, business_repair_mask, social_services_mask, agriculture_mask]:
        if any(mask):
            # Subtract appropriate values based on which category
            if mask.equals(manufacturing_mask):
                subtract_rows = publishing_rows
            elif mask.equals(business_repair_mask):
                subtract_rows = pd.concat([business_services_rows, dwellings])
            elif mask.equals(social_services_mask):
                subtract_rows = management
            else:  # agriculture
                subtract_rows = landscape
                
            df.loc[mask, 'Employment (in 1000s)'] -= subtract_rows['Employment (in 1000s)'].sum()
            df.loc[mask, 'Members (in 1000s)'] -= subtract_rows['Members (in 1000s)'].sum()
            df.loc[mask, 'Covered (in 1000s)'] -= subtract_rows['Covered (in 1000s)'].sum()
            
            # Recalculate percentages
            emp = df.loc[mask, 'Employment (in 1000s)']
            mem = df.loc[mask, 'Members (in 1000s)']
            cov = df.loc[mask, 'Covered (in 1000s)']
            
            df.loc[mask, '% Mem'] = (mem / emp * 100)
            df.loc[mask, '% Cov'] = (cov / emp * 100)
    
    return df

# Recalculate 2002
df_2002_adjusted = recalculate_2002(df_2002)

# Map the adjusted 2002 data to NAICS codes
df_2002_adjusted['industry_code'] = df_2002_adjusted.apply(map_naics_code, axis=1)

# NOW GROUPING BY INDUSTRY CODE
# Simple groupby aggregation
df_2002_agg = df_2002_adjusted.groupby(['year', 'industry_code', 'classification']).agg({
    'Employment (in 1000s)': 'sum',
    'Members (in 1000s)': 'sum',
    'Covered (in 1000s)': 'sum'
}).reset_index()

# Recalculate percentages
df_2002_agg['% Mem'] = (df_2002_agg['Members (in 1000s)'] / df_2002_agg['Employment (in 1000s)']) * 100
df_2002_agg['% Cov'] = (df_2002_agg['Covered (in 1000s)'] / df_2002_agg['Employment (in 1000s)']) * 100

# Replace 2002 in combined_df with our adjusted version
final_df = pd.concat([
    combined_df[combined_df['year'] != 2002],
    df_2002_agg
])


# Making a grouped version of the original to compare
combined_df_agg = combined_df.groupby(['year', 'industry_code', 'classification']).agg({
    'Employment (in 1000s)': 'sum',
    'Members (in 1000s)': 'sum',
    'Covered (in 1000s)': 'sum'
}).reset_index()

# Sort final result
final_df = final_df.sort_values(['year', 'industry_code'])
print(combined_df_agg[combined_df_agg['year'] == 2002])
print(final_df[final_df['year'] == 2002])


# Saving the combined data to a file as combined_union_industry.csv
final_df.to_csv(os.path.join(data_folder, 'combined_union_industry.csv'), index=False)


# Printing unique industry codes for the year 2002
print("Unique industry codes for the year 2002:")
print(old_df[old_df['year'] == 2002]['industry_code'].unique())
print(combined_df[combined_df['year'] == 2002]['industry_code'].unique())

# Seering unique industry codes for SIC and NAICS
print("Unique industry codes for SIC:")
print(combined_df[combined_df['classification'] == 'SIC']['industry_code'].unique())
print

print("Unique industry codes for NAICS:")
print(combined_df[combined_df['classification'] == 'NAICS']['industry_code'].unique())