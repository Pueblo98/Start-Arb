import yfinance as yf
import pandas as pd
from datetime import datetime, timedelta

# Define the ETF tickers
tickers = ['KROP', 'PBJ']  # Added PBJ back since you mentioned both in your initial request

# Calculate date range (4 years of data - 1460 days)
end_date = datetime.now().strftime('%Y-%m-%d')
start_date = (datetime.now() - timedelta(days=1460)).strftime('%Y-%m-%d')

# Download data for all tickers at once (more efficient)
data = yf.download(tickers, start=start_date, end=end_date)

# Create separate dataframes for each ticker
dfs = {}
for ticker in tickers:
    # For a single ticker, data structure is different
    if len(tickers) == 1:
        df = pd.DataFrame({
            'Date': data.index,
            'Open': data['Open'],
            'High': data['High'],
            'Low': data['Low'],
            'Close': data['Close']
        })
    else:
        # For multiple tickers, we need to extract using multi-level columns
        df = pd.DataFrame({
            'Date': data.index,
            'Open': data[('Open', ticker)],
            'High': data[('High', ticker)],
            'Low': data[('Low', ticker)],
            'Close': data[('Close', ticker)]
        })
    
    # Save to CSV for each ticker
    filename = f"{ticker}_Data_{start_date}_to_{end_date}.csv"
    df.to_csv(filename, index=False)
    print(f"Data saved to {filename}")
    
    # Store in dictionary for reference
    dfs[ticker] = df
    
    # Display the first few rows
    print(f"\nSample of {ticker} data:")
    print(df.head())

# You can also save them to a combined file if needed
combined_data = pd.DataFrame()
for ticker in tickers:
    temp_df = dfs[ticker].copy()
    temp_df['Ticker'] = ticker
    combined_data = pd.concat([combined_data, temp_df])
    
combined_filename = f"Combined_ETF_Data_{start_date}_to_{end_date}.csv"
combined_data.to_csv(combined_filename, index=False)
print(f"\nCombined data saved to {combined_filename}")