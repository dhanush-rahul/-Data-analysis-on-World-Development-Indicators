import pandas as pd
import os


os.chdir('C:\Graduate-Studies\R')

df = pd.DataFrame([])


for root, dirs, files in os.walk("."):
   for name in files:
       df_temp = pd.read_csv(name)
       df = pd.concat([df,df_temp])

# Save df to a CSV file
df.to_csv('all-csv-files.csv')

