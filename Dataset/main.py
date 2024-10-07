import pandas as pd
import json

columns = [
    "Id", "Income", "Age", "Experience", "Married/Single", "House_Ownership",
    "Car_Ownership", "Profession", "CITY", "STATE",
    "CURRENT_JOB_YRS", "CURRENT_HOUSE_YRS", "Risk_Flag"
]

with open('loan_approval_dataset.json') as json_file:
    data = json.load(json_file)

df = pd.DataFrame(data, columns=columns)

df.to_csv('data.csv', index=False)

print("Complete!")