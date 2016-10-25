import pandas as pd
import numpy as np

path = "../input/Train/"
s1 = pd.read_csv(path+"model_10.csv")
s2 = pd.read_csv(path+"sub35.csv")

s1 = s1.merge(s2, on=['Patient_ID','Health_Camp_ID'], how='left')
print s1.columns
print np.corrcoef(s1.Outcome_x.values, s1.Outcome_y.values)

s1["Outcome"] = (0.48*s1.Outcome_x.values + 0.52*s1.Outcome_y.values)
s1.drop(["Outcome_x", "Outcome_y"], axis=1, inplace=True)
s1.to_csv("final.csv", index=False)

