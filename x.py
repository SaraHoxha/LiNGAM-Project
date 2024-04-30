import pandas as pd

df = pd.read_csv("statsDfNormalized.csv")

df.drop(columns=["valence","tempo","n_beats","n_bars"], axis=1)

df.to_csv("statsDfNormalizedWithLessColumns.csv", index=False)