import pandas as pd

df = pd.read_csv("gene.csv")
print(df.columns)

#speechiness,acousticness,instrumentalness,liveness,tempo,n_beats,n_bars
"danceability", "liveness", "energy", "acousticness", "loudness"
df = df.drop(columns=['row ID'], axis=1)

df.to_csv("geneNoStringCols.csv", index=False)