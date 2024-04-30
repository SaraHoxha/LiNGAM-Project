import pandas as pd

df = pd.read_csv("statsDfNormalized.csv")


#speechiness,acousticness,instrumentalness,liveness,tempo,n_beats,n_bars
"danceability", "liveness", "energy", "acousticness", "loudness"
df = df.drop(columns=["speechiness","instrumentalness","valence","tempo","n_beats","n_bars"], axis=1)

df.to_csv("statsDfNormalizedWithLessColumns.csv", index=False)