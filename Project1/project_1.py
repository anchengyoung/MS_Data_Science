#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Oct 23 16:49:13 2021

@author: anchengyoung
"""

import numpy as np
from scipy import stats
import pandas as pd

path = 'Documents/atNYU/Grad/Intro to DS/project/Project1/movieReplicationSet.csv'
data = np.genfromtxt(path, delimiter = ',',skip_header=1)
df = pd.read_csv(path,skipinitialspace=True)
rambo = 'Rambo: First Blood Part II'
df = df.rename(columns={rambo: 'Rambo: First Blood Part II (1985)'})

#%%
movies = data[:,:400]

#%% 1

counts = []
ratings = []

for i in range(len(movies[0])):
    movie = movies[:,i]
    movie = movie[~np.isnan(movie)]
    counts.append(len(movie))
    ratings.append(np.median(movie))

popular = np.column_stack([counts,ratings])
popular = popular[np.argsort(popular[:, 0])][::-1][:len(popular)]

high_pop, low_pop = np.split(popular[:, 1], 2)

u,p = stats.mannwhitneyu(high_pop, low_pop, alternative='greater')
print('u:',u,'p:',p)

#%% 2

df_movies = df.iloc[:, :400]
titles = list(df_movies.columns)

years = []
for i in range(len(titles)):
    year = int(titles[i][-5:-1])
    years.append(year)

release = np.column_stack([years,ratings])
release = release[np.argsort(release[:, 0])][::-1][:len(release)]

new_movie, old_movie = np.split(release[:, 1], 2)

u,p = stats.mannwhitneyu(new_movie, old_movie)
print('u:',u,'p:',p)

#%% 3

shrek = np.array(df[['Shrek (2001)']])
gender = np.array(df.iloc[:,474])
shrek = np.column_stack([shrek,gender])

female = shrek[shrek[:, 1] == 1][:, 0]
female = female[~np.isnan(female)]
male = shrek[shrek[:, 1] == 2][:, 0]
male = male[~np.isnan(male)]

u,p = stats.mannwhitneyu(female, male)
print('u:',u,'p:',p)

#%% 4

count = 0
for i in range(len(movies[0])):
    movie = movies[:,i]
    movie = np.column_stack([movie,gender])
    female = movie[movie[:, 1] == 1][:, 0]
    female = female[~np.isnan(female)]
    male = movie[movie[:, 1] == 2][:, 0]
    male = male[~np.isnan(male)]
    u,p = stats.mannwhitneyu(female, male)
    if p <= 0.005:
        count +=1

print(count/len(movies[0]))

#%% 5

lion_king = np.array(df[['The Lion King (1994)']])
only_child = np.array(df.iloc[:,475])
lion_king = np.column_stack([lion_king,only_child])

yes_only = lion_king[lion_king[:, 1] == 1][:, 0]
yes_only = yes_only[~np.isnan(yes_only)]
no_only = lion_king[lion_king[:, 1] == 0][:, 0]
no_only = no_only[~np.isnan(no_only)]

u,p = stats.mannwhitneyu(yes_only, no_only, alternative='greater')
print('u:',u,'p:',p)

#%% 6

count = 0
for i in range(len(movies[0])):
    movie = movies[:,i]
    movie = np.column_stack([movie,only_child])
    yes_only = movie[movie[:, 1] == 1][:, 0]
    yes_only = yes_only[~np.isnan(yes_only)]
    no_only = movie[movie[:, 1] == 0][:, 0]
    no_only = no_only[~np.isnan(no_only)]
    u,p = stats.mannwhitneyu(yes_only, no_only)
    if p <= 0.005:
        count +=1

print(count/len(movies[0]))

#%% 7

wolf = np.array(df[['The Wolf of Wall Street (2013)']])
social = np.array(df.iloc[:,476])
wolf = np.column_stack([wolf,social])

socially = wolf[wolf[:, 1] == 0][:, 0]
socially = socially[~np.isnan(socially)]
alone = wolf[wolf[:, 1] == 1][:, 0]
alone = alone[~np.isnan(alone)]

u,p = stats.mannwhitneyu(socially, alone, alternative='greater')
print('u:',u,'p:',p)

#%% 8

count = 0
for i in range(len(movies[0])):
    movie = movies[:,i]
    movie = np.column_stack([movie,social])
    socially = movie[movie[:, 1] == 0][:, 0]
    socially = socially[~np.isnan(socially)]
    alone = movie[movie[:, 1] == 1][:, 0]
    alone = alone[~np.isnan(alone)]
    u,p = stats.mannwhitneyu(socially, alone, alternative='greater')
    if p <= 0.005:
        count +=1

print(count/len(movies[0]))

#%% 9

home_alone = np.array(df[['Home Alone (1990)']])
home_alone = home_alone[~np.isnan(home_alone)]
find_nemo = np.array(df[['Finding Nemo (2003)']])
find_nemo = find_nemo[~np.isnan(find_nemo)]

k, p = stats.kstest(home_alone, find_nemo)
print('k:',k,'p:',p)

#%% 10

franchises = ['Star Wars', 'Harry Potter', 'The Matrix', 'Indiana Jones', 'Jurassic Park', 'Pirates of the Caribbean', 'Toy Story', 'Batman']

for franchise in franchises:
    data = np.array(df.loc[:,df.columns.str.contains(franchise)])
    episodes = []
    for episode in range(len(data[0])):
        movie = data[:,episode]
        movie = movie[~np.isnan(movie)]
        episodes.append(movie)
    h,p = stats.kruskal(*episodes)
    print(franchise)
    print('h:',h,'p:',p)


