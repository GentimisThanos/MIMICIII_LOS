# -*- coding: utf-8 -*-
"""
Created on Fri Mar 17 12:52:41 2023

@author: agentimis1
"""

#%%============== Libraries
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense
import numpy as np
from sklearn.metrics import accuracy_score
from sklearn.preprocessing import StandardScaler

#from tkinter import filedialog
#root = tk.Tk()
#root.withdraw()

#%%============== Load dataset
#root.attributes("-topmost", True)
#npath=filedialog.askdirectory(parent=root,initialdir="//",title='Pick the directory containing the project')
npath="G:/My Drive/Collaborations/04_Old/Length_Of_Stay/Codes/Python_Scripts"
dataset=pd.read_csv(npath+"\\Data\\Clean_LOS_2.csv")

#%%============== Setting Parameters
total_dpoints = 30000
max_days = 100
size = 0.2
cutoffday = 5
iterations=2
ephs=10
btc_size=30
#%%============= Data Preprocessing

#Filter for entries that have a LOS less than the maximum days specified
dataset = dataset[dataset.full_los < max_days]
#keep only full rows
dataset = dataset.dropna(axis=0)
#Esure that the total number of points does not exceed the maximum dataframe entries
tdpts = min(total_dpoints, dataset.shape[0])
#samples the dataset to specified number of datapoints
dataset = dataset.sample(n=tdpts)

#%%Create a binary split for length of stay beased on cuttofday
dataset['LOSBin'] = dataset['full_los'].apply(lambda x: 0 if x < cutoffday else 1)

#%%============== Convert target variable to binary
encoder = LabelEncoder()
dataset['LOSBin'] = encoder.fit_transform(dataset['LOSBin'])

#%%============== Remove Length of stay 
dataset.drop(columns=["full_los","los"], inplace=True)
#%%================ Define a function to create the model 
def build_model(num_features):
    model = Sequential()
    model.add(Dense(64, input_dim=num_features, activation='relu'))
    model.add(Dense(64, input_dim=num_features, activation='relu'))
    model.add(Dense(1, activation='sigmoid'))
    model.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])
    return model

#%%================ Define a function to train the model and return the accuracvy
def train_model(X_train, y_train, X_test, y_test):
    num_features = X_train.shape[1]
    model = build_model(num_features)
    model.fit(X_train, y_train, epochs=ephs, batch_size=btc_size, validation_data=(X_test, y_test))
    y_pred = model.predict(X_test)
    y_pred = np.round(y_pred)
    acc = accuracy_score(y_test, y_pred)
    return acc

#%%=============== Create an empty list to save the accuracies 
accuracies = []

#%%=============== Train the model and save the accuracies of multiple iterations
for i in range(iterations):
    X = dataset.drop('LOSBin', axis=1)
    y = dataset['LOSBin']
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=size)
    scaler = StandardScaler()
    X_train = scaler.fit_transform(X_train)
    X_test = scaler.transform(X_test)
    acc = train_model(X_train, y_train, X_test, y_test)
    accuracies.append(acc)