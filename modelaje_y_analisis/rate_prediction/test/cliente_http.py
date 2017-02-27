import os.path
import time
import random
import requests, json
import pandas as pd

while not os.path.exists('EX2_DATA_BASE.xlsx'):
    time.sleep(
        3 * random.random());  # Sleeping less than 3 seconds before going to Dropbox - avoid too many students at once.
    if not os.path.exists('EX2_DATA_BASE.xlsx'):
        print "DOWLOADING FILE EX2_DATA_BASE.xlsx FROM DROPBOX BECAUSE LOCAL FILE DOES NOT EXIST!"
        #        csvfile = urllib2.urlopen("https://dl.dropboxusercontent.com/u/28535341/EX2_DEV_DATA.xlsx")
        resp = requests.get("https://dl.dropboxusercontent.com/u/28535341/EX2_DATA_BASE.xlsx")
        output = open('EX2_DATA_BASE.xlsx', 'wb')
        output.write(resp.content)
        output.close()
# DOWLOADING FILE FROM DROPBOX FIRST TIME


print "LOADING DATASETS..."
# df = pd.read_csv("EX2_DEV_DATA.xlsx",sep=";") #DEV-SAMPLE
df = pd.read_excel(open('EX2_DATA_BASE.xlsx', 'rb'), sheetname='DATA')
df_dictionary = pd.read_excel(open('EX2_DATA_BASE.xlsx', 'rb'), sheetname='DICTIONARY')
df = df.fillna(0)

print "CALLING PREDICTION 0..."
list_of_inputs_for_model0 = df_dictionary[df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT0']['FIELD_NAME'].tolist()
list_of_inputs_for_model1 = \
df_dictionary[(df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT0') | (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT1')][
    'FIELD_NAME'].tolist()
list_of_inputs_for_model2 = df_dictionary[
    (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT0') | (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT1') | (
    df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT2')]['FIELD_NAME'].tolist()
list_of_inputs_for_model3 = df_dictionary[
    (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT0') | (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT1') | (
    df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT2') | (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT3')][
    'FIELD_NAME'].tolist()
list_of_inputs_for_model4 = df_dictionary[
    (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT0') | (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT1') | (
    df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT2') | (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT3') | (
    df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT4')]['FIELD_NAME'].tolist()
df0 = df[list_of_inputs_for_model0]
df1 = df[list_of_inputs_for_model1]
df2 = df[list_of_inputs_for_model2]
df3 = df[list_of_inputs_for_model3]
df4 = df[list_of_inputs_for_model4]

#PYTHONANYWHERE_URL = 'http://amarino.pythonanywhere.com'
PYTHONANYWHERE_URL= 'http://localhost:8080'

for i in range(len(df)):
    print i
    df_i = df0.iloc[i]
    res = requests.post(PYTHONANYWHERE_URL + '/api/predict0/1234', json=df_i.to_json(orient='split'))
    if res.ok:
        print "Company:", df["TICKER"].iloc[i], "; API 0 prediction:", res.json(), "; Real= ", \
        df["BB_1YR_DEFAULT_PROB_CQ4_2015"].iloc[i]

    df_i = df1.iloc[i]
    res = requests.post(PYTHONANYWHERE_URL + '/api/predict1/1234', json=df_i.to_json(orient='split'))
    if res.ok:
        print "Company:", df["TICKER"].iloc[i], "; API 1 prediction:", res.json(), "; Real= ", \
        df["BB_1YR_DEFAULT_PROB_CQ4_2015"].iloc[i]

    df_i = df2.iloc[i]
    res = requests.post(PYTHONANYWHERE_URL + '/api/predict2/1234', json=df_i.to_json(orient='split'))
    if res.ok:
        print "Company:", df["TICKER"].iloc[i], "; API 2 prediction:", res.json(), "; Real= ", \
        df["BB_1YR_DEFAULT_PROB_CQ4_2015"].iloc[i]

    df_i = df3.iloc[i]
    res = requests.post(PYTHONANYWHERE_URL + '/api/predict3/1234', json=df_i.to_json(orient='split'))
    if res.ok:
        print "Company:", df["TICKER"].iloc[i], "; API 3 prediction:", res.json(), "; Real= ", \
        df["BB_1YR_DEFAULT_PROB_CQ4_2015"].iloc[i]

    df_i = df4.iloc[i]
    res = requests.post(PYTHONANYWHERE_URL + '/api/predict4/1234', json=df_i.to_json(orient='split'))
    if res.ok:
        print "Company:", df["TICKER"].iloc[i], "; API 4 prediction:", res.json(), "; Real= ", \
        df["BB_1YR_DEFAULT_PROB_CQ4_2015"].iloc[i]