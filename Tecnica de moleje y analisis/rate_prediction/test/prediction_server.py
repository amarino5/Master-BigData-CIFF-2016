# -*- coding: utf-8 -*-
from flask import Flask, request, render_template, jsonify
import numpy as np
from sklearn.decomposition import PCA
from sklearn.externals import joblib
import pandas.io.sql as sql
import sqlite3
import platform


######
# FLASK - START
######
app = Flask(__name__)  # instancing the Flask object


# In Windows use "localhost" in your browser, what is equivalent to http://localhost:80/, which is also equivalent to http://127.0.0.1:80/
# http means your data will be tranfered using Hypertext Transfer Protocol, most commonly used over the internet
# localhost is a domain name system (DNS) which is equivalent to say 127.0.0.1, in other word "localhost" resolves to 127.0.0.1
# 80, means you will use the internet default port

# In pythonanywhere we will use http://<YOUR USER>.pythonanywhere.com, my one for the viedo is: http://rating.pythonanywhere.com



# Setting a route for python function called index, / is equivalent to "no route"
# This route is is widelly as Index or Home Page.
# @app.route('/') is equivalent to @app.route('/', methods=['GET', 'POST']), meaning GET and POST methods are allowed.
@app.route('/')
def index():
    return render_template('index.html',
                           userlogged="Hello visitor ;-), you are current not logged in. Hola visitante, no te encuentras logeado.")


print "IMPORTING LIBRARIES..."
import pandas as pd
import requests

# DOWLOADING FILE FROM DROPBOX FIRST TIME
import os.path
import time
import random

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

print "GETTING LIST OF VARIABLES..."
print "-> targe variable:"
target_variable = str(df_dictionary[df_dictionary['TYPE_OF_VARIABLE'] == 'TARGET']['FIELD_NAME'].tolist()[0])
print "target_variable = ", target_variable

list_of_inputs_for_model0 = df_dictionary[df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT0']['FIELD_NAME'].tolist()
print "list_of_inputs_for_model0 = ", list_of_inputs_for_model0

list_of_inputs_for_model1 = \
df_dictionary[(df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT0') | (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT1')][
    'FIELD_NAME'].tolist()
print "list_of_inputs_for_model1 = ", list_of_inputs_for_model1

list_of_inputs_for_model2 = df_dictionary[
    (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT0') | (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT1') | (
    df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT2')]['FIELD_NAME'].tolist()
print "list_of_inputs_for_model2 = ", list_of_inputs_for_model2

list_of_inputs_for_model3 = df_dictionary[
    (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT0') | (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT1') | (
    df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT2') | (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT3')][
    'FIELD_NAME'].tolist()
print "list_of_inputs_for_model3 = ", list_of_inputs_for_model3

list_of_inputs_for_model4 = df_dictionary[
    (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT0') | (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT1') | (
    df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT2') | (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT3') | (
    df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT4')]['FIELD_NAME'].tolist()
print "list_of_inputs_for_model4 = ", list_of_inputs_for_model4



@app.route('/api/predict0/<uuid>', methods=['GET', 'POST'])
def predict0(uuid):
    content = request.json
    df_request = pd.read_json(content, typ='series', orient='split')
    target_variable = 'BB_1YR_DEFAULT_PROB_CQ4_2015'
    identifier = 'TICKER'
    list_continuous_PD = [col for col in list(df) if (col.startswith('BB_1YR_DEFAULT_PROB'))]

    #Transformamos la request en un dtaframe con un registro a predecir
    for item in list_of_inputs_for_model0:
        print item, " = ", df_request[item]

    #df_modelo0.drop([identifier], axis=1, inplace=True)

    list_of_inputs_for_model0_clean = [v for v in list_of_inputs_for_model0 if
                                 not (v.startswith('BEST_') | v.startswith('CUR_')
                                      | v.startswith('NAME')
                                      | v.startswith('INDUSTRY_')
                                      | v.startswith('TICKER'))]

    df_modelo0 = df[list_of_inputs_for_model0_clean]
    lenls = len(df_modelo0.columns)
    #Calculamos los ratios
    list0ratios = [x for x in list_of_inputs_for_model0_clean if x not in list_continuous_PD]

    for i in range(0, lenls):
        v_1 = df_modelo0.columns[i]
        for j in range(i + 1, lenls):
            v_2 = df_modelo0.columns[j]
            if (v_1 in list0ratios and v_2 in list0ratios):
                ratio = 'r_div' + v_1 + v_2
                df_modelo0[ratio] = np.where(df_modelo0[v_2] == 0, 0, (df_modelo0[v_1] / df_modelo0[v_2]))
                list_of_inputs_for_model0.append(ratio)

    df_modelo0 = df_modelo0.replace([np.inf, -np.inf], 0)  # Repasar si se sustituye por la media.

    for var in df_modelo0.columns:
        if (var in list_of_inputs_for_model0):
            media = df_modelo0[var].mean()
            stdev = df_modelo0[var].std()
            df_modelo0[var] = (df_modelo0[var] - media) / stdev
    # Realizadas todas las transformaciones, restringimos el df a ratios y PD para aplicar PCA
    list_of_inputs_for_model0_rev = [v for v in list_of_inputs_for_model0 if
                                 (v.startswith('r') | v.startswith('BB_1YR_DEFAULT_PROB'))]

    pca = PCA()
    df_pca = pca.fit_transform(df_modelo0)

    variance = pca.explained_variance_ratio_.cumsum()

    for i in range(0, len(df_modelo0.columns)):
        var_explained = variance[i:i + 1]
        if var_explained >= 0.90:
            print ("Numero de Componentes", i, var_explained)
            pca = PCA(n_components=i)
            df_0pca = pca.fit_transform(df_modelo0)

    pca = PCA(n_components=40)
    pca = pca.fit(df_modelo0)
    df2 = pca.transform(df_modelo0)
    df_pca2 = pd.DataFrame(df2, columns=range(pca.n_components_))

    # Convertir a un nuevo dataframe
    df_pca = pd.DataFrame(data=df_pca[::], index=list(range(0, len(df))))

    ####### Modelo 0#######
    try:
        rf = joblib.load('C:/Desarrollo/repos/Master-BigData-CIFF-2016/Tecnica de moleje y analisis/Balancesheet-PythonRF_model0.pkl')
        myprediction0 = rf.predict(df_pca2)
    except np.linalg.linalg.LinAlgError as err:
        if 'Singular matrix' in err.message:
            print "MODEL-INVALID (Singular Matrix)"
        else:
            raise

    return jsonify({"prediction": myprediction0})


@app.route('/api/predict1/<uuid>', methods=['GET', 'POST'])
def predict1(uuid):
    if uuid != '1234':
        return jsonify({"error": "user not unauthorised, wrong API key."})
    content = request.json
    df = pd.read_json(content, typ='series', orient='split')
    df = pd.read_json(content, typ='series', orient='split')

    #    d = json.loads(content)
    #    print df.columns
    for item in list_of_inputs_for_model1:
        print item, " = ", df[item]

    myprediction1 = 0.1  # IMPLEMENT HERE MY MODEL 1
    return jsonify({"prediction": myprediction1})


@app.route('/api/predict2/<uuid>', methods=['GET', 'POST'])
def predict2(uuid):
    if uuid != '1234':
        return jsonify({"error": "user not unauthorised, wrong API key."})
    content = request.json
    df = pd.read_json(content, typ='series', orient='split')
    df = pd.read_json(content, typ='series', orient='split')

    #    d = json.loads(content)
    #    print df.columns
    for item in list_of_inputs_for_model2:
        print item, " = ", df[item]

    myprediction2 = 0.2  # IMPLEMENT HERE MY MODEL 1
    return jsonify({"prediction": myprediction2})


@app.route('/api/predict3/<uuid>', methods=['GET', 'POST'])
def predict3(uuid):
    if uuid != '1234':
        return jsonify({"error": "user not unauthorised, wrong API key."})
    content = request.json
    df = pd.read_json(content, typ='series', orient='split')
    df = pd.read_json(content, typ='series', orient='split')

    #    d = json.loads(content)
    #    print df.columns
    for item in list_of_inputs_for_model3:
        print item, " = ", df[item]

    myprediction3 = 0.3  # IMPLEMENT HERE MY MODEL 1

    return jsonify({"prediction": myprediction3})


@app.route('/api/predict4/<uuid>', methods=['GET', 'POST'])
def predict4(uuid):
    if uuid != '1234':
        return jsonify({"error": "user not unauthorised, wrong API key."})
    content = request.json
    df = pd.read_json(content, typ='series', orient='split')
    df = pd.read_json(content, typ='series', orient='split')

    #    d = json.loads(content)
    #    print df.columns
    for item in list_of_inputs_for_model4:
        print item, " = ", df[item]

    myprediction4 = 0.4  # IMPLEMENT HERE MY MODEL 1

    return jsonify({"prediction": myprediction4})


###############################################################################################################
#         INFINITY LOOP LISTENING TO PORT 80 (port=int("80")) FROM THE OUTSIDE WORLD (host="0.0.0.0") - START #
###############################################################################################################
if __name__ == '__main__':
    app.run(
        host="localhost",
        port=int("8080")
        #        ,	processes=9
        #        debug=True
    )
###############################################################################################################
#         INFINITY LOOP LISTENING TO PORT 80 (port=int("80")) FROM THE OUTSIDE WORLD (host="0.0.0.0") - END   #
###############################################################################################################

