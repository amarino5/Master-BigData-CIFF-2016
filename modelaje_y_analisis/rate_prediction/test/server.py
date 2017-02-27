# -*- coding: utf-8 -*-
from flask import Flask, request, render_template, jsonify
import pandas.io.sql as sql
import sqlite3
import platform


######
# FLASK - START
######
app = Flask(__name__) # instancing the Flask object

#In Windows use "localhost" in your browser, what is equivalent to http://localhost:80/, which is also equivalent to http://127.0.0.1:80/
# http means your data will be tranfered using Hypertext Transfer Protocol, most commonly used over the internet
# localhost is a domain name system (DNS) which is equivalent to say 127.0.0.1, in other word "localhost" resolves to 127.0.0.1
# 80, means you will use the internet default port

#In pythonanywhere we will use http://<YOUR USER>.pythonanywhere.com, my one for the viedo is: http://rating.pythonanywhere.com



#Setting a route for python function called index, / is equivalent to "no route"
#This route is is widelly as Index or Home Page.
#@app.route('/') is equivalent to @app.route('/', methods=['GET', 'POST']), meaning GET and POST methods are allowed.
@app.route('/')
def index():
    return render_template('index.html',userlogged="Hello visitor ;-), you are current not logged in. Hola visitante, no te encuentras logeado.")


###############################################################
#         EVERY REQUEST ABOVE NEEDS PASSWORD - END            #
###############################################################

#@app.route('/predict0/<uuid>', methods=['GET','POST'])
#def predict0(uuid):
#    content = request.json
#    print content['BB_1YR_DEFAULT_PROB_CQ4_2013']
#    return jsonify({"uuid":content['BB_1YR_DEFAULT_PROB_CQ4_2013']})



print "IMPORTING LIBRARIES..."
import pandas as pd
import requests


#DOWLOADING FILE FROM DROPBOX FIRST TIME
import os.path
import time
import random
while not os.path.exists('EX2_DATA_BASE.xlsx'):
    time.sleep (3*random.random()); #Sleeping less than 3 seconds before going to Dropbox - avoid too many students at once.
    if not os.path.exists('EX2_DATA_BASE.xlsx'):
        print "DOWLOADING FILE EX2_DATA_BASE.xlsx FROM DROPBOX BECAUSE LOCAL FILE DOES NOT EXIST!"
#        csvfile = urllib2.urlopen("https://dl.dropboxusercontent.com/u/28535341/EX2_DEV_DATA.xlsx")
        resp = requests.get("https://dl.dropboxusercontent.com/u/28535341/EX2_DATA_BASE.xlsx")
        output = open('EX2_DATA_BASE.xlsx','wb')
        output.write(resp.content)
        output.close()
#DOWLOADING FILE FROM DROPBOX FIRST TIME


print "LOADING DATASETS..."
#df = pd.read_csv("EX2_DEV_DATA.xlsx",sep=";") #DEV-SAMPLE
df = pd.read_excel(open('EX2_DATA_BASE.xlsx','rb'), sheetname='DATA')
df_dictionary = pd.read_excel(open('EX2_DATA_BASE.xlsx','rb'), sheetname='DICTIONARY')

print "GETTING LIST OF VARIABLES..."
print "-> targe variable:"
target_variable = str(df_dictionary[df_dictionary['TYPE_OF_VARIABLE'] == 'TARGET']['FIELD_NAME'].tolist()[0])
print "target_variable = ",target_variable

list_of_inputs_for_model0 = df_dictionary[df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT0']['FIELD_NAME'].tolist()
print "list_of_inputs_for_model0 = ",list_of_inputs_for_model0

list_of_inputs_for_model1 = df_dictionary[(df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT0') | (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT1')]['FIELD_NAME'].tolist()
print "list_of_inputs_for_model1 = ",list_of_inputs_for_model1

list_of_inputs_for_model2 = df_dictionary[(df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT0') | (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT1')| (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT2')]['FIELD_NAME'].tolist()
print "list_of_inputs_for_model2 = ",list_of_inputs_for_model2

list_of_inputs_for_model3 = df_dictionary[(df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT0') | (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT1')| (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT2')| (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT3')]['FIELD_NAME'].tolist()
print "list_of_inputs_for_model3 = ",list_of_inputs_for_model3

list_of_inputs_for_model4 = df_dictionary[(df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT0') | (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT1')| (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT2')| (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT3')| (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT4')]['FIELD_NAME'].tolist()
print "list_of_inputs_for_model4 = ",list_of_inputs_for_model4


list_of_inputs_for_model0 = df_dictionary[df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT0']['FIELD_NAME'].tolist()
list_of_inputs_for_model0.remove('TICKER')
list_of_inputs_for_model0.remove('NAME')
list_of_inputs_for_model0.remove('INDUSTRY_GROUP')
print "list_of_inputs_for_model0 = ",list_of_inputs_for_model0


list_of_inputs_for_model1 = df_dictionary[(df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT0') | (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT1')]['FIELD_NAME'].tolist()
list_of_inputs_for_model1.remove('TICKER')
list_of_inputs_for_model1.remove('NAME')
list_of_inputs_for_model1.remove('INDUSTRY_GROUP')
print "list_of_inputs_for_model1 = ",list_of_inputs_for_model1

list_of_inputs_for_model2 = df_dictionary[(df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT0') | (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT1')| (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT2')]['FIELD_NAME'].tolist()
list_of_inputs_for_model2.remove('TICKER')
list_of_inputs_for_model2.remove('NAME')
list_of_inputs_for_model2.remove('INDUSTRY_GROUP')
print "list_of_inputs_for_model2 = ",list_of_inputs_for_model2

list_of_inputs_for_model3 = df_dictionary[(df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT0') | (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT1')| (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT2')| (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT3')]['FIELD_NAME'].tolist()
list_of_inputs_for_model3.remove('TICKER')
list_of_inputs_for_model3.remove('NAME')
list_of_inputs_for_model3.remove('INDUSTRY_GROUP')
print "list_of_inputs_for_model3 = ",list_of_inputs_for_model3

list_of_inputs_for_model4 = df_dictionary[(df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT0') | (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT1')| (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT2')| (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT3')| (df_dictionary['TYPE_OF_VARIABLE'] == 'INPUT4')]['FIELD_NAME'].tolist()
list_of_inputs_for_model4.remove('TICKER')
list_of_inputs_for_model4.remove('NAME')
list_of_inputs_for_model4.remove('INDUSTRY_GROUP')
print "list_of_inputs_for_model4 = ",list_of_inputs_for_model4


in_model0=list_of_inputs_for_model0
in_model1=list_of_inputs_for_model1
in_model2=list_of_inputs_for_model2
in_model3=list_of_inputs_for_model3
in_model4=list_of_inputs_for_model4

from sklearn.ensemble import RandomForestClassifier
import cPickle
from sklearn.externals import joblib


@app.route('/api/predict0/<uuid>', methods=['GET', 'POST'])
def predict0(uuid):
    if uuid != '1234':
        return jsonify({"error":"user not unauthorised, wrong API key."})
    content = request.json
    df = pd.read_json(content, typ='series', orient='split')

    with open('C:/Desarrollo/repos/Master-BigData-CIFF-2016/Tecnica de moleje y analisis/Balancesheet-Python/RF0.pkl',
              'rb') as f:
        rf0 = cPickle.load(f)
#    d = json.loads(content)
#    print df.columns
    for item in list_of_inputs_for_model0:
        print item," = ", df[item]

    in_model=list_of_inputs_for_model0
    X = df[in_model]
    myprediction0 = str(rf0.predict(X)[0])
    print 'prediction model 0 is:', myprediction0
    json = jsonify({"prediction":myprediction0})
    return json

with open('C:/Desarrollo/repos/Master-BigData-CIFF-2016/Tecnica de moleje y analisis/Balancesheet-Python/RF1.pkl', 'rb') as f:
    rf1 = cPickle.load(f)

@app.route('/api/predict1/<uuid>', methods=['GET', 'POST'])
def predict1(uuid):
    if uuid != '1234':
        return jsonify({"error":"user not unauthorised, wrong API key."})
    content = request.json
    df = pd.read_json(content, typ='series', orient='split')
    df = pd.read_json(content, typ='series', orient='split')

#    d = json.loads(content)
#    print df.columns
    for item in list_of_inputs_for_model1:
        print item," = ", df[item]

    in_model=list_of_inputs_for_model1
    X = df[in_model]
    myprediction1 = str(rf1.predict(X)[0])
    return jsonify({"prediction":myprediction1})

with open('C:/Desarrollo/repos/Master-BigData-CIFF-2016/Tecnica de moleje y analisis/Balancesheet-Python/RF2.pkl', 'rb') as f:
    rf2 = cPickle.load(f)

@app.route('/api/predict2/<uuid>', methods=['GET', 'POST'])
def predict2(uuid):
    if uuid != '1234':
        return jsonify({"error":"user not unauthorised, wrong API key."})
    content = request.json
    df = pd.read_json(content, typ='series', orient='split')
    df = pd.read_json(content, typ='series', orient='split')

#    d = json.loads(content)
#    print df.columns
    for item in list_of_inputs_for_model2:
        print item," = ", df[item]

    in_model=list_of_inputs_for_model2
    X = df[in_model]
    myprediction2 = str(rf2.predict(X)[0])
    return jsonify({"prediction":myprediction2})

with open('C:/Desarrollo/repos/Master-BigData-CIFF-2016/Tecnica de moleje y analisis/Balancesheet-Python/RF3.pkl', 'rb') as f:
    rf3 = cPickle.load(f)

@app.route('/api/predict3/<uuid>', methods=['GET', 'POST'])
def predict3(uuid):
    if uuid != '1234':
        return jsonify({"error":"user not unauthorised, wrong API key."})
    content = request.json
    df = pd.read_json(content, typ='series', orient='split')

#    d = json.loads(content)
#    print df.columns
    for item in list_of_inputs_for_model3:
        print item," = ", df[item]

    in_model=list_of_inputs_for_model3
    X = df[in_model]
    myprediction3 = str(rf3.predict(X)[0])
    return jsonify({"prediction":myprediction3})

with open('C:/Desarrollo/repos/Master-BigData-CIFF-2016/Tecnica de moleje y analisis/Balancesheet-Python/RF4.pkl', 'rb') as f:
    rf4 = cPickle.load(f)

@app.route('/api/predict4/<uuid>', methods=['GET', 'POST'])
def predict4(uuid):
    if uuid != '1234':
        return jsonify({"error":"user not unauthorised, wrong API key."})
    content = request.json
    df = pd.read_json(content, typ='series', orient='split')

#    d = json.loads(content)
#    print df.columns
    for item in list_of_inputs_for_model4:
        print item," = ", df[item]

    in_model=list_of_inputs_for_model4
    X = df[in_model]
    myprediction4 = str(rf4.predict(X)[0])

    return jsonify({"prediction":myprediction4})

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




