#coding:utf-8
import os,sys
import json
import time
from datetime import datetime

def gen_desc(res_json):
    """
    replace your scene description as following examples. 
    """
    desc_json={
       "mysqlVerison":"5.7.24",
       "tableNumber":"single table(e.g:t_test)",
       "sceneDescription":"This is a model for connecting directly to a database through a proxy.We used id, k, c, pad fields in the table.Including a database and a table",
       "SELECT":{
           "SqlExample":"SELECT id,k FROM t_test WHERE id = # AND k = #",
           "ComparativeType": "SELECT"
       },
       "INSERT":{
           "SqlExample":"INSERT INTO t_test(k,c,pad) VALUES(#,#,#)",
           "ComparativeType": "INSERT"
       },
       "DELETE":{
           "SqlExample":"DELETE FROM t_test WHERE id = # AND k = #",
           "ComparativeType": "DELETE"
       },
       "UPDATE":{
           "SqlExample":"UPDATE t_test SET k = # WHERE id = # AND k = #",
           "ComparativeType": "UPDATE"
       }
    }
    res_json['DESC']=desc_json
    return res_json

def gen_insert(insert_file_name,res_json):
    if not os.path.exists(insert_file_name):
        os.system(r"touch {}".format(insert_file_name))
    with open(insert_file_name) as f:
        for line in f:
            line = line.strip("\n")
            if line != "":
                res_json['INSERT'][0]['data'].append(json.loads(line))
    
def gen_select(select_file_name,res_json):
    if not os.path.exists(select_file_name):
        os.system(r"touch {}".format(select_file_name))
    with open(select_file_name) as f:
        for line in f:
            line = line.strip("\n")
            if line != "":
                res_json['SELECT'][0]['data'].append(json.loads(line))
def gen_update(update_file_name,res_json):
    if not os.path.exists(update_file_name):
        os.system(r"touch {}".format(update_file_name))
    with open(update_file_name) as f:
        for line in f:
            line = line.strip("\n")
            if line != "":
                res_json['UPDATE'][0]['data'].append(json.loads(line))
def gen_delete(delete_file_name,res_json):
    if not os.path.exists(delete_file_name):
        os.system(r"touch {}".format(delete_file_name))
    with open(delete_file_name) as f:
        for line in f:
            line = line.strip("\n")
            if line != "":
                res_json['DELETE'][0]['data'].append(json.loads(line))
def cur_file_dir():
    path = sys.path[0]
    if os.path.isdir(path):
        return path
    elif os.path.isfile(path):
        return os.path.dirname(path)
def gen_json(select_file_name, insert_file_name, update_file_name, delete_file_name, out_name):
    res_json = {
        "SELECT":[
             {"type":"Sharding-Proxy",
              "data":[]
             },
             {"type":"Sharding-JDBC",
             "data":[]
             },
             {"type":"MySQL", 
             "data":[]
             }
        ],
        "INSERT":[
             {"type":"Sharding-Proxy",
              "data":[]
             },
             {"type":"Sharding-JDBC",
             "data":[]
             },
             {"type":"MySQL", 
             "data":[]
             }
         ],
        "UPDATE":[
             {"type":"Sharding-Proxy",
              "data":[]
             },
             {"type":"Sharding-JDBC",
             "data":[]
             },
             {"type":"MySQL", 
             "data":[]
             }
        ],
        "DELETE":[
             {"type":"Sharding-Proxy",
              "data":[]
             },
             {"type":"Sharding-JDBC",
             "data":[]
             },
             {"type":"MySQL", 
             "data":[]
             }
        ],
        "DESC":{
         }
    }
    res_json=gen_desc(res_json)
    gen_insert(insert_file_name,res_json)
    gen_select(select_file_name,res_json)
    gen_update(update_file_name,res_json)
    gen_delete(delete_file_name,res_json)
    save_dir = cur_file_dir()
    newfile='%s/%s'%(save_dir,out_name)
    with open(newfile,'w') as f:
        f.write(json.dumps(res_json))

     
if __name__ == '__main__':
    select_file_name = sys.argv[1]
    insert_file_name = sys.argv[2]
    update_file_name = sys.argv[3]
    delete_file_name = sys.argv[4]
    out_name = sys.argv[5]
    gen_json(select_file_name, insert_file_name, update_file_name, delete_file_name, out_name)
   
