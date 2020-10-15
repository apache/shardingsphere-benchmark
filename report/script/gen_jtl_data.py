#coding:utf-8
import os,sys
import json
import time
from datetime import datetime
from collections import OrderedDict
def gen_desc(res_json):
    """
    replace your scene description as following examples. 
    """
    desc_json={
       "mysqlVerison":"5.7.24",
       "tableDescription":"id  bigint(20)  primary key,\nk int(11),\nc char(120),\npad  char(60)\n",
       "shardingRule":"tables:\n  tbl:\n    actualDataNodes: ds_${0..3}.tbl${0..1023}\n    tableStrategy:\n      inline:\n        shardingColumn: k\n        algorithmExpression: tbl${k % 1024}\n    keyGenerator:\n        type: SNOWFLAKE\n        column: id\ndefaultDatabaseStrategy:\n  inline:\n    shardingColumn: id\n    algorithmExpression: ds_${id % 4}",
       "masterSlaveRule":"None",
       "encryptRule":"None",
       "INSERT+UPDATE+DELETE":{
           "SqlExample":"INSERT INTO tbl(k, c, pad) VALUES(1, '###-###-###', '###-###');\nUPDATE tbl SET c='####-####-####', pad='####-####' WHERE id=?;\nDELETE FROM tbl WHERE id=?",
           "ComparativeType": "INSERT+UPDATE+DELETE"
       },
       "SELECT":{
           "SqlExample":"SELECT id,k FROM tbl ignore index(`PRIMARY`) WHERE id=? AND k=?",
           "ComparativeType": "SELECT"
       },
    }
    res_json['DESC']=desc_json
    return res_json
    
def gen_select(input_file_name,res_json):
    #if not os.path.exists(select_file_name):
    #    os.system(r"touch {}".format(select_file_name))
    mysql_file_name = '{}/{}'.format(input_file_name, "mysql_select.jtl")
    sharding_proxy_file_name = '{}/{}'.format(input_file_name, "sharding-proxy_select.jtl")
    sharding_jdbc_file_name = '{}/{}'.format(input_file_name, "sharding-jdbc_select.jtl")
    with open(sharding_proxy_file_name) as f:
        for line in f:
            line = line.strip("\n")
            if line != "":
                res_json['SELECT'][0]['data'].append(json.loads(line))
    with open(sharding_jdbc_file_name) as f:
        for line in f:
            line = line.strip("\n")
            if line != "":
                res_json['SELECT'][1]['data'].append(json.loads(line))
    with open(mysql_file_name) as f:
        for line in f:
            line = line.strip("\n")
            if line != "":
                res_json['SELECT'][2]['data'].append(json.loads(line))

def gen_all(input_file_name,res_json):
    mysql_file_name = '{}/{}'.format(input_file_name, "mysql_all.jtl")
    sharding_proxy_file_name = '{}/{}'.format(input_file_name, "sharding-proxy_all.jtl")
    sharding_jdbc_file_name = '{}/{}'.format(input_file_name, "sharding-jdbc_all.jtl")
    with open(sharding_proxy_file_name) as f:
        for line in f:
            line = line.strip("\n")
            if line != "":
                res_json['INSERT+UPDATE+DELETE'][0]['data'].append(json.loads(line))
    with open(sharding_jdbc_file_name) as f:
        for line in f:
            line = line.strip("\n")
            if line != "":
                res_json['INSERT+UPDATE+DELETE'][1]['data'].append(json.loads(line))
    with open(mysql_file_name) as f:
        for line in f:
            line = line.strip("\n")
            if line != "":
                res_json['INSERT+UPDATE+DELETE'][2]['data'].append(json.loads(line))

def cur_file_dir():
    path = sys.path[0]
    if os.path.isdir(path):
        return path
    elif os.path.isfile(path):
        return os.path.dirname(path)

def gen_json(input_file_name, out_name):
    res_json = OrderedDict()
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
        "INSERT+UPDATE+DELETE":[
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
    gen_select(input_file_name,res_json)
    gen_all(input_file_name,res_json)
    save_dir = cur_file_dir()
    newfile='%s/%s'%(save_dir,out_name)
    with open(newfile,'w') as f:
        f.write(json.dumps(res_json))

     
if __name__ == '__main__':
    input_file_name = sys.argv[1]
    out_name = sys.argv[2]
    gen_json(input_file_name, out_name)
   
