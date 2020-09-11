import sys
from decimal import Decimal
import time
import json

def calculate_tps(file_name,output_name):
    with open(file_name) as file_input:
       total = 0
       start_time = 0
       end_time = 0
       items = []
       time_list = []
       succ_count = 0
       fail_count = 0
       #timestr=int(time.time()*1000)
       timestr=time.strftime('%Y.%m.%d %H:%M:%S ',time.localtime(time.time()))
       v3_item={
           "Samples":"null",
           "Throughout":"null",
           "50th":"null",
           "95th":"null",
           "99.9th":"null",
           "Avg":"null",
           "Min":"null",
           "Max":"null",
           "Err":"null",
           "Date":"null"
       }
       ##['1571884442222', '36', 'JdbcSingleTableSelect', '', '', 'Thread Group 1-15', '', 'true', '', '0', '0', '20', '20', '0', '0', '0\n']
       for line in file_input:
           line = line.strip("\n")
           if line != "":
               items=line.split(",")
               #print(items[1])
               time_list.append(int(items[1]))
               if total == 0:
                   start_time = items[0]
               if items[7] == "true":
                   succ_count = succ_count+1
               else:
                   fail_count = fail_count+1
               total = total+1
       if total > 0:
           end_time = items[0]
           the99 = int(total*0.999)
           the95 = int(total*0.95)
           the50 = int(total*0.5)
           timeStamp = int(end_time)-int(start_time)
           tps=str(Decimal(str(total*1.0/(timeStamp*1.0/1000))).quantize(Decimal('0.00')))
           #print(start_time)
           #print(timeStamp)
           #print(succ_count)
           err=str(float(fail_count/total)*100)+"%"
           time_new_list=sorted(time_list)
           the90s=str(time_new_list[the50])
           the95s=str(time_new_list[the95])
           the99s=str(time_new_list[the99])
           avg=str(sum(time_list)/total)
           minstr=str(time_new_list[0])
           maxstr=str(time_new_list[len(time_new_list)-1])
           #timestr=int(time.time()*1000)
           #time.strftime('%Y.%m.%d %H:%M:%S ',time.localtime(time.time()))
           v3_item={
               "Samples":total,
               "Throughout":tps,
               "50th":the90s,
               "95th":the95s,
               "99.9th":the99s,
               "Avg":avg,
               "Min":minstr,
               "Max":maxstr,
               "Err":err,
               "Date":timestr
           }
           print(v3_item)
    with open(output_name, 'a+') as f:
        f.write(json.dumps(v3_item))
        f.write("\n")
        f.close()
if __name__=='__main__':
    file_name = sys.argv[1]
    out_name = sys.argv[2]
    gen_stat(file_name,out_name)