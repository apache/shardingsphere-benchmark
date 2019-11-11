import sys
import time
import json
from decimal import Decimal

def gen_stat(file_name,output_name):
    with open(file_name) as file_input:
        total = 0
        start_time = 0
        end_time = 0
        items = []
        time_list = []
        succ_count = 0
        fail_count = 0
        timestr=time.strftime('%Y.%m.%d %H:%M:%S ',time.localtime(time.time()))
        output_item={
            "Samples":0,
            "Throughout":0,
            "50th":0,
            "95th":0,
            "99.9th":0,
            "Avg":0,
            "Min":0,
            "Max":0,
            "Err":0,
            "Date":timestr
        }
        for line in file_input:
            line = line.strip("\n")
            if line != "":
                items=line.split(",")
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
            err=str(float(fail_count/total)*100)+"%"
            time_sorted_list=sorted(time_list)
            the50s=str(time_sorted_list[the50])
            the95s=str(time_sorted_list[the95])
            the99s=str(time_sorted_list[the99])
            avg=str(sum(time_list)/total)
            minstr=str(time_sorted_list[0])
            maxstr=str(time_sorted_list[len(time_sorted_list)-1])
            output_item={
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
        print(output_name)
        with open(output_name, 'a+') as f:
            f.write(json.dumps(output_item)) 
            f.write("\n")
if __name__=='__main__':
    file_name = sys.argv[1]
    out_name = sys.argv[2] 
    gen_stat(file_name,out_name)
