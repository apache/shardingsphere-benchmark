package org.apache.shardingsphere.benchmark.common.file.util;

import java.io.*;
import java.util.*;

/**
 * Properties util.
 */
public class PropertiesUtil {

    public static Map SQL = new HashMap<>(1,1);
    static {
        Properties prop = new Properties();
        try {
            InputStream in = PropertiesUtil.class.getResourceAsStream("/config/sqlconfig.properties");
            BufferedReader br = new BufferedReader(new InputStreamReader(in));
            prop.load(in);
            String values = "";
            Iterator<String> it = prop.stringPropertyNames().iterator();
            while (it.hasNext()) {
                String key = it.next();
                if (key.contains(".values")) {
                    values = prop.getProperty(key);
                    if (values != null && values.length() > 0 && values.contains(",")) {
                        String[] arrayValues = values.split(",");
                        List listValues = new ArrayList();
                        for (int i = 0; i < arrayValues.length; i++) {
                            String[] subs = arrayValues[i].split(":");
                            if (subs.equals("Int")) {
                                listValues.add(Integer.valueOf(subs[1]));
                            } else if (subs[0].equals("Float")) {
                                listValues.add(Float.valueOf(subs[1]));
                            } else if (subs[0].equals("Long")) {
                                listValues.add(Long.valueOf(subs[1]));
                            } else if (subs[0].equals("String")) {
                                listValues.add(String.valueOf(subs[1]));
                            } else {
                                listValues.add(subs[1]);
                            }
                        }
                        SQL.put(key, listValues);
                    } else {
                        SQL.put(key, prop.getProperty(key));
                    }
                } else {
                    SQL.put(key, prop.getProperty(key));
                }
            }
            in.close();
            br.close();
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    public static void main(String[] args) {
        System.out.println(SQL.toString());
        Map m = new HashMap();
        m.put("key1", "abc");
        m.put("key2", 1);
        System.out.println(m.toString());
        System.out.println(Boolean.TRUE.toString());
        System.out.println(Boolean.FALSE.toString());
    }
}
