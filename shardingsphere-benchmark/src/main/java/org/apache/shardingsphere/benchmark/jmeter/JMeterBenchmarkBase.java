package org.apache.shardingsphere.benchmark.jmeter;

import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import org.apache.shardingsphere.benchmark.common.file.util.PropertiesUtil;
import org.apache.shardingsphere.benchmark.db.jdbc.JDBCDataSourceUtil;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

public class JMeterBenchmarkBase extends AbstractJavaSamplerClient {

    public static Map sqlConfig = new HashMap<>();
    public static Map dbConfig = new HashMap<>();
    public static Map userConfig = new HashMap<>();
    public static Map benchmarkResultPath = new HashMap<>();
    public static List<String> initDbSqlList = new ArrayList<String>();
    public static String benchmarkVersion;

    static {
        initSqlConfig();
        initDbConfig();
        initBenchmarkResultPath();
        initBenchmarkVersion();
        initDbSqls();
        initUserConfig();
    }

    /**
     * Init dataSource config for all of benchmark scenarios.
     */
    public static void initDbConfig() {
        Properties dbConfigProp = new Properties();
        try {
            InputStream in = PropertiesUtil.class.getResourceAsStream("/config/dbconfig.properties");
            BufferedReader br = new BufferedReader(new InputStreamReader(in));
            dbConfigProp.load(in);
            Iterator<String> it = dbConfigProp.stringPropertyNames().iterator();
            while (it.hasNext()) {
                String key = it.next();
                if (key.contains(".port") || key.equals("maximumpoolsize") || key.equals("connectiontimeout") || key.equals("idletimeout")
                        || key.equals("maxlifetime") || key.equals("prepstmtcachesize") || key.equals("prepstmtcachesqllimit") || key.equals("nettimeoutforstreamingresults")) {
                    dbConfig.put(key, Integer.valueOf(dbConfigProp.getProperty(key)).intValue());
                } else {
                    dbConfig.put(key, dbConfigProp.getProperty(key));
                }
            }
            in.close();
            br.close();

        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    /**
     * Init sql config for all of benchmark scenarios.
     */
    public static void initSqlConfig() {
        Properties sqlProp = new Properties();
        try {
            InputStream in = PropertiesUtil.class.getResourceAsStream("/config/sqlconfig.properties");
            BufferedReader br = new BufferedReader(new InputStreamReader(in));
            sqlProp.load(in);
            String values = "";
            Iterator<String> it = sqlProp.stringPropertyNames().iterator();
            while (it.hasNext()) {
                String key = it.next();
                if (key.contains(".values")) {
                    values = sqlProp.getProperty(key);
                    if (values != null && values.length() > 0) {
                        String[] arrayValues = values.split(",");
                        List listValues = new ArrayList();
                        for (int i = 0; i < arrayValues.length; i++) {
                            String[] subs = arrayValues[i].split(":");
                            if ("Int".equals(subs[0])) {
                                listValues.add(Integer.valueOf(subs[1]).intValue());
                            } else if ("Float".equals(subs[0])) {
                                listValues.add(Float.valueOf(subs[1]).floatValue());
                            } else if ("Long".equals(subs[0])) {
                                listValues.add(Long.valueOf(subs[1]).longValue());
                            } else if ("String".equals(subs[0])) {
                                listValues.add(String.valueOf(subs[1]));
                            } else if ("".equals(((String) arrayValues[i]))) {
                                listValues.add((String) arrayValues[i]);
                            } else {
                                listValues.add((String) subs[1]);
                            }
                        }
                        sqlConfig.put(key, listValues);
                    } else {
                        sqlConfig.put(key, sqlProp.getProperty(key));
                    }

                } else {
                    sqlConfig.put(key, sqlProp.getProperty(key));
                }


            }
            in.close();
            br.close();

        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    /**
     * Store path of JMeter result file for benchmark into memory.
     */
    public static void initBenchmarkResultPath() {
        Properties dbConfigProp = new Properties();
        try {
            InputStream in = PropertiesUtil.class.getResourceAsStream("/config/benchmark-result.properties");
            BufferedReader br = new BufferedReader(new InputStreamReader(in));
            dbConfigProp.load(in);
            Iterator<String> it = dbConfigProp.stringPropertyNames().iterator();
            while (it.hasNext()) {
                String key = it.next();
                benchmarkResultPath.put(key, dbConfigProp.getProperty(key));
            }
            in.close();
            br.close();

        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
    
    /**
     * Init benchmark sql.
     */
    public static void initDbSqls(){
        InputStream in = PropertiesUtil.class.getResourceAsStream("/config/benchmark.sql");
        BufferedReader br = new BufferedReader(new InputStreamReader(in));
        String line = "";
        try {
            while ((line = br.readLine()) != null) {
                initDbSqlList.add(line);
            }
        } catch(IOException exception){
            exception.printStackTrace();
        } catch (Exception exception){
            exception.printStackTrace();
        } finally {
            try {
                br.close();
            } catch (IOException exception) {
                exception.printStackTrace();
            }
        }

    }
    
    /**
     * Init user config.
     */
    public static void initUserConfig(){
        Properties dbConfigProp = new Properties();
        try {
            InputStream in = PropertiesUtil.class.getResourceAsStream("/config/user-config.properties");
            BufferedReader br = new BufferedReader(new InputStreamReader(in));
            dbConfigProp.load(in);
            Iterator<String> it = dbConfigProp.stringPropertyNames().iterator();
            while (it.hasNext()) {
                String key = it.next();
                userConfig.put(key, dbConfigProp.getProperty(key));
            }
            in.close();
            br.close();
        
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
    
    @Override
    public SampleResult runTest(JavaSamplerContext javaSamplerContext) {
        return null;
    }
    
    /**
     * Set params.
     * 
     * @param values
     * @return
     */
    public List setParams(List values) {
        List sqlValues = new ArrayList();
        if (values != null) {
            for (int i = 0; i < values.size(); i++) {
                sqlValues.add(values.get(i));
            }
        }
        return sqlValues;
    }
    
    /**
     * Convert params for sql which is for generating random values.
     * 
     * @param values
     * @param counter
     * @return
     */
    public List convertParams(List values, int counter) {
        List sqlValues = new ArrayList();
        if (values != null) {
            for (int i = 0; i < values.size(); i++) {
                if (values.get(i) instanceof Integer) {
                    sqlValues.add(counter);
                } else if(values.get(i) instanceof String){
                    sqlValues.add((String)values.get(i) + counter);
                }else {
                    sqlValues.add(values.get(i));
                }
            }
        }
        return sqlValues;
    }
    
    /**
     * Convert params without any modification.
     * 
     * @param values
     * @return
     */
    public List convertParams(List values) {
        List sqlValues = new ArrayList();
        if (values != null) {
            for (int i = 0; i < values.size(); i++) {
                sqlValues.add(values.get(i));
            }
        }
        return sqlValues;
    }
    
    public static void initBenchmarkVersion(){
        Properties dbConfigProp = new Properties();
        try {
            InputStream in = PropertiesUtil.class.getResourceAsStream("/config/benchmark-version.properties");
            BufferedReader br = new BufferedReader(new InputStreamReader(in));
            dbConfigProp.load(in);
            Iterator<String> it = dbConfigProp.stringPropertyNames().iterator();
            while (it.hasNext()) {
                String key = it.next();
                if("ss.benchmark.version".equals(key)){
                    benchmarkVersion = dbConfigProp.getProperty(key);
                    break;
                }
            }
            in.close();
            br.close();
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
    
    public int getInsertCount(String sql){
        String[] strs = sql.split("values");
        return strs[1].split("\\),").length;
    }
    
    public List<Long> batchInsert(ResultSet rs, int count) throws SQLException {
        List<Long> ids = new ArrayList<Long>(count);
        for(int i = 0; i < count; i++){
            ids.add(rs.getLong(1));
        }
        return ids;
    }
    
    public List<Long> batchInsert(int count, Connection connection, String insertSql, List insertParams) throws SQLException {
        ResultSet rs = null;
        List<Long> ids = new ArrayList<Long>(count);
        for(int i = 0; i < count; i++){
            rs = JDBCDataSourceUtil.insert(connection, insertSql, insertParams);
            rs.next();
            ids.add(rs.getLong(1));
        }
        return ids;
    }
    
    public List appendIds(List ids, List params){
        for (int i = 0; i < ids.size(); i++){
            params.add(ids.get(i));
        }
        
        return params;
    }
}

