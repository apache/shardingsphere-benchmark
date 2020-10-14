package org.apache.shardingsphere.benchmark.common.file.properties;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Iterator;
import java.util.Properties;

/**
 * Benchmark config properties.
 */
public final class BenchmarkConfigProperties {
    
    /**
     * Get all of benchmark output paths of result for jtl.
     * @param benchmarkBasePath
     * @param outputBasePath
     * @return
     */
    public static Properties getBenchmarkOutputConfig(String benchmarkBasePath, String outputBasePath) {
        Properties result = new Properties();
        try {
            InputStream in = BenchmarkConfigProperties.class.getResourceAsStream("/config/benchmark-result.properties");
            BufferedReader br = new BufferedReader(new InputStreamReader(in));
            result.load(in);
            Iterator<String> it = result.stringPropertyNames().iterator();
            while (it.hasNext()) {
                String key = it.next();
                String value = result.getProperty(key);
                value = value.replace("/export/shardingsphere-benchmark/result", outputBasePath);
                result.setProperty(key, value);
            }
            in.close();
            br.close();
            
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        return result;
    }
    
    /**
     * Modify benchmark output path with root directory configured from user-config.properties.
     * @param benchmarkBasePath
     * @param outputBasePath
     */
    public static void modifyBenchmarkOutputConfig(String benchmarkBasePath, String outputBasePath){
         FileOutputStream fileOutputStream = null;
        try {
            Properties benchmarkResultConfigProp = getBenchmarkOutputConfig(benchmarkBasePath, outputBasePath);
            String filePath = benchmarkBasePath + "/src/main/resources/config/benchmark-result.properties";
            fileOutputStream = new FileOutputStream(filePath);
            benchmarkResultConfigProp.store(fileOutputStream, "");
        } catch (FileNotFoundException ex) {
            ex.printStackTrace();
        } catch (IOException ex) {
            ex.printStackTrace();
        } finally {
            try {
                fileOutputStream.close();
            } catch (IOException ex) {
                ex.printStackTrace();
            }
        }
    }
    
    public static void main(String[] args){
        String benchmarkBasePath = "D:/shardingsphere-benchmark";
        String outputBasePath = "/export/shardingsphere-benchmark/result";
        modifyBenchmarkOutputConfig(benchmarkBasePath, outputBasePath);
    }
}
