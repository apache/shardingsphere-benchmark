package org.apache.shardingsphere.benchmark.common.file.yaml;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileFilter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Benchmark config yaml
 */
public final class BenchmarkConfigYaml {
    
    public static FileFilter yamlFilter = new BenchmarkConfigYaml.YamlFileFilter();
    
    /**
     * Modify benchmark yaml file
     * @param benchmarkBasePath
     * @param shardingDbCount
     * @param shardingTableCount
     * @param maxConnectionCount
     * @param minConnectionCount
     * @param maxConnectionPerQuery
     */
    public static void modifyBenchmarkYamlFile(String benchmarkBasePath, int shardingDbCount, int shardingTableCount, int maxConnectionCount, int minConnectionCount, int maxConnectionPerQuery){
        String line = "";
        File yamlFile = null;
        List<File> yamlFileList = getYamlFileList(benchmarkBasePath);
        try {
            for(int i = 0; i < yamlFileList.size(); i++) {
                yamlFile = yamlFileList.get(i);
                StringBuffer bufAll = new StringBuffer();
                BufferedReader br = new BufferedReader(new FileReader(yamlFile));
                while ((line = br.readLine()) != null) {
                    if (line.contains("${0..2}.sbtest${0..99}")){
                        line = line.replace("${0..2}.sbtest${0..99}", "${0.." + (shardingDbCount-1) + "}.sbtest${0.." + (shardingTableCount-1) + "}");
                    }
                    if (line.contains("maximumPoolSize: 200")){
                        line = line.replaceAll("maximumPoolSize: 200", "maximumPoolSize: " + maxConnectionCount);
                    }
                    if (line.contains("{id % 3}")) {
                        line = line.replace("{id % 3}", "{id % " + shardingDbCount + "}");
                    }
                    if (line.contains("{k % 100}")){
                        line = line.replace("{k % 100}", "{k % " + shardingTableCount + "}");
                    } 
                    if (line.contains("max.connections.size.per.query: 2")){
                        line = line.replace("max.connections.size.per.query: 2", "max.connections.size.per.query: " + maxConnectionPerQuery);
                    }
                    if (line.contains("maxPoolSize: 200")){
                        line = line.replaceAll("maxPoolSize: 200", "maxPoolSize: " + maxConnectionCount);
                    }
                    if (line.contains("minPoolSize: 200")){
                        line = line.replaceAll("minPoolSize: 200", "minPoolSize: " + minConnectionCount);
                    }
    
                    bufAll.append(line);
                    bufAll.append(System.getProperty("line.separator"));
                }
                br.close();
                BufferedWriter bw = new BufferedWriter(new FileWriter(yamlFile));
                bw.write(bufAll.toString());
                bw.close();
            } 
        } catch (IOException ex) {
            ex.printStackTrace();
        } finally {
        }
    }
    
    /**
     * Get all of yaml files.
     * @param benchmarkBasePath
     * @return
     */
    public static List<File> getYamlFileList(String benchmarkBasePath){
        String yamlBasePath =  benchmarkBasePath + "/src/main/resources/yaml";
        File yamlBaseDir = new File(yamlBasePath);
        List<File> yamlFileList = new ArrayList<File>(10);
        return filterTargetFiles(yamlBaseDir, yamlFilter, yamlFileList);
    }
    
    /**
     * File target files with its suffix.
     * @param targetDir
     * @param filter
     * @param resultFiles
     * @return
     */
    public static List<File> filterTargetFiles(File targetDir, FileFilter filter, List<File> resultFiles) {
        File[] files = targetDir.listFiles(filter);
        for (File file : files) {
            if (file.isDirectory()) {
                filterTargetFiles(file, filter, resultFiles);
            } else {
                resultFiles.add(file);
            }
        }
        return resultFiles;
    }
    
    /**
     * Yaml file filter.
     */
    private static class YamlFileFilter implements FileFilter {
        
        @Override
        public boolean accept(File file) {
            if (file.isDirectory()) {
                return true;
            }
            String fileName = file.getName();
            return fileName.matches("(?i).+yaml$");
        }
    }
    
    public static void main(String[] args) {
        String path = "D:\\shardingsphere-benchmark\\src\\main\\resources\\yaml\\fullrouting\\sharding-masterslave-encrypt\\shardingjdbc\\config-shardingjdbc-fullrouting-sharding-masterslave-encrypt-test.yaml";
        String benchmarkBasePath = "D:/shardingsphere-benchmark";
        modifyBenchmarkYamlFile(benchmarkBasePath, 1000,1000, 99, 9, 8);
    }
}

