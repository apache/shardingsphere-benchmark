package org.apache.shardingsphere.benchmark.common.file.jmeter;

import java.io.*;
import java.util.*;

/**
 * Benchmark result parser for jtl.
 */
public final class BenchmarkResultParser {
    
    /**
     * Benchmark statistic.
     * @param filePath
     * @param skipBegin
     * @param skipEnd
     * @return
     */
    public static Map benchmarkStatistic(String filePath, int skipBegin, int skipEnd) {
        int failCount = 0;
        int successCount = 0;
        FileInputStream fileStream = null;
        Map result = new HashMap(1,1);
        List jMeterCostsList = new ArrayList<>(10);
        List jMeterTimeList = new ArrayList<>(10);
        
        if (new File(filePath).exists()) {
            try {
                int totalCount = 0;
                fileStream = new FileInputStream(filePath);
                InputStreamReader readStream = new InputStreamReader(fileStream);
                BufferedReader reader = new BufferedReader(readStream);
                String eachJMeterResult = "";
                while ((eachJMeterResult = reader.readLine()) != null) {
                    totalCount = totalCount + 1;
                    if (totalCount > skipBegin && totalCount < skipEnd) {
                        Map eachPerformanceInfo = BenchmarkResultRowParser.convertResult(eachJMeterResult);
                        jMeterCostsList.add(eachPerformanceInfo.get("jMeterCost"));
                        jMeterTimeList.add(eachPerformanceInfo.get("jMeterTime"));
                        if ("true".equals(eachPerformanceInfo.get("isJMeterSuccess"))) {
                            successCount = successCount + 1;
                        } else {
                            failCount = failCount + 1;
                        }
                    }
                }
                if (jMeterCostsList.size() > 0) {
                    int concurrentCount = jMeterCostsList.size();
                    double startTime = Double.valueOf((Double) jMeterTimeList.get(0)).doubleValue();
                    double endTime = Double.valueOf((Double) jMeterTimeList.get(concurrentCount - 1)).doubleValue();
                    double totalTimeCost = (endTime - startTime) / 1000;
                    double benchmarkTps = concurrentCount / totalTimeCost;
                    if (filePath.contains("insertupdatedelete")) {
                        benchmarkTps = benchmarkTps * 4;
                    }
                    Collections.sort(jMeterCostsList);
                    int tp50thIndex = (int) 0.5 * jMeterCostsList.size();
                    double tp50th = Double.valueOf((Double) jMeterCostsList.get(tp50thIndex)).doubleValue();
                    int tp90thIndex = (int) 0.9 * jMeterCostsList.size();
                    double tp90th = Double.valueOf((Double) jMeterCostsList.get(tp90thIndex)).doubleValue();
                    int tp95thIndex = (int) 0.95 * jMeterCostsList.size();
                    double tp95th = Double.valueOf((Double) jMeterCostsList.get(tp95thIndex)).doubleValue();
                    double maxCost = Double.valueOf((Double) jMeterCostsList.get(concurrentCount - 1)).doubleValue();
                    double minCost = Double.valueOf((Double) jMeterCostsList.get(0)).doubleValue();
                    result.put("tps", benchmarkTps);
                    result.put("total", concurrentCount);
                    result.put("tp50th", tp50th);
                    result.put("tp90th", tp90th);
                    result.put("tp95th", tp95th);
                    result.put("maxCost", maxCost);
                    result.put("minCost", minCost);
                }
            } catch (FileNotFoundException ex) {
                ex.printStackTrace();
            } catch (IOException ex) {
                ex.printStackTrace();
            }
        }
        return result;
    }
}


