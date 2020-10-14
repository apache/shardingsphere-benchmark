package org.apache.shardingsphere.benchmark.common.file.jmeter;

import java.util.HashMap;
import java.util.Map;

/**
 * Benchmark result row parser.
 */
public final class BenchmarkResultRowParser {
    
    /**
     * Convert result of each row for jtl.
     * @param eachJMeterResult
     * @return
     */
    public static Map convertResult(String eachJMeterResult) {
        Map performanceInfo = new HashMap<>(1,1);
        String[] eachJMeterDetails = eachJMeterResult.split(",");
        performanceInfo.put("jMeterTime", Double.valueOf(eachJMeterDetails[0]).doubleValue());
        performanceInfo.put("jMeterCost", Double.valueOf(eachJMeterDetails[1]).doubleValue());
        performanceInfo.put("isJMeterSuccess", (String) eachJMeterDetails[7]);
        return performanceInfo;
    }
}
