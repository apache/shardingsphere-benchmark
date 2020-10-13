package org.apache.shardingsphere.benchmark.jmeter.statistic;

import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import org.apache.shardingsphere.benchmark.bean.BenchmarkResultBean;
import org.apache.shardingsphere.benchmark.common.file.excel.BenchmarkExcelWriter;
import org.apache.shardingsphere.benchmark.common.statistic.BenchmarkFullroutingStatistic;
import org.apache.shardingsphere.benchmark.common.statistic.BenchmarkRangeroutingStatistic;
import org.apache.shardingsphere.benchmark.common.statistic.BenchmarkSingleroutingStatistic;
import org.apache.shardingsphere.benchmark.db.jdbc.JDBCDataSourceUtil;
import org.apache.shardingsphere.benchmark.jmeter.JMeterBenchmarkBase;

import javax.sql.DataSource;
import java.util.Arrays;
import java.util.List;

/**
 * JMeter test plan for benchmark statistic.
 */
public class JMeterShardigSphereBenchmarkStatistic extends JMeterBenchmarkBase {
    
    public static DataSource dataSource;
    static {
        dataSource = JDBCDataSourceUtil.initDb((String) userConfig.get("shardingsphere.benchmark.result.datasource"),
                (String) userConfig.get("shardingsphere.benchmark.result.database.host"), Integer.valueOf((String)userConfig.get("shardingsphere.benchmark.result.database.port")).intValue(),
                (String) userConfig.get("shardingsphere.benchmark.result.database.username"), (String) userConfig.get("shardingsphere.benchmark.result.database.password"));
    }
    
    @Override
    public SampleResult runTest(JavaSamplerContext context) {
        
        SampleResult results = new SampleResult();
        results.setSampleLabel("JMeterSSBenchmarkStatistic");
        results.sampleStart();
        int concurrency = Integer.valueOf((String)userConfig.get("shardingsphere.jmeter.concurrency.count")).intValue();
        String[] sampleArray = ((String)userConfig.get("shardingsphere.result.sample.amount")).split(",");
        int skipBegin = Integer.valueOf(sampleArray[0]).intValue();
        int skipEnd = Integer.valueOf(sampleArray[1]).intValue();
        long updateTime = System.currentTimeMillis();
        int dbShardingCount = Integer.valueOf((String)userConfig.get("shardingsphere.sharding.db.count")).intValue();
        int tableShardingCount = Integer.valueOf((String)userConfig.get("shardingsphere.sharding.table.count")).intValue();
        String benchmarkVersion = (String)userConfig.get("shardingsphere.version");
        String benchmarkInsertSql = (String) sqlConfig.get("ss.benchmark.result.insert.sql");
        String benchmarkAvgInsertSql = (String)sqlConfig.get("ss.benchmark.avg.result.insert.sql");
        String currentTime = String.valueOf(System.currentTimeMillis());
        BenchmarkFullroutingStatistic benchmarkFullroutingStatistic= new BenchmarkFullroutingStatistic();
        List<BenchmarkResultBean> fullRoutingResult = benchmarkFullroutingStatistic.calculateFullroutingScenarioResult(benchmarkResultPath, sqlConfig, benchmarkVersion, skipBegin, skipEnd, concurrency, updateTime, dbShardingCount, tableShardingCount);
        BenchmarkRangeroutingStatistic benchmarkRangeroutingStatistic = new BenchmarkRangeroutingStatistic();
        List<BenchmarkResultBean> rangeRoutingResult = benchmarkRangeroutingStatistic.calculateRangeRoutingScenarioResult(benchmarkResultPath, sqlConfig, benchmarkVersion, skipBegin, skipEnd, concurrency, updateTime, dbShardingCount, tableShardingCount);
        BenchmarkSingleroutingStatistic benchmarkSingleroutingStatistic = new BenchmarkSingleroutingStatistic();
        List<BenchmarkResultBean> singleRoutingResult = benchmarkSingleroutingStatistic.calculateSingleRoutingScenarioResult(benchmarkResultPath, sqlConfig, benchmarkVersion, skipBegin, skipEnd, concurrency, updateTime, dbShardingCount, tableShardingCount);
        benchmarkFullroutingStatistic.updateBenchmarkRecordInDb(dataSource, fullRoutingResult, benchmarkInsertSql);
        benchmarkRangeroutingStatistic.updateBenchmarkRecordInDb(dataSource, rangeRoutingResult, benchmarkInsertSql);
        benchmarkSingleroutingStatistic.updateBenchmarkRecordInDb(dataSource, singleRoutingResult, benchmarkInsertSql);
        List shardingParams = Arrays.asList(benchmarkVersion, tableShardingCount, dbShardingCount, concurrency);
        List noShardingParams = Arrays.asList(benchmarkVersion, 0, 0, concurrency);
    
        List<BenchmarkResultBean> fullRoutingCalResult = benchmarkFullroutingStatistic.calculateFullroutingScenarioAvgResult(dataSource, sqlConfig, noShardingParams, shardingParams);
        List<BenchmarkResultBean> rangeRoutingCalResult = benchmarkRangeroutingStatistic.calculateRangeroutingScenarioAvgResult(dataSource, sqlConfig, noShardingParams, shardingParams);
        List<BenchmarkResultBean> singleRoutingCalResult = benchmarkSingleroutingStatistic.calculateSingleroutingScenarioAvgResult(dataSource, sqlConfig, noShardingParams, shardingParams);
    
        benchmarkFullroutingStatistic.updateBenchmarkRecordInDb(dataSource, fullRoutingCalResult, benchmarkAvgInsertSql);
        benchmarkRangeroutingStatistic.updateBenchmarkRecordInDb(dataSource, rangeRoutingCalResult, benchmarkAvgInsertSql);
        benchmarkSingleroutingStatistic.updateBenchmarkRecordInDb(dataSource, singleRoutingCalResult, benchmarkAvgInsertSql);
        
        String resultExcelPath = (String)userConfig.get("shardingsphere.benchmark.result.base.path") + "/" + (String)userConfig.get("shardingsphere.benchmark.result.excel.name");
        String resultAvgExcelPath = (String)userConfig.get("shardingsphere.benchmark.result.base.path")  + "/" + (String)userConfig.get("shardingsphere.benchmark.avg.result.excel.name");
        BenchmarkExcelWriter.clearExportExcel(resultExcelPath);
        BenchmarkExcelWriter.writeExcel(resultExcelPath, "full-routing-" + currentTime, true, 1, fullRoutingResult);
        BenchmarkExcelWriter.writeExcel(resultExcelPath, "range-routing-" + currentTime, true, 1, rangeRoutingResult);
        BenchmarkExcelWriter.writeExcel(resultExcelPath, "single-routing-" + currentTime, true, 1, singleRoutingResult);
        BenchmarkExcelWriter.clearExportExcel(resultAvgExcelPath);
        BenchmarkExcelWriter.writeExcel(resultAvgExcelPath, "full-routing-" + currentTime, true, 1, fullRoutingCalResult);
        BenchmarkExcelWriter.writeExcel(resultAvgExcelPath, "range-routing-" + currentTime, true, 1, rangeRoutingCalResult);
        BenchmarkExcelWriter.writeExcel(resultAvgExcelPath, "single-routing-" + currentTime, true, 1, singleRoutingCalResult);
        
        results.sampleEnd();
        return results;
    }
}