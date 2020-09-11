package org.apache.shardingsphere.benchmark.common.statistic;

import org.apache.shardingsphere.benchmark.bean.BenchmarkResultBean;
import org.apache.shardingsphere.benchmark.db.jdbc.JDBCDataSourceUtil;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class BenchmarkStatistic {
    
    public void updateBenchmarkRecordInDb(DataSource dataSource, List<BenchmarkResultBean> benchMarkResults, String sql){
        Connection connection = null;
        for(int i = 0; i < benchMarkResults.size(); i++){
            try {
                connection = dataSource.getConnection();
                if (benchMarkResults.get(i) != null) {
                    BenchmarkResultBean benchmarkResultBean = benchMarkResults.get(i);
                    List insertParams = Arrays.asList(
                            benchmarkResultBean.getProduct(),
                            benchmarkResultBean.getVersion(),
                            benchmarkResultBean.getScenario(),
                            benchmarkResultBean.getRules(),
                            (double)benchmarkResultBean.getBenchmarkResult().get("tps"),
                            (int)benchmarkResultBean.getBenchmarkResult().get("total"),
                            (double)benchmarkResultBean.getBenchmarkResult().get("tp50th"),
                            (double)benchmarkResultBean.getBenchmarkResult().get("tp90th"),
                            (double)benchmarkResultBean.getBenchmarkResult().get("tp95th"),
                            (double)benchmarkResultBean.getBenchmarkResult().get("maxCost"),
                            (double)benchmarkResultBean.getBenchmarkResult().get("minCost"),
                            benchmarkResultBean.getSql(),
                            benchmarkResultBean.getDbAction(),
                            benchmarkResultBean.getConcurrency(),
                            benchmarkResultBean.getUpdateTime(),
                            benchmarkResultBean.getTableShardingCount(),
                            benchmarkResultBean.getDbShardingCount());
                    JDBCDataSourceUtil.insert(connection, sql, insertParams);
                }
            } catch (SQLException ex) {
                ex.printStackTrace();
            } finally {
                if(null != connection){
                    try {
                        connection.close();
                    } catch (SQLException ex) {
                        ex.printStackTrace();
                    }
                }
            }
        }
    }
    
    public BenchmarkResultBean getTargetResult(DataSource dataSource, String sql, List params, List<BenchmarkResultBean> result){
        Connection connection = null;
        BenchmarkResultBean benchmarkResultBean = new BenchmarkResultBean();
        try {
            double totalTps = 0;
            int totalCount = 0;
            int totalRequestCount = 0;
            double totalMaxTime = 0;
            double totalMinTime = 0;
            double totalTp95Th = 0;
            double totalTp90Th = 0;
            double totalTp50Th = 0;
            connection = dataSource.getConnection();
            ResultSet rs = JDBCDataSourceUtil.select(connection, sql, params);
            while(rs.next()){
                totalCount =  totalCount + 1;
                benchmarkResultBean.setProduct(rs.getString(2));
                benchmarkResultBean.setVersion(rs.getString(3));
                benchmarkResultBean.setScenario(rs.getString(4));
                benchmarkResultBean.setRules(rs.getString(5));
                totalTps = totalTps + rs.getDouble(6);
                totalRequestCount = totalRequestCount + rs.getInt(7);
                totalTp50Th = totalTp50Th + rs.getDouble(8);
                totalTp90Th = totalTp50Th + rs.getDouble(9);
                totalTp95Th = totalTp50Th + rs.getDouble(10);
                totalMaxTime = totalMaxTime + rs.getDouble(11);
                totalMinTime = totalMinTime + rs.getDouble(12);
                benchmarkResultBean.setSql(rs.getString(13));
                benchmarkResultBean.setDbAction(rs.getString(14));
                benchmarkResultBean.setConcurrency(rs.getInt(15));
                benchmarkResultBean.setUpdateTime(rs.getLong(16));
                benchmarkResultBean.setDbShardingCount(rs.getInt(17));
                benchmarkResultBean.setTableShardingCount(rs.getInt(18));
            }
            if (totalCount > 0){
                Map benchmarkPerformanceData = new HashMap<>(1,1);
                benchmarkPerformanceData.put("tps", totalTps / totalCount);
                benchmarkPerformanceData.put("total" , totalRequestCount / totalCount);
                benchmarkPerformanceData.put("maxCost" , totalMaxTime / totalCount);
                benchmarkPerformanceData.put("minCost" , totalMinTime / totalCount);
                benchmarkPerformanceData.put("tp50th", totalTp50Th / totalCount);
                benchmarkPerformanceData.put("tp90th", totalTp90Th / totalCount);
                benchmarkPerformanceData.put("tp95th", totalTp95Th / totalCount);
                benchmarkResultBean.setBenchmarkResult(benchmarkPerformanceData);
                result.add(benchmarkResultBean);
            } else {
                return null;
            }
            
        } catch (SQLException ex) {
            ex.printStackTrace();
        } finally {
            try {
                connection.close();
            } catch (SQLException ex) {
                ex.printStackTrace();
            }
        }
        return benchmarkResultBean;
    }
}
