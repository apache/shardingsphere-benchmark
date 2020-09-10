package org.apache.shardingsphere.benchmark.bean;

import java.util.Map;

/**
 * Benchmark result bean from jtl.
 */
public final class BenchmarkResultBean {
    
    public String version;
    public Map benchmarkResult;
    public String sql;
    public String rules;
    public String product;
    public String scenario;
    public String dbAction;
    public int concurrency;
    public long updateTime;
    public int dbShardingCount;
    public int tableShardingCount;
    
    public BenchmarkResultBean(){}
    
    public BenchmarkResultBean(String version, Map benchmarkResult, String sql, String rules, String product, String scenario, String dbAction, int concurrency, long updateTime, int dbShardingCount, int tableShardingCount){
        this.benchmarkResult = benchmarkResult;
        this.sql = sql;
        this.rules = rules;
        this.product = product;
        this.scenario = scenario;
        this.dbAction = dbAction;
        this.version = version;
        this.concurrency = concurrency;
        this.updateTime = updateTime;
        this.dbShardingCount = dbShardingCount;
        this.tableShardingCount = tableShardingCount;
    }
    
    public void setVersion(String version){
        this.version = version;
    }
    
    public void setSql(String sql){
        this.sql = sql;
    }
    
    public void setRules(String rules){
        this.rules = rules;
    }
    
    public void setProduct(String product){
        this.product = product;
    }
    
    public void setScenario(String scenario){
        this.scenario = scenario;
    }
    
    public void setDbAction(String dbAction){
        this.dbAction = dbAction;
    }
    
    public void setBenchmarkResult(Map benchmarkResult){
        this.benchmarkResult = benchmarkResult;
    }
    
    public String getVersion(){
        return this.version;
    }
    
    public Map getBenchmarkResult(){
        return this.benchmarkResult;
    }
    
    public String getScenario(){
        return this.scenario;
    }
    
    public String getRules(){
        return this.rules;
    }
    
    public String getSql(){
        return this.sql;
    }
    
    public String getProduct(){
        return this.product;
    }
    
    public String getDbAction(){
        return this.dbAction;
    }
    
    public void setConcurrency(int concurrency){
        this.concurrency = concurrency;
    }
    
    public int getConcurrency(){
        return this.concurrency;
    }
    
    public void setUpdateTime(long updateTime){
        this.updateTime = updateTime;
    }
    
    public long getUpdateTime(){
        return this. updateTime;
    }
    
    public void setDbShardingCount(int dbShardingCount){
        this.dbShardingCount = dbShardingCount;
    }
    
    public int getDbShardingCount(){
        return this.dbShardingCount;
    }
    
    public void setTableShardingCount(int tableShardingCount){
        this.tableShardingCount = tableShardingCount;
    }
    
    public int getTableShardingCount(){
        return this.tableShardingCount;
    }
}
