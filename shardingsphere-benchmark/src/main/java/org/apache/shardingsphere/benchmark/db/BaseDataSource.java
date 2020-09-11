package org.apache.shardingsphere.benchmark.db;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

import javax.sql.DataSource;

public class BaseDataSource {

    /**
     * create HikariDataSource
     *
     * @param dataSourceName
     * @param host
     * @param port
     * @param userName
     * @param password
     * @return
     */
    public DataSource createHikariDataSource(String dataSourceName, String host, int port, String userName, String password) {
        HikariConfig config = new HikariConfig();
        config.setDriverClassName("com.mysql.jdbc.Driver");
        config.setJdbcUrl(String.format("jdbc:mysql://%s:%s/%s?useSSL=false&serverTimezone=UTC", host, port, dataSourceName));
        config.setUsername(userName);
        config.setPassword(password);
        config.setMaximumPoolSize(200);
        config.setConnectionTimeout(30000);
        config.setIdleTimeout(60000);
        config.setMaxLifetime(1800000);
        config.addDataSourceProperty("useServerPrepStmts", Boolean.TRUE.toString());
        config.addDataSourceProperty("cachePrepStmts", "true");
        config.addDataSourceProperty("prepStmtCacheSize", 250);
        config.addDataSourceProperty("prepStmtCacheSqlLimit", 2048);
        config.addDataSourceProperty("useLocalSessionState", Boolean.TRUE.toString());
        config.addDataSourceProperty("rewriteBatchedStatements", Boolean.TRUE.toString());
        config.addDataSourceProperty("cacheResultSetMetadata", Boolean.TRUE.toString());
        config.addDataSourceProperty("cacheServerConfiguration", Boolean.TRUE.toString());
        config.addDataSourceProperty("elideSetAutoCommits", Boolean.TRUE.toString());
        config.addDataSourceProperty("maintainTimeStats", Boolean.FALSE.toString());
        config.addDataSourceProperty("netTimeoutForStreamingResults", 0);
        return new HikariDataSource(config);
    }


    /**
     * create C3p0DataSource
     *
     * @param dataSourceName
     * @param host
     * @param port
     * @param userName
     * @param password
     * @return
     */
    public DataSource createC3p0DataSource(String dataSourceName, String host, int port, String userName, String password) {
        return null;
    }

    /**
     * create DhcpDataSource
     *
     * @param dataSourceName
     * @param host
     * @param port
     * @param userName
     * @param password
     * @return
     */
    public DataSource createDhcpDataSource(String dataSourceName, String host, int port, String userName, String password) {
        return null;

    }
}
