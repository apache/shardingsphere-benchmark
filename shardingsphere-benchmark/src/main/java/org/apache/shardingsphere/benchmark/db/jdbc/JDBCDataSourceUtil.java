package org.apache.shardingsphere.benchmark.db.jdbc;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

import javax.sql.DataSource;
import java.sql.*;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class JDBCDataSourceUtil {

    private static final Map<String, DataSource> DATASOURCES = new HashMap<>();

    /**
     *  Init datasource.
     *
     * @param dataSourceName
     * @param host
     * @param port
     * @param userName
     * @param password
     * @return
     */
    public static void initDb(String dataSourceName, String host, int port, String userName, String password) {
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
        DataSource dataSource = new HikariDataSource(config);
        DATASOURCES.put(dataSourceName, dataSource);
    }


    /**
     * Get dataSource by its name.
     *
     * @param dataSourceName
     * @return
     */
    public static DataSource getDataSource(String dataSourceName) {
        return DATASOURCES.get(dataSourceName);
    }


    /**
     * create an item by criteria.
     * @param conn
     * @param insertSql
     * @param params
     * @return
     * @throws SQLException
     */
    public static ResultSet insert(Connection conn, String insertSql, List params) throws SQLException {

        ResultSet result = null;
        Connection connection = null;
        PreparedStatement preparedStatement=null;

        if(conn != null){
            preparedStatement = conn.prepareStatement(insertSql, Statement.RETURN_GENERATED_KEYS);
            preparedStatement = setParams(preparedStatement, params);
            preparedStatement.execute();
            result = preparedStatement.getGeneratedKeys();
        }

        return result;
    }


    /**
     * Update an item by criteria..
     * @param conn
     * @param updateSql
     * @param params
     * @return
     * @throws SQLException
     */
    public static ResultSet update(Connection conn, String updateSql, List params) throws SQLException {
        ResultSet result = null;
        PreparedStatement preparedStatement=null;

        if(conn != null){
            preparedStatement = conn.prepareStatement(updateSql);
            preparedStatement = setParams(preparedStatement, params);
            preparedStatement.executeUpdate();
        }

        return result;
    }


    /**
     * Delete an item by criteria.
     *
     * @param conn
     * @param deleteSql
     * @return
     * @throws SQLException
     */
    public static ResultSet delete(Connection conn, String deleteSql, List params) throws SQLException {
        ResultSet result = null;
        PreparedStatement preparedStatement=null;

        if(conn != null){
            preparedStatement = conn.prepareStatement(deleteSql);
            preparedStatement = setParams(preparedStatement, params);
            preparedStatement.executeUpdate();
        }

        return result;
    }


    /**
     * Query items by criteria.
     *
     * @param conn
     * @param selectSql
     */
    public static ResultSet select(Connection conn, String selectSql, List params) throws SQLException {
        ResultSet result = null;
        PreparedStatement preparedStatement=null;

        if(conn != null){
            preparedStatement = conn.prepareStatement(selectSql);
            preparedStatement = setParams(preparedStatement, params);
            preparedStatement.executeQuery();
        }

        return result;

    }

    /**
     * Allocate values to PreparedStatement.
     *
     * @param ps
     * @param params
     * @return
     * @throws SQLException
     */
    public static PreparedStatement setParams(PreparedStatement ps, List params) throws SQLException {
        if(params != null){
            for (int i = 0; i < params.size(); i++){
                if (params.get(i) instanceof Long){
                    ps.setLong(i+1, (Long) params.get(i));
                } else if(params.get(i) instanceof Integer){
                    ps.setInt(i+1, (Integer) params.get(i));
                } else if(params.get(i) instanceof  String){
                    ps.setString(i+1, (String)params.get(i));
                }
            }
        }
        return ps;
    }


    /**
     * Close database connection.
     *
     * @param conn
     * @throws SQLException
     */
    public static void close(Connection conn) throws SQLException {
        if(conn != null){
            conn.close();
        }
    }

}
