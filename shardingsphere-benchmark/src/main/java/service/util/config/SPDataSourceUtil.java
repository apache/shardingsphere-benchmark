/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package service.util.config;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

import javax.sql.DataSource;
import java.sql.PreparedStatement;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import perfstmt.ShardingPerfStmt;
import service.api.entity.Iou;

/**
 * data source utils for sharding proxy.
 * @author nancyzrh
 */
public class SPDataSourceUtil {
    private static final String DEFAULT_SCHEMA = "test";
    
    private static final Map<String, DataSource> DATA_SOURCE_MAP = new HashMap<>();
    
    /**
     * create datasource.
     * @param usrName user name
     * @param dataSourceName datasource name
     * @param host host ip
     * @param port port
     * @param password pwd
     */
    public static void createDataSource(final String usrName, final String dataSourceName, final String host, final int port, final String password) {
        HikariConfig config = new HikariConfig();
        config.setDriverClassName("com.mysql.jdbc.Driver");
        config.setJdbcUrl(String.format("jdbc:mysql://%s:%s/%s?useSSL=false&serverTimezone=UTC", host, port, dataSourceName));
        config.setUsername(usrName);
        config.setPassword(password);
        config.setMaximumPoolSize(200);
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
        DATA_SOURCE_MAP.put(dataSourceName, dataSource);
    }
    
    /**
     * get datasource from name.
     * @param dataSourceName input name
     * @return datasource
     */
    public static DataSource getDataSource(final String dataSourceName) {
        return DATA_SOURCE_MAP.get(dataSourceName);
    }
    
    /**
     * create schema if db don't exist.
     * @param dataSourceName datasource name
     */
    public static void createSchema(final String dataSourceName) {
        String sql = "CREATE DATABASE " + dataSourceName;
        try (Connection connection = getDataSource(DEFAULT_SCHEMA).getConnection();
             Statement statement = connection.createStatement()) {
            statement.execute(sql);
        } catch (final SQLException ignored) {
        }
    }
    
    /**
     * insert data for update.
     * @param sql input sql
     * @param datasource datasource
     * @throws SQLException sql exception
     */
    public static void insertDemo(final String sql, final String datasource) throws SQLException {
        try (Connection connection = getDataSource(datasource).getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            preparedStatement.setInt(1, 1);
            preparedStatement.setInt(2, 2);
            preparedStatement.setString(3, "##-####");
            preparedStatement.setString(4, "##-####");
            preparedStatement.execute();
        } catch (final SQLException ignored) {
        }
    }
    
    /**
     * select result.
     * @param sql input sql
     * @param datasource datasource
     * @return list result
     * @throws SQLException ex
     */
    public static List<Iou> getIou(final String sql, final String datasource) throws SQLException {
        List<Iou> result = new LinkedList<>();
        try (Connection connection = getDataSource(datasource).getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql);
             ResultSet resultSet = preparedStatement.executeQuery()) {
            while (resultSet.next()) {
                Iou iou = new Iou();
                iou.setK(resultSet.getInt(2));
                result.add(iou);
            }
        }
        return result;
    }
    
    /**
     * Insert+Update+Delete as one operation
     * @param datasource
     * @throws SQLException
     */
    public static void  writeOp(final String datasource) throws SQLException {
        String sqlStmt = ShardingPerfStmt.INSERT_SQL_STMT.getValue();
        Long id = Long.MIN_VALUE;
        try (Connection connection = getDataSource(datasource).getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sqlStmt, Statement.RETURN_GENERATED_KEYS)) {
            preparedStatement.setInt(1, 1);
            preparedStatement.setString(2, "##-####");
            preparedStatement.setString(3, "##-####");
            preparedStatement.executeUpdate();
            ResultSet result = preparedStatement.getGeneratedKeys();
            result.next();
            id = result.getLong(1);
        }catch (final SQLException ex) {
            ex.printStackTrace();
        }
        sqlStmt = ShardingPerfStmt.UPDATE_SQL_STMT.getValue();
        try (Connection connection = getDataSource(datasource).getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sqlStmt)) {
            preparedStatement.setString(1,"##-#####");
            preparedStatement.setString(2,"##-#####");
            preparedStatement.setLong(3, id);
            preparedStatement.setInt(4,1);
            preparedStatement.executeUpdate();
        }
        sqlStmt = ShardingPerfStmt.DELETE_SQL_STMT.getValue();
        try (Connection connection = getDataSource(datasource).getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sqlStmt)) {
            preparedStatement.setInt(1,1);
            preparedStatement.setLong(2, id);
            preparedStatement.executeUpdate();
        }
    }
    /**
     * update stmt.
     * @param sql input stmt
     * @param datasource datasource
     * @return execute result
     * @throws SQLException ex
     */
    public static int updateStmt(final String sql, final String datasource) throws SQLException {
        try (Connection connection = getDataSource(datasource).getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            preparedStatement.setString(1, "##-#####");
            preparedStatement.setString(2, "##-#####");
            preparedStatement.setInt(3, 1);
            preparedStatement.setInt(4, 2);
            return preparedStatement.executeUpdate();
        }
    }
    
    /**
     * insert data.
     * @param sql input stmt
     * @param datasource datasource
     * @throws SQLException ex
     */
    public static void insertIou(final String sql, final String datasource) throws SQLException {
        try (Connection connection = getDataSource(datasource).getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            preparedStatement.setInt(1, 2);
            preparedStatement.setString(2, "##-####");
            preparedStatement.setString(3, "##-####");
            preparedStatement.execute();
        } catch (final SQLException ex) {
            ex.printStackTrace();
        }
    }
    
    /**
     * clean up.
     * @param sql input stmt
     * @param datasource datasource
     * @throws SQLException ex
     */
    public static void clean(final String sql, final String datasource) throws SQLException {
        try (Connection connection = getDataSource(datasource).getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            preparedStatement.executeUpdate();
        }
    }
    
    /**
     * delete target data.
     * @param sql input stmt
     * @param datasource datasource
     * @return execute
     * @throws SQLException ex
     */
    public static int deleteIou(final String sql, final String datasource) throws SQLException {
        try (Connection connection = getDataSource(datasource).getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            preparedStatement.setInt(1, 2);
            preparedStatement.setInt(2, 1);
            return preparedStatement.executeUpdate();
        }
    }
}
