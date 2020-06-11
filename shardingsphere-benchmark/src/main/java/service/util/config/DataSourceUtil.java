package service.util.config;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import service.api.entity.Iou;
import java.util.Date;
import java.text.SimpleDateFormat;
import javax.sql.DataSource;
import java.sql.*;
import java.util.*;

/**
 * dataSource util
 * @author nancyzrh
 */
public class DataSourceUtil {
    private static final String USER_NAME = "root";
    private static final String DEFAULT_SCHEMA = "test";
    private static final Map<String, DataSource> datasourceMap = new HashMap<>();

    /**
     * create data source
     * @param dataSourceName
     * @param host
     * @param port
     * @param password
     */
    public static void createDataSource(final String dataSourceName, final String host, final int port, final String password) {
        HikariConfig config = new HikariConfig();
        config.setDriverClassName("com.mysql.jdbc.Driver");
        config.setJdbcUrl(String.format("jdbc:mysql://%s:%s/%s?useSSL=false&serverTimezone=UTC", host, port, dataSourceName));
        config.setUsername(USER_NAME);
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
        datasourceMap.put(dataSourceName, dataSource);
    }

    /**
     * get datasource
     * @param dataSourceName
     * @return datasource
     */
    public static DataSource getDataSource(final String dataSourceName) {
        return datasourceMap.get(dataSourceName);
    }

    /**
     * create default schema
     * @param dataSourceName
     */
    public static void createSchema(final String dataSourceName) throws SQLException {
        String sql = "CREATE DATABASE " + dataSourceName;
        Connection connection = null;
        try {
            connection = getDataSource(DEFAULT_SCHEMA).getConnection();
            Statement statement = connection.createStatement();
            statement.execute(sql);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if(connection != null){
                connection.close();
            }
        }
    }

    public static void main(String args[]) {
        DataSourceUtil.createDataSource("sharding_db", "10.222.16.178", 3307, "root");
        String insertSql = "INSERT INTO sbtest99 (id,k,c,pad) VALUES (?,?,?,?)";
        try {
            DataSourceUtil.insertDemo(insertSql, "sharding_db");
        } catch (final SQLException ex) {
        
        }
    }
    /**
     * insert demo data
     * @param sql
     * @param datasource
     * @throws SQLException
     */
    public static void insertDemo(final String sql, String datasource) throws SQLException {
        Connection connection = null;
        try {
            connection = getDataSource(datasource).getConnection();
            PreparedStatement preparedStatement = connection.prepareStatement(sql);
            preparedStatement.setInt(1, 1);
            preparedStatement.setInt(2, 3);
            preparedStatement.setString(3, "##-####");
            preparedStatement.setString(4, "##-####");
            preparedStatement.execute();
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if(connection != null){
                connection.close();
            }
        }
    }

    /**
     * for select
     * @param sql
     * @param datasource
     * @return
     * @throws SQLException
     */
    public static List<Iou> getIou(final String sql, String datasource) throws SQLException {
        List<Iou> result = new LinkedList<>();
        Connection connection = null;
        try {
            connection = getDataSource(datasource).getConnection();
            PreparedStatement preparedStatement = connection.prepareStatement(sql);
            ResultSet resultSet = preparedStatement.executeQuery();
            while (resultSet.next()) {
                Iou iou = new Iou();
                iou.setK(resultSet.getInt(2));
                result.add(iou);
            }
        } catch (Exception e){
            e.printStackTrace();
        } finally {
            if(connection != null){
                connection.close();
            }
        }
        return result;
    }
    public static void  singleMsAll(String datasource) throws SQLException {
        String sql = "INSERT INTO sbtest3 (k,c,pad) VALUES (?,?,?)";
        Long id =  Long.MIN_VALUE;
        Connection connection=null;
        PreparedStatement preparedStatement=null;
        ResultSet resultSet= null;
        try {
            connection = getDataSource(datasource).getConnection();
            preparedStatement = connection.prepareStatement(sql,Statement.RETURN_GENERATED_KEYS);
            preparedStatement.setInt(1, 3);
            preparedStatement.setString(2, "##-####");
            preparedStatement.setString(3, "##-####");
            preparedStatement.executeUpdate();
            ResultSet result = preparedStatement.getGeneratedKeys();
            result.next();
            id = result.getLong(1);
            //sql = "select id,k from sbtest3 ignore index(`PRIMARY`) where id=1 and k=3";
            //preparedStatement = connection.prepareStatement(sql);
            //preparedStatement.setLong(1,id);
            //preparedStatement.executeQuery();
            //sql = "select id,k from sbtest3 ignore index(`PRIMARY`) where id=2 and k=3";
            //preparedStatement = connection.prepareStatement(sql);
            //preparedStatement.setLong(1,id);
            //preparedStatement.executeQuery();
            sql = "select count(id) from sbtest3";
            preparedStatement = connection.prepareStatement(sql);
            //preparedStatement.setLong(1,id);
            preparedStatement.executeQuery();
            sql = "select max(id) from sbtest3 ignore index(`PRIMARY`)";
            //sql = "select max(id) from sbtest3";
            preparedStatement = connection.prepareStatement(sql);
            //preparedStatement.setLong(1,id);
            preparedStatement.executeQuery();
    //        sql = "update sbtest3 set c=?,pad =? where id=? and k=3";
    //        preparedStatement = connection.prepareStatement(sql);
    //        preparedStatement.setString(1,"new");
    //        preparedStatement.setString(2,"new");
    //        preparedStatement.setLong(3,id);
            //preparedStatement.setInt(4,3);
    //        preparedStatement.executeUpdate();
            sql = "delete from sbtest3 where k=? and id=?";
            preparedStatement = connection.prepareStatement(sql);
            preparedStatement.setInt(1,3);
            preparedStatement.setLong(2,id);
            preparedStatement.executeUpdate();
            preparedStatement.close();
            result.close();
        }catch (final SQLException ex) {
            ex.printStackTrace();
        } finally {
            connection.close();
        }
    }

    public static void  singleAll(String datasource) throws SQLException {
        String sql = "INSERT INTO sbtest3 (k,c,pad) VALUES (?,?,?)";
        Long id =  Long.MIN_VALUE;
        Connection connection=null;
        PreparedStatement preparedStatement=null;
        ResultSet resultSet= null;
        try {
            connection = getDataSource(datasource).getConnection();
            preparedStatement = connection.prepareStatement(sql,Statement.RETURN_GENERATED_KEYS);
            preparedStatement.setInt(1, 3);
            preparedStatement.setString(2, "##-####");
            preparedStatement.setString(3, "##-####");
            preparedStatement.executeUpdate();
            ResultSet result = preparedStatement.getGeneratedKeys();
            result.next();
            id = result.getLong(1);
            sql = "update sbtest3 set c=?,pad =? where id=? and k=3";
            preparedStatement = connection.prepareStatement(sql);
            preparedStatement.setString(1,"new");
            preparedStatement.setString(2,"new");
            preparedStatement.setLong(3,id);
            //preparedStatement.setInt(4,3);
            preparedStatement.executeUpdate();
            sql = "delete from sbtest3 where k=? and id=?";
            preparedStatement = connection.prepareStatement(sql);
            preparedStatement.setInt(1,3);
            preparedStatement.setLong(2,id);
            preparedStatement.executeUpdate();
            preparedStatement.close();
            result.close();
        }catch (final SQLException ex) {
            ex.printStackTrace();
        } finally {
            connection.close();
            //preparedStatement.close();
            //resultSet.close();
            //connection=null;
        }
    }
    /**
     * operation
     */
    public static void  singleAllbak(String datasource) throws SQLException {
        String sql = "INSERT INTO sbtest3 (k,c,pad) VALUES (?,?,?)";
        long id = Long.MIN_VALUE;
        Connection connection  = null;
        try {
            connection = getDataSource(datasource).getConnection();
            PreparedStatement preparedStatement = connection.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS);
            preparedStatement.setInt(1, 1);
            preparedStatement.setString(2, "##-####");
            preparedStatement.setString(3, "##-####");
            preparedStatement.executeUpdate();
            ResultSet result = preparedStatement.getGeneratedKeys();
            result.next();
            id = result.getLong(1);

            sql = "update sbtest3 set c=?,pad =? where id=? and k=?";
            preparedStatement = connection.prepareStatement(sql);;
            preparedStatement.setString(1,"new");
            preparedStatement.setString(2,"new");
            preparedStatement.setLong(3,id);
            preparedStatement.setInt(4,1);

            sql="delete from sbtest3 where k=? and id=?";
            preparedStatement = connection.prepareStatement(sql);
            preparedStatement.setInt(1,1);
            preparedStatement.setLong(2,id);
            preparedStatement.executeUpdate();

        }catch (final SQLException ex) {
            ex.printStackTrace();
        } finally {
            if(connection != null){
                connection.close();
            }
        }
    }
    
    public static void  changeAll(String datasource) throws SQLException {
        String sql = "INSERT INTO sbtest (k,c,pad) VALUES (?,?,?)";
        Long id =  Long.MIN_VALUE;
        Connection connection = null;
        PreparedStatement preparedStatement=null;
        ResultSet resultSet= null;
        try {
            connection = getDataSource(datasource).getConnection();
            preparedStatement = connection.prepareStatement(sql,Statement.RETURN_GENERATED_KEYS);
            preparedStatement.setInt(1, 3);
            preparedStatement.setString(2, "##-####");
            preparedStatement.setString(3, "##-####");
            preparedStatement.executeUpdate();
            ResultSet result = preparedStatement.getGeneratedKeys();
            result.next();
            id = result.getLong(1);
            sql = "update sbtest set c=?,pad =? where id=? and k=3";
            preparedStatement = connection.prepareStatement(sql);
            preparedStatement.setString(1,"new");
            preparedStatement.setString(2,"new");
            preparedStatement.setLong(3,id);
            //preparedStatement.setInt(4,3);
            preparedStatement.executeUpdate();
            sql = "delete from sbtest where k=? and id=?";
             preparedStatement = connection.prepareStatement(sql);
            preparedStatement.setInt(1,3);
            preparedStatement.setLong(2,id);
            preparedStatement.executeUpdate();
        }catch (Exception ex) {
            ex.printStackTrace();
        } finally {
            if (connection != null) {
                connection.close();
            }
        }
    }
    /**
     * operation for sharindg
     */
    public static void  changeAllbak(String datasource) throws SQLException {
        String sql = "INSERT INTO sbtest (k,c,pad) VALUES (?,?,?)";
        Long id =  Long.MIN_VALUE;
        try (Connection connection = getDataSource(datasource).getConnection();
        PreparedStatement preparedStatement = connection.prepareStatement(sql,Statement.RETURN_GENERATED_KEYS)) {
            preparedStatement.setInt(1, 3);
            preparedStatement.setString(2, "##-####");
            preparedStatement.setString(3, "##-####");
            preparedStatement.executeUpdate();
            ResultSet result = preparedStatement.getGeneratedKeys();
            result.next();
            id = result.getLong(1);
        }catch (final SQLException ex) {
            ex.printStackTrace();
        }
        sql = "update sbtest set c=?,pad =? where id=? and k=?";
        try (Connection connection = getDataSource(datasource).getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            preparedStatement.setString(1,"new");
            preparedStatement.setString(2,"new");
            preparedStatement.setLong(3,id);
            preparedStatement.setInt(4,3);
            preparedStatement.executeUpdate();
        }
        sql = "delete from sbtest where k=? and id=?";
        try (Connection connection = getDataSource(datasource).getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            preparedStatement.setInt(1,3);
            preparedStatement.setLong(2,id);
            preparedStatement.executeUpdate();
        }
    }

    /**
     * for update
     * @param sql
     * @param datasource
     * @return
     * @throws SQLException
     */
    public static int updateStmt(final String sql, String datasource) throws SQLException{
        try (Connection connection = getDataSource(datasource).getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            preparedStatement.setString(1,"##-#####");
            preparedStatement.setString(2,"##-#####");
            preparedStatement.setInt(3,1);
            preparedStatement.setInt(4,1);
            return preparedStatement.executeUpdate();
        }
    }

    /**
     * for insert performance
     * @param sql
     * @param datasource
     * @throws SQLException
     */
    public static void insertIou(final String sql, String datasource) throws SQLException {
        Connection connection = null;
        try {
            connection = getDataSource(datasource).getConnection();
            PreparedStatement preparedStatement = connection.prepareStatement(sql);
            preparedStatement.setInt(1, 3);
            preparedStatement.setString(2, "##-####");
            preparedStatement.setString(3, "##-####");
            preparedStatement.execute();
        } catch (Exception ex) {
            ex.printStackTrace();
        } finally {
            if (connection != null) {
                connection.close();
            }
        }
    }

    /**
     * clean up environment
     * @param sql
     * @param datasource
     * @throws SQLException
     */
    public static void clean(final String sql, String datasource) throws SQLException {
        Connection connection = null;
        try {
            connection = getDataSource(datasource).getConnection();
            PreparedStatement preparedStatement = connection.prepareStatement(sql);
            preparedStatement.executeUpdate();
        } catch (Exception e){
            e.printStackTrace();
        } finally {
            if(connection != null) {
                connection.close();
            }
        }
    }

    /**
     * for delete performance
     * @param sql
     * @param datasource
     * @return
     * @throws SQLException
     */
    public static int deleteIou(final String sql, String datasource) throws SQLException {
        try (Connection connection = getDataSource(datasource).getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            preparedStatement.setInt(1,1);
            preparedStatement.setInt(2,1);
            return preparedStatement.executeUpdate();
        }
    }


}
