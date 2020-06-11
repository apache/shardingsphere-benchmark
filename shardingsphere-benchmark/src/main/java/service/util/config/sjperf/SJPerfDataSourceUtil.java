package service.util.config.sjperf;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import service.api.entity.Iou;
import sjperf.v3.SQLStatement;

import javax.sql.DataSource;
import java.sql.*;
import java.util.LinkedList;
import java.util.List;

public class SJPerfDataSourceUtil {
    private static final String USER_NAME = "root";
    private static final String DEFAULT_SCHEMA = "test";

    public static DataSource createDataSource(final String dataSourceName, final String host, final int port, final String password) {
        HikariConfig config = new HikariConfig();
        config.setDriverClassName("com.mysql.jdbc.Driver");
        config.setJdbcUrl(String.format("jdbc:mysql://%s:%s/%s?useSSL=false&serverTimezone=UTC", host, port, dataSourceName));
        config.setUsername(USER_NAME);
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

        return dataSource;
    }

    public void createSchema(final DataSource dataSource) {
        //TODO
    }
    public static void  singleMsAll(final DataSource datasource) throws SQLException {
        String sql = "INSERT INTO sbtest3 (k,c,pad) VALUES (?,?,?)";
        Long id =  Long.MIN_VALUE;
        Connection connection=null;
        PreparedStatement preparedStatement=null;
        ResultSet result= null;
        try {
            connection = datasource.getConnection();
            preparedStatement = connection.prepareStatement(sql,Statement.RETURN_GENERATED_KEYS);
            preparedStatement.setInt(1, 3);
            preparedStatement.setString(2, "##-####");
            preparedStatement.setString(3, "##-####");
            preparedStatement.executeUpdate();
            result = preparedStatement.getGeneratedKeys();
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
            //sql = "select id,k from sbtest3 ignore index(`PRIMARY`) where id=2 and k=3";
            //preparedStatement = connection.prepareStatement(sql);
            //preparedStatement.setLong(1,id);
            //preparedStatement.executeQuery();
            //sql = "update sbtest3 set c=?,pad =? where id=? and k=3";
            //preparedStatement = connection.prepareStatement(sql);
            //preparedStatement.setString(1,"new");
            //preparedStatement.setString(2,"new");
            //preparedStatement.setLong(3,id);
            //preparedStatement.setInt(4,3);
            //preparedStatement.executeUpdate();
            sql = "delete from sbtest3 where k=? and id=?";
            preparedStatement = connection.prepareStatement(sql);
            preparedStatement.setInt(1,3);
            preparedStatement.setLong(2,id);
            preparedStatement.executeUpdate();
            //sql = "select max(k) from sbtest3";
            //sql = "select id,k from sbtest3 ignore index(`PRIMARY`) where id=2 and k=3";
            //preparedStatement = connection.prepareStatement(sql);
            //preparedStatement.setLong(1,id);
            //preparedStatement.executeQuery();
            preparedStatement.close();
            result.close();
        }catch (final SQLException ex) {
            ex.printStackTrace();
        } finally {
            connection.close();
        }
    }

    public static void  singleAll(final DataSource datasource) throws SQLException {
        String sql = "INSERT INTO sbtest3 (k,c,pad) VALUES (?,?,?)";
        Long id =  Long.MIN_VALUE;
        Connection connection=null;
        PreparedStatement preparedStatement=null;
        ResultSet result= null;
        try {
            connection = datasource.getConnection();
            preparedStatement = connection.prepareStatement(sql,Statement.RETURN_GENERATED_KEYS);
            preparedStatement.setInt(1, 3);
            preparedStatement.setString(2, "##-####");
            preparedStatement.setString(3, "##-####");
            preparedStatement.executeUpdate();
            result = preparedStatement.getGeneratedKeys();
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
            // preparedStatement.close();
            // resultSet.close();
            // connection=null;
        }
    }
    public static void  singleAllnewbak(final DataSource datasource) throws SQLException {
        String sql = "INSERT INTO sbtest3 (k,c,pad) VALUES (?,?,?)";
        Long id = Long.MIN_VALUE;
        try (Connection connection = datasource.getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS)) {
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
        sql = "update sbtest3 set c=?,pad =? where id=? and k=?";
        try (Connection connection = datasource.getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            preparedStatement.setString(1,"##-#####");
            preparedStatement.setString(2,"##-#####");
            preparedStatement.setLong(3,id);
            preparedStatement.setInt(4,1);
            preparedStatement.executeUpdate();
        }
        // sql = "TRUNCATE TABLE sbtest3";
        sql = "delete from sbtest3 where k=? and id=?";
        try (Connection connection = datasource.getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            preparedStatement.setInt(1,1);
            preparedStatement.setLong(2,id);
            preparedStatement.executeUpdate();
        }
    }

    /**
     * operation
     */
    public static void  singleAllbak(final DataSource datasource) throws SQLException {
        String sql = "INSERT INTO sbtest3 (k,c,pad) VALUES (?,?,?)";
        int id = 0;
        try (Connection connection = datasource.getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS)) {
            preparedStatement.setInt(1, 1);
            preparedStatement.setString(2, "##-####");
            preparedStatement.setString(3, "##-####");
            preparedStatement.executeUpdate();
            ResultSet result = preparedStatement.getGeneratedKeys();
            result.next();
            id = result.getInt(1);
        }catch (final SQLException ex) {
            ex.printStackTrace();
        }
        //sql = "update sbtest3 set c=?,pad =? where id=? and k=?";
        sql = "select id,k from sbtest3 ignore index(`PRIMARY`) where id=1 and k=3";
        try (Connection connection = datasource.getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            //    preparedStatement.setString(1,"##-#####");
            //    preparedStatement.setString(2,"##-#####");
            //   preparedStatement.setInt(3,id);
            //   preparedStatement.setInt(4,1);
            //  preparedStatement.executeUpdate();
            preparedStatement.executeQuery();
        }

        //sql = "select max(k) from sbtest3";
        sql = "select id,k from sbtest3 ignore index(`PRIMARY`) where id=2 and k=3";
        try (Connection connection = datasource.getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            //    preparedStatement.setString(1,"##-#####");
            //    preparedStatement.setString(2,"##-#####");
            //   preparedStatement.setInt(3,id);
            //   preparedStatement.setInt(4,1);
            //  preparedStatement.executeUpdate();
            preparedStatement.executeQuery();
        }
        sql = "delete from sbtest3 where k=? and id=?";
        try (Connection connection = datasource.getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            preparedStatement.setInt(1,1);
            preparedStatement.setInt(2,id);
            preparedStatement.executeUpdate();
        }

        sql = "select max(k) from sbtest3";
        try (Connection connection = datasource.getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            //    preparedStatement.setString(1,"##-#####");
            //    preparedStatement.setString(2,"##-#####");
            //   preparedStatement.setInt(3,id);
            //   preparedStatement.setInt(4,1);
            //  preparedStatement.executeUpdate();
            preparedStatement.executeQuery();
        }
    }
    public static void  changeMsAll(final DataSource datasource) throws SQLException {
        String sql = "INSERT INTO sbtest (k,c,pad) VALUES (?,?,?)";
        Long id =  Long.MIN_VALUE;
        Connection connection=null;
        PreparedStatement preparedStatement=null;
        ResultSet resultSet= null;
        try {
            connection = datasource.getConnection();
            preparedStatement = connection.prepareStatement(sql,Statement.RETURN_GENERATED_KEYS);
            preparedStatement.setInt(1, 3);
            preparedStatement.setString(2, "##-####");
            preparedStatement.setString(3, "##-####");
            preparedStatement.executeUpdate();
            ResultSet result = preparedStatement.getGeneratedKeys();
            result.next();
            id = result.getLong(1);
            sql = "select id,k from sbtest ignore index(`PRIMARY`) where id=? and k=3";
            preparedStatement = connection.prepareStatement(sql);
            preparedStatement.setLong(1,id);
            preparedStatement.executeQuery();
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
            preparedStatement.close();
            result.close();
        }catch (final SQLException ex) {
            ex.printStackTrace();
        } finally {
            connection.close();
        }
    }

    public static void  changeAll(final DataSource datasource) throws SQLException {
        String sql = "INSERT INTO sbtest (k,c,pad) VALUES (?,?,?)";
        Long id =  Long.MIN_VALUE;
        Connection connection=null;
        PreparedStatement preparedStatement=null;
        ResultSet resultSet= null;
        try {
            connection = datasource.getConnection();
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
            preparedStatement.close();
            result.close();
        }catch (final SQLException ex) {
            ex.printStackTrace();
        } finally {
            connection.close();
            // preparedStatement.close();
            // resultSet.close();
            // connection=null;
        }
    }
    /**
     * operation for sharindg
     */
    public static void  changeAllbak(final DataSource datasource) throws SQLException {
        String sql = "INSERT INTO sbtest (k,c,pad) VALUES (?,?,?)";
        long id = Long.MIN_VALUE;
        //int id =0;
        try (Connection connection = datasource.getConnection();
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
        try (Connection connection = datasource.getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            preparedStatement.setString(1,"##-#####");
            preparedStatement.setString(2,"##-#####");
            preparedStatement.setLong(3,id);
            preparedStatement.setInt(4,3);
            preparedStatement.executeUpdate();
        }
        // sql = "TRUNCATE TABLE sbtest";
        sql="delete from sbtest where k=? and id=?";
        try (Connection connection = datasource.getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            preparedStatement.setInt(1,3);
            preparedStatement.setLong(2,id);
            preparedStatement.executeUpdate();
        }
    }

    public static void insertDemo(final String sql, final DataSource datasource) throws SQLException {
        try (Connection connection = datasource.getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            preparedStatement.setInt(1, 1);
            preparedStatement.setInt(2, 1);
            preparedStatement.setString(3, "##-####");
            preparedStatement.setString(4, "##-####");
            preparedStatement.execute();
        } catch (final SQLException ignored) {
        }
    }

    public static void main(String args[]) {
        String SELECT_SQL_MASTER_SLAVE =  SQLStatement.INSERT_SQL_DEMO.getValue();//SQLStatement.SELECT_SQL_MASTER_SLAVE.getValue();
        try {
            DataSource dataSource = SJPerfDataSourceOp.CreateShardingDataSource();//SJPerfDataSourceOp.CreateMSDataSource();
            SJPerfDataSourceUtil.changeAll(dataSource);
        } catch (final SQLException ex) {

        }

    }
    public static void getSelect(final String sql, final DataSource dataSource) throws SQLException{
        List<Iou> result = new LinkedList<>();
        try (Connection connection = dataSource.getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)){
            ResultSet resultSet = preparedStatement.executeQuery();
            while (resultSet.next()) {
                System.out.println("id: " + resultSet.getInt(1) + " , " + "k: " + resultSet.getInt(2));
                //   Iou iou = new Iou();
                //     Long id = resultSet.getLong("id");
                //TODO add result iou.setK(resultSet.getInt(2));
                // result.add(iou);
            }
            connection.close();
        }
    }

    public static int updateStmt(final String sql, final DataSource datasource) throws SQLException {
        try (Connection connection = datasource.getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            preparedStatement.setString(1,"##-#####");
            preparedStatement.setString(2,"##-#####");
            preparedStatement.setInt(3,1);
            preparedStatement.setInt(4,1);
            return preparedStatement.executeUpdate();
        }
    }

    public static int delete(final String sql, final DataSource datasource) throws SQLException {
        try (Connection connection = datasource.getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            //TODO ADD preparedStatement.setInt(1,1);
            preparedStatement.setInt(1,1);
            preparedStatement.setInt(2,1);
            return preparedStatement.executeUpdate();
        }
    }

    public static void clean(final String sql, final DataSource datasource) throws SQLException {
        try (Connection connection = datasource.getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            preparedStatement.executeUpdate();
        }
    }

    public static void insert(final String sql, final DataSource datasource) throws SQLException {
        try (Connection connection = datasource.getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            //TODO ADD preparedStatement.setInt(1, 1);

            preparedStatement.setInt(1, 1);
            preparedStatement.setString(2, "##-####");
            preparedStatement.setString(3, "##-####");
            preparedStatement.execute();
        } catch (final SQLException ex) {
            ex.printStackTrace();
        }
    }

}
