package test;

import javax.sql.DataSource;
import javax.xml.crypto.Data;
import java.io.File;
import java.io.IOException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.shardingsphere.benchmark.db.jdbc.JDBCDataSourceUtil;
import org.apache.shardingsphere.benchmark.db.shardingjdbc.ShardingConfigType;
import org.apache.shardingsphere.benchmark.db.shardingjdbc.ShardingJDBCDataSourceFactory;
import org.apache.shardingsphere.driver.api.yaml.YamlShardingSphereDataSourceFactory;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

public class DatasourceTest {
    private static final String ALL_DATABASE_ALL_TABLE_SHARDING_CONFIG_PATH = "/yaml/shardingjdbc/all-db-all-table-sharding-config.yaml";

    public static void main(String[] args) throws IOException, SQLException {

        //DataSource  ds = ShardingJDBCDataSourceFactory.newInstance(ShardingConfigType.ALL_DATABASE_ALL_TABLE_SHARDING_CONFIG);
        String proxyAllDbAllTableYaml = "/yaml/proxy/config-sharding_alldb_alltable.yaml";

        byte[] contents = ShardingJDBCDataSourceFactory.getFileContents(proxyAllDbAllTableYaml);
        DataSource  ds = YamlShardingSphereDataSourceFactory.createDataSource(contents);
        Connection connection = ds.getConnection();
        String deletesql = " truncate table sbtest";

        PreparedStatement ps2 = connection.prepareStatement(deletesql);
        ps2.executeUpdate();

        List insertParams = Arrays.asList(3, "##-####", "##-####");
        Connection conn = ds.getConnection();

        String insert_sql = "insert into sbtest (k,c,pad) VALUES (?,?,?)";

        for(int i = 0; i < 10; i++){
            ResultSet rs1 = JDBCDataSourceUtil.insert(conn, insert_sql, insertParams);
            rs1.next();
            Long id = rs1.getLong(1);
        }

        ResultSet rs1 = JDBCDataSourceUtil.insert(conn, insert_sql, insertParams);
        rs1.next();
        Long id = rs1.getLong(1);
        System.out.println(id);


        String sql = "select * from sbtest where id=107 and k=3";
        PreparedStatement ps = connection.prepareStatement(sql);
        ResultSet rs = ps.executeQuery();


        while(rs.next()){
            System.out.println("id: " + rs.getInt(1) + " , " + "k: " + rs.getInt(2) + " , " + "c: " + rs.getString(3));
        }

        connection.close();

    }

    private static File getFile(final String fileName) {
        return new File(DatasourceTest.class.getResource(fileName).getFile());
    }
}
