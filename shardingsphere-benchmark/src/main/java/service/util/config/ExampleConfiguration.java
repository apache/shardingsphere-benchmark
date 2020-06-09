package service.util.config;

import javax.sql.DataSource;
import java.sql.SQLException;

public interface ExampleConfiguration {
    DataSource createDataSource() throws SQLException;

    DataSource getDataSource();
}
