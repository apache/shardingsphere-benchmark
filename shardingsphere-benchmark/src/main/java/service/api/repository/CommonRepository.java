package service.api.repository;

import java.sql.SQLException;
import java.util.List;

public interface CommonRepository<T> {

    void createTableIfNotExists();

    void dropTable();

    void truncateTable();

    Long insert(T entity);

    void delete(Long id);

    List<T> selectAll(String sql) throws SQLException;

    List<T> selectRange() throws SQLException;
}
