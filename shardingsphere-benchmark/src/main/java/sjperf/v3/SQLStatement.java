package sjperf.v3;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
@Getter
public enum SQLStatement {
    //DataBase Routing SQL
    DELETE_SQL_DB_ROUTING("delete from t_order where user_id=5")
    ,INSERT_SQL_DB_ROUTING("insert into t_order(user_id) values(7)")
    ,SELECT_SQL_DB_ROUTING("select * from t_order where user_id=5")
    ,UPDATE_SQL_DB_ROUTING("update t_order set name='jdbc',sex='sun' where user_id=5")
    
    //Full Routing SQL
    ,DELETE_SQL_FULL_ROUTING("delete from t_order")
    ,INSERT_SQL_FULL_ROUTING("insert into t_order(name,sex) values('beijing','tongzhou')")
    ,SELECT_SQL_FULL_ROUTING("select * from t_order")
    ,UPDATE_SQL_FULL_ROUTING("update t_order set name='hunter',sex='male'")
    
    //Single Routing SQL
    ,DELETE_SQL_SINGLE_ROUTING("delete from t_order where user_id=7 and order_id=11")
    ,INSERT_SQL_SINGLE_ROUTING("insert into t_order(user_id,order_id,name,sex) values(9,13,'Tom','male')")
    ,SELECT_SQL_SINGLE_ROUTING("select * from t_order where user_id=3 and order_id=5")
    ,UPDATE_SQL_SINGLE_ROUTING("update t_order set name='Crazy Man',sex='male' where user_id=9 and order_id=13")
    
    ,DELETE_SQL_TABLE_ROUTING("delete from t_order where user_id=7 and order_id=11")
    ,INSERT_SQL_TABLE_ROUTING("insert into t_order(user_id,order_id,name,sex) values(9,13,'Tom','male')")
    ,SELECT_SQL_TABLE_ROUTING("select * from t_order where user_id=3 and order_id=5")
    ,UPDATE_SQL_TABLE_ROUTING("update t_order set name='Crazy Man',sex='male' where user_id=9 and order_id=13")
    //Table Sharding SQL
    ,DELETE_SQL_SHRADING("delete from sbtest where k=? and id=?")
    ,INSERT_SQL_SHARDING("INSERT INTO sbtest (k,c,pad) VALUES (?,?,?)")
    ,INSERT_SQL_SHARDING_DEMO("INSERT INTO sbtest (id,k,c,pad) VALUES (?,?,?,?)")
    ,SELECT_SQL_SHARDING("SELECT id,k from sbtest where id=1 and k=1")
    ,UPDATE_SQL_SHARDING("update sbtest set c=?,pad=? where id=? and k=?")
    ,CLEAN_ALL("delete from sbtest")
    
    //Master Slave SQL
    ,DELETE_SQL_MASTER_SLAVE("delete from sbtest99 where k=? and id=?")
    ,INSERT_SQL_MASTER_SLAVE("INSERT INTO sbtest99 (k,c,pad) VALUES (?,?,?)")
    ,INSERT_SQL_DEMO("INSERT INTO sbtest99 (id,k,c,pad) VALUES (?,?,?,?)")
    ,SELECT_SQL_MASTER_SLAVE("SELECT id,k from sbtest99 where id=1 and k=1")
    ,CLEAN("delete from sbtest99")
    ,UPDATE_SQL_MASTER_SLAVE("update sbtest99 set c=?,pad=? where id=? and k=?");
    private final String value;
    public static void main(String[] args) {
        System.out.println(SQLStatement.INSERT_SQL_MASTER_SLAVE.getValue());
    }
}

