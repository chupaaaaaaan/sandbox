package org.example;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.StringJoiner;

/**
 * テーブルの各カラム名と型名を出力する、JDBCの使い方サンプル。
 */
public class App {
    public static void main( String[] args ) {
        String tableName = "example_table";

        try(Connection conn = DriverManager.getConnection("jdbc:postgresql://localhost:15432/postgres", "postgres", "postgres");
            ResultSet rs = conn.getMetaData().getColumns(null, "public", tableName, "%")) {

            StringJoiner sj = new StringJoiner(",", "[", "]");
            while (rs.next()) {
                sj.add("{\"column\":\"" + rs.getString("COLUMN_NAME") + "\",\"type\":\"" + rs.getString("TYPE_NAME") + "\"}");
            }

            if (sj.toString().isEmpty()) {
                throw new SQLException(tableName + "を見つけられません。");
            }

            System.out.println(sj);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

}