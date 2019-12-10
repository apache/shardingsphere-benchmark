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

package shardingjdbc.perf.msencryptsharding;

import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import perfstmt.ShardingPerfStmt;
import service.util.config.SJDataSourceFactory;
import service.util.config.SJDataSourceUtil;

import javax.sql.DataSource;
import java.sql.SQLException;

/**
 * Sharding-Jdbc master slave & encrypt & sharding delete performance with ss dev branch.
 * @author nancyzrh
 */
public class SJMsEncryptShardingDelete extends AbstractJavaSamplerClient {
    private static final String DELETE_ENC_STMT = ShardingPerfStmt.DELETE_STMT.getValue();
    
    private static DataSource dataSource;
    
    static {
        try {
            dataSource = SJDataSourceFactory.createMSEncShardingDataSource();
        } catch (final SQLException ignore) {
        }
    }
    
    /**
     * run test.
     * @param javaSamplerContext context
     * @return res
     */
    @Override
    public SampleResult runTest(JavaSamplerContext javaSamplerContext) {
        
        SampleResult results = new SampleResult();
        results.setSampleLabel("SJEncryptDelete");
        results.sampleStart();
        try {
            SJDataSourceUtil.delete(DELETE_ENC_STMT, dataSource);
        } catch (SQLException ex) {
            results.setSuccessful(false);
            return results;
        } finally {
            results.sampleEnd();
        }
        results.setSuccessful(true);
        return results;
    }
}

