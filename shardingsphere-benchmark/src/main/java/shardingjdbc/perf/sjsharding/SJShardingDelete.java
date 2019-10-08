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

package shardingjdbc.perf.sjsharding;

import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import perfstmt.ShardingPerfStmt;
import service.api.service.SJPerfService;
import service.util.config.SJDataSourceOp;
import service.util.config.SJDataSourceUtil;

import javax.sql.DataSource;
import java.sql.SQLException;

/**
 * for shardingjdbc delete performance with ss dev branch.
 * @author nancyzrh
 */
public class SJShardingDelete extends AbstractJavaSamplerClient {
    private static final String DELETE_SHARDING = ShardingPerfStmt.DELETE_STMT.getValue();
    
    private static SJPerfService sjPerfService;
    
    private static DataSource dataSource;
    
    static {
        try {
            sjPerfService = new SJPerfService(SJDataSourceOp.createEncryptDataSource());
        } catch (final SQLException ignore) {
        }
    }
    
    /**
     * prepare setup.
     * @param context context
     */
    @Override
    public void setupTest(JavaSamplerContext context) {
        dataSource = sjPerfService.getDataSource();
    }
    
    /**
     * run test.
     * @param javaSamplerContext context
     * @return res
     */
    @Override
    public SampleResult runTest(JavaSamplerContext javaSamplerContext) {
        
        SampleResult results = new SampleResult();
        results.setSampleLabel("SJShardingDelete");
        results.sampleStart();
        try {
            SJDataSourceUtil.delete(DELETE_SHARDING, dataSource);
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


