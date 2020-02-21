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
package jdbc.perf;

import org.apache.jmeter.config.Arguments;
import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import service.util.config.DataSourceUtil;

import java.sql.SQLException;

/**
 * for mysql insert+update+delete
 */
public class JdbcWrite extends AbstractJavaSamplerClient {
    static {
        DataSourceUtil.createDataSource("###", "sharding_db", "###", 3306, "###");
    }
    
    @Override
    public Arguments getDefaultParameters() {
        return null;
    }
    
    @Override
    public void setupTest(JavaSamplerContext context) {
    }
    
    @Override
    public SampleResult runTest(JavaSamplerContext context) {
        SampleResult results = new SampleResult();
        results.setSampleLabel("JdbcWrite");
        results.sampleStart();
        try {
            DataSourceUtil.writeOp( "sharding_db");
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

