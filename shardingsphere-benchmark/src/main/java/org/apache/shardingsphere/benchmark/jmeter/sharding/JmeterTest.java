package org.apache.shardingsphere.benchmark.jmeter.sharding;

import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class JmeterTest extends AbstractJavaSamplerClient {

    private static final Logger log = LoggerFactory.getLogger(JmeterTest.class);

    static {
        System.out.println("static .....");
    }

    @Override
    public SampleResult runTest(JavaSamplerContext javaSamplerContext) {
        log.debug(this.getClass().getName() + ": runTest");
        System.out.println(this.getClass().getName() + ": runTest");
        return null;
    }

    @Override
    public void setupTest(JavaSamplerContext context) {

        log.debug(this.getClass().getName() + ": setupTest");
        System.out.println(this.getClass().getName() + ": setupTest");
    }

    @Override
    public void teardownTest(JavaSamplerContext context) {
        log.debug(this.getClass().getName() + ": teardownTest");
        System.out.println(this.getClass().getName() + ": teardownTest");
        super.teardownTest(context);


    }
}
