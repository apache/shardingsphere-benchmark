package org.apache.shardingsphere.benchmark.bean;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

/**
 * operation object for jmeter performance
 */
@Getter
@Setter
public class Iou implements Serializable {

    private static final long serialVersionUID = 5377660875939152645L;

    private int k;

    private String c;

    private String pad;

    @Override
    public String toString() {
        return String.format("k: %s, c: %s, pad: %s", k, c, pad);
    }

}
