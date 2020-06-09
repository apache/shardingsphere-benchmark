package service.api.entity;

import java.io.Serializable;

/**
 * operation object for jmeter performance
 */
public class Iou implements Serializable {
    private static final long serialVersionUID = 5377660875939152645L;

    private int k;
    private String c;
    private String pad;

    public int getK() {
        return k;
    }

    public void setK(int k) {
        this.k = k;
    }

    public String getC() {
        return c;
    }

    public void setC(String c) {
        this.c = c;
    }

    public String getPad() {
        return pad;
    }

    public void setPad(String pad) {
        this.pad = pad;
    }

    @Override
    public String toString() {
        return String.format("k: %s, c: %s, pad: %s", k, c, pad);
    }

}
