package org.apache.shardingsphere.benchmark.common.file.util;

import org.apache.commons.lang.StringUtils;

import java.awt.*;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;

public class ProcessUtil {
    
    public static void killProcess(String processName){
        if (System.getProperty("os.name").toLowerCase().contains("linux")){
            String pid = getLinuxProcess(processName);
            killLinuxProcess(pid);
        } else {
            killWindowsProcess(processName);
        }
    }
    
    public static void startUpProcess(String file){
        if (System.getProperty("os.name").toLowerCase().contains("linux")){
            startLinuxProcess(file);
        } else {
            startWindowsProcess(file);
        }
    }
    
    
    public static boolean startLinuxProcess(String shScriptPath){
    
        String line = "";
        Process process = null;
        BufferedReader input = null;
        try {
            String[] cmd = {"/bin/sh", "-c", shScriptPath};
            process = Runtime.getRuntime().exec(cmd);
            input = new BufferedReader(new InputStreamReader(process.getInputStream()));
            StringBuffer sb = new StringBuffer("");
            while ((line = input.readLine()) != null) {
                sb.append(line).append("\r\n");
            }
            if(process != null){
                int extValue = process.waitFor(); 
                if(0 == extValue){
                    return true;
                }
            }
            return false;
        } catch (Exception e) {
            return false;
        } finally {
            try {
                input.close();
                process.destroy();
            } catch (Exception e) {}
        }
    }
    
    
    public static String getLinuxProcess(String processName) {
        
        BufferedReader reader = null;
        try {
            Process process = Runtime.getRuntime().exec("ps -ef | grep " + processName);
            reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
            String line = null;
            while ((line = reader.readLine()) != null) {
                if (line.contains(processName)) {
                    String[] strs = line.split("\\s+");
                    return strs[1];
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) { }
            }
        }
        return null;
    }
    
    
    public static void killLinuxProcess(String Pid) {
        
        Process process = null;
        BufferedReader reader = null;
        try {
            process = Runtime.getRuntime().exec("kill -9 " + Pid);
            reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
            String line = null;
            while ((line = reader.readLine()) != null) {
                System.out.println("kill PID return info -----> " + line);
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if (process != null) {
                process.destroy();
            }
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) { }
            }
        }
    }
    
    public static void startWindowsProcess(String processName) {
        if (StringUtils.isNotBlank(processName)) {
            try {
                Desktop.getDesktop().open(new File(processName));
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public static void killWindowsProcess(String processName) {
        if (StringUtils.isNotBlank(processName)) {
            try {
                executeWindowsCmd("taskkill /F /IM " + processName);
            } catch (IOException exception) {
                exception.printStackTrace();
            }
        }
    }
    
    public static String executeWindowsCmd(String command) throws IOException {
        Runtime runtime = Runtime.getRuntime();
        Process process = runtime.exec("cmd /c " + command);
        BufferedReader br = new BufferedReader(new InputStreamReader(process.getInputStream(), "UTF-8"));
        String line = null;
        StringBuilder build = new StringBuilder();
        while ((line = br.readLine()) != null) {
            build.append(line);
        }
        return build.toString();
    }


    
    public static boolean getWindowsProcess(String processName) {
        BufferedReader bufferedReader = null;
        try {
            Process proc = Runtime.getRuntime().exec("tasklist -fi " + '"' + "imagename eq " + processName + '"');
            bufferedReader = new BufferedReader(new InputStreamReader(proc.getInputStream()));
            String line = null;
            while ((line = bufferedReader.readLine()) != null) {
                if (line.contains(processName)) {
                    return true;
                }
            }
            return false;
        } catch (Exception ex) {
            ex.printStackTrace();
            return false;
        } finally {
            if (bufferedReader != null) {
                try {
                    bufferedReader.close();
                } catch (Exception ex) {
                }
            }
        }
    }
}
