package org.apache.shardingsphere.benchmark.common.file.xml;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Benchmark config jmx.
 */
public final class BenchmarkConfigJmx {
    
    public static FileFilter jmxFilter = new JmxFileFilter();
    
    /**
     * Modify benchmark output directory for jmx with configured root path in the user-config.properties. 
     * @param benchmarkBasePath
     * @param benchmarkOutputBasePath
     * @param jmeterConcurrencyCount
     * @param jmeterLoopCount
     */
    public static void modifyBenchmarkOutputBasePath(String benchmarkBasePath, String benchmarkOutputBasePath, int jmeterConcurrencyCount, int jmeterLoopCount){
        File jmxFile = null;
        List<File> jmxFileList = getJmxFileList(benchmarkBasePath);
        for (int i = 0; i < jmxFileList.size(); i++){
            jmxFile = jmxFileList.get(i);
            Document xmlDocument = getOutputElement(jmxFile, benchmarkOutputBasePath, jmeterConcurrencyCount, jmeterLoopCount);
            saveJmxFile(jmxFile, xmlDocument);
        }
    }
    
    /**
     * Filter target files with its suffix.
     * @param targetDir
     * @param filter
     * @param resultFiles
     * @return
     */
    public static List<File> filterTargetFiles(File targetDir, FileFilter filter, List<File> resultFiles) {
        File[] files = targetDir.listFiles(filter);
        for (File file : files) {
            if (file.isDirectory()) {
                filterTargetFiles(file, filter, resultFiles);
            } else {
                resultFiles.add(file);
            }
        }
        return resultFiles;
    }
    
    /**
     * Get all of jmx files.
     * @param benchmarkBasePath
     * @return
     */
    public static List<File> getJmxFileList(String benchmarkBasePath){
        
        List<File> jmxFileList = new ArrayList<File>(10);
        String jmxBasePath = benchmarkBasePath + "/src/main/resources/testplan";
        File jmxBaseDir = new File(jmxBasePath);
        return filterTargetFiles(jmxBaseDir, jmxFilter, jmxFileList);
    }
    
    /**
     * Save jmx file.
     * @param testPlanFile
     * @param xmlDocument
     */
    public static void saveJmxFile(File testPlanFile, Document xmlDocument){
        TransformerFactory transFactory = TransformerFactory.newInstance();
        Transformer transformer = null;
        try {
            transformer = transFactory.newTransformer();
            transformer.setOutputProperty("indent", "yes");
            DOMSource source = new DOMSource();
            source.setNode(xmlDocument);
            StreamResult result = new StreamResult();
            result.setOutputStream(new FileOutputStream(testPlanFile));
            transformer.transform(source, result);
        } catch (TransformerConfigurationException ex) {
            ex.printStackTrace();
        } catch (FileNotFoundException ex) {
            ex.printStackTrace();
        } catch (TransformerException ex) {
            ex.printStackTrace();
        } finally {}
    
    }
    
    /**
     * Get xml element of jmx.
     * @param testPlanFile
     * @param outPutBasePath
     * @param jmeterConcurrencyCount
     * @param jmeterLoopCount
     * @return
     */
    public static Document getOutputElement(File testPlanFile, String outPutBasePath, int jmeterConcurrencyCount, int jmeterLoopCount){
        Element root = null;
        Document result = null;
        DocumentBuilder documentBuilder = null;
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setIgnoringElementContentWhitespace(true);
        try {
            documentBuilder = factory.newDocumentBuilder();
            result = (Document) documentBuilder.parse(testPlanFile);
            root = result.getDocumentElement();
            XPathFactory xpathFactory = XPathFactory.newInstance();
            XPath xpath = xpathFactory.newXPath();
            String resultCollectorXpath = "//jmeterTestPlan/hashTree/hashTree/hashTree/hashTree/ResultCollector";
            NodeList nodeList = (NodeList) xpath.evaluate(resultCollectorXpath, root, XPathConstants.NODESET);
            Element resultCollectorElement = (Element) nodeList.item(1);
            String stringPropXpath = "./stringProp";
            Element stringPropElement = (Element) xpath.evaluate(stringPropXpath, resultCollectorElement, XPathConstants.NODE);
            String outputPath = stringPropElement.getTextContent();
            outputPath = outputPath.replace("/export/shardingsphere-benchmark/result", outPutBasePath);
            stringPropElement.setTextContent(outputPath);
            if (testPlanFile.getName().contains("insertupdatedelete") || testPlanFile.getName().contains("select")){
                String jmeterLoopXpath = "//jmeterTestPlan/hashTree/hashTree/ThreadGroup/elementProp/stringProp";
                String jmeterConcurrencyXpath = "//jmeterTestPlan/hashTree/hashTree/ThreadGroup/stringProp[@name='ThreadGroup.num_threads']";
                Element jmeterLoopElement = (Element) xpath.evaluate(jmeterLoopXpath, root, XPathConstants.NODE);
                Element jmeterConcurrencyElement = (Element) xpath.evaluate(jmeterConcurrencyXpath, root, XPathConstants.NODE);
                jmeterLoopElement.setTextContent("" + jmeterLoopCount);
                jmeterConcurrencyElement.setTextContent("" + jmeterConcurrencyCount);
            }
        } catch (ParserConfigurationException ex) {
            ex.printStackTrace();
        } catch (IOException ex) {
            ex.printStackTrace();
        } catch (XPathExpressionException ex) {
            ex.printStackTrace();
        } catch (SAXException ex) {
            ex.printStackTrace();
        } finally {
        }
        return result;
    }
    
    
    /**
     * Jmx file filter.
     */
    private final static class JmxFileFilter implements FileFilter {
        
        @Override
        public boolean accept(File file) {
            if (file.isDirectory()) {
                return true;
            }
            String fileName = file.getName();
            return fileName.matches("(?i).+jmx$");
        }
        
    }
    
    public static void main(String[] args) {
        
        String benchmarkBasePath = "D:/shardingsphere-benchmark";
        String outputBasePath = "/export/shardingsphere-benchmark/result";
        modifyBenchmarkOutputBasePath(benchmarkBasePath, outputBasePath, 10,1000);
    }
}
