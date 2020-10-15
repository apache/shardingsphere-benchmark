package org.apache.shardingsphere.benchmark.common.file.excel;

import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.FillPatternType;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.shardingsphere.benchmark.bean.BenchmarkResultBean;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * Benchmark excel writer to generate report.
 */
public final class BenchmarkExcelWriter {
    
    private static List<String> CELL_HEADS;
    static{
        CELL_HEADS = new ArrayList<String>(10);
        CELL_HEADS.add("Version");
        CELL_HEADS.add("Scenario");
        CELL_HEADS.add("Rules");
        CELL_HEADS.add("Database Action");
        CELL_HEADS.add("Product");
        CELL_HEADS.add("TPS");
        CELL_HEADS.add("Concurrency Count");
        CELL_HEADS.add("Sample Amount");
        CELL_HEADS.add("Max Cost");
        CELL_HEADS.add("Min Cost");
        CELL_HEADS.add("SQL");
        CELL_HEADS.add("Sharding Database Count");
        CELL_HEADS.add("Sharding Table Count");
    }
    
    /**
     * Write jtl result to excel.
     * @param excelPath
     * @param sheetName
     * @param isHeader
     * @param rowNum
     * @param dataList
     */
    public static void writeExcel(String excelPath, String sheetName, boolean isHeader, int rowNum, List<BenchmarkResultBean> dataList){
        Workbook workbook = null;
        File exportFile = null;
        FileOutputStream fileOut = null;
        if (dataList.size() > 0) {
            try {
                exportFile = new File(excelPath);
                if(exportFile.exists()){
                    workbook = new HSSFWorkbook(new FileInputStream(excelPath));
                } else {
                    workbook = new HSSFWorkbook();
                    exportFile.createNewFile();
                }
                
                workbook = buildDataSheet(workbook, sheetName, isHeader, rowNum, dataList);
        
                fileOut = new FileOutputStream(excelPath);
                workbook.write(fileOut);
                fileOut.flush();
        
            } catch (FileNotFoundException ex) {
                ex.printStackTrace();
            } catch (IOException ex) {
                ex.printStackTrace();
            } finally {
                try {
                    if (fileOut != null) {
                        fileOut.close();
                    }
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }
    
    /**
     * Clear export result of benchmark from excel.
     * 
     * @param excelPath
     */
    public static void clearExportExcel(String excelPath){
        Workbook workbook = null;
        String sheetName = "";
        File exportFile = null;
        FileOutputStream fileOut = null;
        try {
            exportFile = new File(excelPath);
            if(exportFile.exists()){
                workbook = new HSSFWorkbook(new FileInputStream(excelPath));
                List<Long> timeStamps = new ArrayList<Long>();
                int sheetCount = workbook.getNumberOfSheets();
                if (sheetCount >= 9) {
                    for (int i = 0; i < sheetCount; i++) {
                        sheetName = workbook.getSheetName(i);
                        String[] nameSubs = sheetName.split("-");
                        long timeStamp = Long.valueOf(nameSubs[2]).longValue();
                        if (!timeStamps.contains(timeStamp)) {
                            timeStamps.add(timeStamp);
                        }
                    }
                    Collections.sort(timeStamps);
                    long minTimeStamp = timeStamps.get(0);
                    String removedSheetName = "full-routing-" + minTimeStamp;
                    int sheetIndex = workbook.getSheetIndex(removedSheetName);
                    workbook.removeSheetAt(sheetIndex);
                    removedSheetName = "single-routing-" + minTimeStamp;
                    sheetIndex = workbook.getSheetIndex(removedSheetName);
                    workbook.removeSheetAt(sheetIndex);
                    removedSheetName = "range-routing-" + minTimeStamp;
                    sheetIndex = workbook.getSheetIndex(removedSheetName);
                    workbook.removeSheetAt(sheetIndex);
                }
                fileOut = new FileOutputStream(excelPath);
                workbook.write(fileOut);
                workbook.close();
            }
            
            
        } catch (FileNotFoundException ex) {
            ex.printStackTrace();
        } catch (IOException ex) {
            ex.printStackTrace();
        } finally {
            try {
                if (fileOut != null) {
                    fileOut.close();
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
    
    /**
     * Build excel sheet.
     * @param result
     * @param sheetName
     * @param isHeader
     * @param rowNum
     * @param dataList
     * @return
     */
    private static Workbook buildDataSheet(Workbook result, String sheetName, boolean isHeader, int rowNum, List<BenchmarkResultBean> dataList) {
        
        Sheet sheet = null;
        if(null == result.getSheet(sheetName)){
            sheet = result.createSheet(sheetName);
        } else {
            sheet = result.getSheet(sheetName);
        }
        
        CellStyle cellStyle = buildHeadCellStyle(sheet.getWorkbook());
        if(isHeader){
            for (int i=0; i < CELL_HEADS.size(); i++) {
                sheet.setColumnWidth(i, 6000);
            }
            sheet.setDefaultRowHeight((short) 400);
            Row head = sheet.createRow(0);
            for (int i = 0; i < CELL_HEADS.size(); i++) {
                Cell cell = head.createCell(i);
                
                cell.setCellValue(CELL_HEADS.get(i));
                cell.setCellStyle(cellStyle);
            }
        }
        
        for (int i = 0; i < dataList.size(); i++) {
            BenchmarkResultBean benchmarkResultBean = dataList.get(i);
            Row row = sheet.createRow(rowNum++);
            convertDataToRow(benchmarkResultBean, row);
        }
        return result;
    }
    
    /**
     * Build cell style.
     * @param workbook
     * @return
     */
    private static CellStyle buildHeadCellStyle(Workbook workbook) {
        CellStyle result = workbook.createCellStyle();
        result.setAlignment(HorizontalAlignment.CENTER);
        result.setBorderBottom(BorderStyle.THIN);
        result.setBottomBorderColor(IndexedColors.BLACK.getIndex());
        result.setBorderLeft(BorderStyle.THIN);
        result.setLeftBorderColor(IndexedColors.BLACK.getIndex());
        result.setBorderRight(BorderStyle.THIN);
        result.setRightBorderColor(IndexedColors.BLACK.getIndex());
        result.setBorderTop(BorderStyle.THIN);
        result.setTopBorderColor(IndexedColors.BLACK.getIndex());
        result.setFillForegroundColor(IndexedColors.GREY_25_PERCENT.getIndex());
        result.setFillPattern(FillPatternType.SOLID_FOREGROUND);
        Font font = workbook.createFont();
        font.setBold(true);
        result.setFont(font);
        return result;
    }
    
    /**
     * Fill jtl data into each row of excel.
     * @param data
     * @param row
     */
    private static void convertDataToRow(BenchmarkResultBean data, Row row){
        int cellNum = 0;
        Cell cell;
        cell = row.createCell(cellNum++);
        cell.setCellValue(data.getVersion());
        cell = row.createCell(cellNum++);
        cell.setCellValue(data.getScenario());
        cell = row.createCell(cellNum++);
        cell.setCellValue(data.getRules());
        cell = row.createCell(cellNum++);
        cell.setCellValue(data.getDbAction());
        cell = row.createCell(cellNum++);
        cell.setCellValue(data.getProduct());
        Map benchmarkResult = data.getBenchmarkResult();
        cell = row.createCell(cellNum++);
        cell.setCellValue((double)benchmarkResult.get("tps"));
        cell = row.createCell(cellNum++);
        cell.setCellValue(data.getConcurrency());
        cell = row.createCell(cellNum++);
        cell.setCellValue((int)benchmarkResult.get("total"));
        cell = row.createCell(cellNum++);
        cell.setCellValue((double)benchmarkResult.get("maxCost"));
        cell = row.createCell(cellNum++);
        cell.setCellValue((double)benchmarkResult.get("minCost"));
        cell = row.createCell(cellNum++);
        cell.setCellValue(data.getSql());
        cell = row.createCell(cellNum++);
        cell.setCellValue(data.getDbShardingCount());
        cell = row.createCell(cellNum++);
        cell.setCellValue(data.getTableShardingCount());
    }
}
